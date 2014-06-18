/*
 * Copyright 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _GNU_SOURCE

#include "config.h"

#include "linted/spawn.h"

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

enum file_action_type {
    FILE_ACTION_ADDDUP2
};

struct adddup2
{
    enum file_action_type type;
    int oldfildes;
    int newfildes;
};

union file_action
{
    enum file_action_type type;
    struct adddup2 adddup2;
};

struct linted_spawn_file_actions
{
    size_t action_count;
    union file_action actions[];
};

struct linted_spawn_attr
{
    pid_t pgroup;
    bool setpgroup : 1;
};

struct spawn_error
{
    linted_error errnum;
};

static int execveat(int dirfd, const char *filename, char *const argv[],
                    char *const envp[]);
static size_t align_to_page_size(size_t size);
static void exit_with_error(volatile struct spawn_error *spawn_error,
                            linted_error errnum);

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp)
{
    linted_error errnum;
    struct linted_spawn_attr *attr;

    attr = linted_mem_alloc_zeroed(&errnum, sizeof *attr);
    if (errnum != 0) {
        return errnum;
    }

    *attrp = attr;
    return 0;
}

void linted_spawn_attr_setpgroup(struct linted_spawn_attr *attr, pid_t pgroup)
{
    attr->setpgroup = true;
    attr->pgroup = pgroup;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr *attr)
{
    linted_mem_free(attr);
}

linted_error linted_spawn_file_actions_init(struct linted_spawn_file_actions
                                            **file_actionsp)
{
    linted_error errnum;
    struct linted_spawn_file_actions *file_actions;

    file_actions = linted_mem_alloc_zeroed(&errnum, sizeof *file_actions);
    if (errnum != 0) {
        return errnum;
    }

    *file_actionsp = file_actions;
    return 0;
}

linted_error linted_spawn_file_actions_adddup2(struct linted_spawn_file_actions
                                               **file_actionsp,
                                               int oldfildes, int newfildes)
{
    linted_error errnum;
    struct linted_spawn_file_actions *file_actions;
    struct linted_spawn_file_actions *new_file_actions;
    union file_action *new_action;
    size_t old_count;
    size_t new_count;

    file_actions = *file_actionsp;

    old_count = file_actions->action_count;
    new_count = old_count + 1u;
    new_file_actions = linted_mem_realloc(
        &errnum, file_actions,
        sizeof *file_actions + new_count * sizeof file_actions->actions[0u]);
    if (errnum != 0) {
        return errnum;
    }

    new_file_actions->action_count = new_count;

    new_action = &new_file_actions->actions[old_count];

    new_action->type = FILE_ACTION_ADDDUP2;
    new_action->adddup2.oldfildes = oldfildes;
    new_action->adddup2.newfildes = newfildes;

    *file_actionsp = new_file_actions;

    return 0;
}

void linted_spawn_file_actions_destroy(struct linted_spawn_file_actions
                                       *file_actions)
{
    linted_mem_free(file_actions);
}

linted_error linted_spawn(pid_t *childp, int dirfd, char const *filename,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char *const argv[], char *const envp[])
{
    linted_error errnum = 0;
    bool is_relative_path = filename[0u] != '/';
    bool at_fdcwd = AT_FDCWD == dirfd;
    if (is_relative_path && !at_fdcwd && dirfd < 0) {
        return EBADF;
    }

    /*
     * So adddup2 works use memory mapping instead of a pipe to
     * communicate an error.
     */
    size_t spawn_error_length = align_to_page_size(sizeof(struct spawn_error));

    volatile struct spawn_error *spawn_error
        = mmap(NULL, spawn_error_length, PROT_READ | PROT_WRITE,
               MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == spawn_error) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    pid_t child;
    {
        /*
         * To save stack space reuse the same sigset for the full set and
         * the old set.
         */
        sigset_t sigset;
        sigfillset(&sigset);

        pthread_sigmask(SIG_BLOCK, &sigset, &sigset);

        child = fork();
        if (-1 == child) {
            errnum = errno;
            assert(errnum != 0);
        }

        if (0 == child) {
            /*
             * Get rid of signal handlers so that they can't be called before
             * execve.
             */
            for (int ii = 1; ii < NSIG; ++ii) {
                /* Uncatchable, avoid Valgrind warnings */
                if (SIGSTOP == ii || SIGKILL == ii) {
                    continue;
                }

                struct sigaction action;
                sigaction(ii, NULL, &action);

                if (action.sa_handler != SIG_IGN) {
                    action.sa_handler = SIG_DFL;

                    sigaction(ii, &action, NULL);
                }
            }
        }

        pthread_sigmask(SIG_SETMASK, &sigset, NULL);

    }

    if (child != 0) {
        if (errnum != 0) {
            goto unmap_spawn_error;
        }

        if (attr != NULL) {
            if (attr->setpgroup) {
                if (-1 == setpgid(child, attr->pgroup)) {
                    errnum = errno;
                    assert(errnum != 0);

                    assert(errnum != EINVAL);
                    assert(EACCES == errnum || EPERM == errnum || ESRCH == errnum);
                    errnum = 0;
                }

                /**
                 * @todo return the fds used to kill child process
                 * groups back.
                 */
                int kill_fd_read;
                int kill_fd_write;
                {
                    int kill_fds[2u];
                    if (-1 == pipe2(kill_fds, O_CLOEXEC)) {
                        errnum = errno;
                        assert(errnum != 0);
                        goto unmap_spawn_error;
                    }
                    kill_fd_read = kill_fds[0u];
                    kill_fd_write = kill_fds[1u];
                }

                if (-1 == fcntl(kill_fd_read, F_SETSIG, (long)SIGKILL)) {
                    errnum = errno;
                    assert(errnum != 0);
                    goto unmap_spawn_error;
                }

                struct f_owner_ex ex
                    = { .type = F_OWNER_PGRP,
                        .pid = 0 == attr->pgroup ? child : attr->pgroup };
                if (-1 == fcntl(kill_fd_read, F_SETOWN_EX, &ex)) {
                    errnum = errno;
                    assert(errnum != 0);
                    goto unmap_spawn_error;
                }

                if (-1 == fcntl(kill_fd_read, F_SETFL, (long)O_ASYNC)) {
                    errnum = errno;
                    assert(errnum != 0);
                    goto unmap_spawn_error;
                }

                /*
                 * Duplicate the read fd so that it is closed after the write
                 * fd.
                 */
                int kill_fd_read_copy
                    = fcntl(kill_fd_read, F_DUPFD_CLOEXEC, (long)kill_fd_write);
                if (-1 == kill_fd_read_copy) {
                    errnum = errno;
                    assert(errnum != 0);
                    goto unmap_spawn_error;
                }

                if (-1 == linted_ko_close(kill_fd_read)) {
                    errnum = errno;
                    assert(errnum != 0);
                    goto unmap_spawn_error;
                }
            }
        }

        {
            siginfo_t info;
            {
                do {
                    if (-1 == waitid(P_PID, child, &info, WEXITED | WSTOPPED)) {
                        errnum = errno;
                        assert(errnum != 0);
                    } else {
                        errnum = 0;
                    }
                } while (EINTR == errnum);
                if (errnum != 0) {
                    goto unmap_spawn_error;
                }
            }

            switch (info.si_code) {
            case CLD_EXITED: {
                int exit_status = info.si_status;
                switch (exit_status) {
                case 0:
                    errnum = EINVAL;
                    goto unmap_spawn_error;

                default:
                    errnum = ENOSYS;
                    goto unmap_spawn_error;
                }
            }

            case CLD_DUMPED:
            case CLD_KILLED: {
#if defined __linux__
                int signo = info.si_status;
                switch (signo) {
                case SIGKILL:
                    errnum = ENOMEM;
                    break;

                case SIGSEGV:
                    /* Corrupted elf file */
                    errnum = ENOEXEC;
                    break;

                case SIGTERM:
                    /* Exited with error and passed an error code */
                    errnum = spawn_error->errnum;
                    assert(errnum != 0);
                    break;

                default:
                    errnum = ENOSYS;
                    break;
                }
                goto unmap_spawn_error;
#else
#error The exit status of an aborted execve is very OS specific
#endif
            }

            case CLD_STOPPED:
                if (-1 == kill(child, SIGCONT)) {
                    errnum = errno;
                    assert(errnum != 0);
                    assert(errnum != EINVAL);
                    assert(errnum != EPERM);
                    assert(errnum != ESRCH);
                    assert(false);
                }
                break;

            default:
                assert(false);
            }
        }

    unmap_spawn_error:
        if (-1 == munmap((void *)spawn_error, spawn_error_length)) {
            if (0 == errnum) {
                errnum = errno;
                assert(errnum != 0);
            }
        }

        *childp = child;
        return errnum;
    }

    if (attr != NULL) {
        if (attr->setpgroup) {
            if (-1 == setpgid(0, attr->pgroup)) {
                errnum = errno;
                assert(errnum != 0);
                if (errnum != EACCES) {
                    exit_with_error(spawn_error, errnum);
                }
            }
        }
    }

    /* Copy it in case it is overwritten */

    int dirfd_copy;

    if (is_relative_path && !at_fdcwd) {
        if (-1 == (dirfd_copy = fcntl(dirfd, F_DUPFD_CLOEXEC, (long)0))) {
            exit_with_error(spawn_error, errno);
        }
    }

    if (file_actions != NULL) {
        for (size_t ii = 0u; ii < file_actions->action_count; ++ii) {
            union file_action const *action = &file_actions->actions[ii];
            switch (action->type) {
            case FILE_ACTION_ADDDUP2: {
                int newfildes = action->adddup2.newfildes;

                if (is_relative_path && !at_fdcwd) {
                    if (dirfd_copy == newfildes) {
                        dirfd_copy
                            = fcntl(dirfd_copy, F_DUPFD_CLOEXEC, (long)0);
                        if (-1 == dirfd_copy) {
                            exit_with_error(spawn_error, errno);
                        }
                    }
                }

                if (-1 == dup2(action->adddup2.oldfildes, newfildes)) {
                    exit_with_error(spawn_error, errno);
                }
                break;
            }

            default:
                exit_with_error(spawn_error, EINVAL);
            }
        }
    }

    /* Bizarre hack to achieve error reporting on a bad execve */
    int stop_fd_read;
    int stop_fd_write;
    {
        int stop_fds[2u];
        if (-1 == pipe2(stop_fds, O_CLOEXEC)) {
            exit_with_error(spawn_error, errno);
        }
        stop_fd_read = stop_fds[0u];
        stop_fd_write = stop_fds[1u];
    }

    if (-1 == fcntl(stop_fd_read, F_SETSIG, (long)SIGSTOP)) {
        exit_with_error(spawn_error, errno);
    }

    if (-1 == fcntl(stop_fd_read, F_SETOWN, (long)getpid())) {
        exit_with_error(spawn_error, errno);
    }

    if (-1 == fcntl(stop_fd_read, F_SETFL, (long)O_ASYNC)) {
        exit_with_error(spawn_error, errno);
    }

    /*
     * Duplicate the read fd so that it is closed after the write fd.
     */
    if (-1 == fcntl(stop_fd_read, F_DUPFD_CLOEXEC, (long)stop_fd_write)) {
        exit_with_error(spawn_error, errno);
    }

    execveat(dirfd_copy, filename, argv, envp);

    exit_with_error(spawn_error, errno);
    return 0;
}

static int execveat(int dirfd, const char *filename, char *const argv[],
                    char *const envp[])
{
    linted_error errnum;
    bool is_relative_path = filename[0u] != '/';
    bool at_fdcwd = AT_FDCWD == dirfd;
    char *new_path = NULL;

    if (is_relative_path && !at_fdcwd) {
        new_path = linted_mem_alloc(&errnum, strlen("/proc/self/fd/") + 10u
                                             + strlen(filename) + 1u);
        if (errnum != 0) {
            errno = errnum;
            return -1;
        }
        sprintf(new_path, "/proc/self/fd/%i/%s", dirfd, filename);
        filename = new_path;
    }

    execve(filename, argv, envp);

    {
        errnum = errno;
        linted_mem_free(new_path);
        errno = errnum;
    }

    return -1;
}

static size_t align_to_page_size(size_t size)
{
    size_t page_size = sysconf(_SC_PAGESIZE);

    /* This should always be true */
    assert(page_size > 0u);

    /* Round up to a multiple of page size */
    size_t remainder = size % page_size;
    if (0u == remainder)
        return size;

    return size - remainder + page_size;
}

static void exit_with_error(volatile struct spawn_error *spawn_error,
                            linted_error errnum)
{
    spawn_error->errnum = errnum;

    /* Stop the SIG_IGN handler from catching SIGTERM */
    {
        struct sigaction action;
        memset(&action, 0, sizeof action);
        action.sa_handler = SIG_DFL;

        sigaction(SIGTERM, &action, NULL);
    }

    /* Unlock SIGTERM */
    {
        sigset_t termset;
        sigemptyset(&termset);
        sigaddset(&termset, SIGTERM);

        pthread_sigmask(SIG_UNBLOCK, &termset, NULL);
    }

    raise(SIGTERM);

    assert(false);
}
