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

#include <assert.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
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

static size_t align_to_page_size(size_t size);
static void exit_with_error(volatile struct spawn_error *spawn_error,
                            linted_error errnum);

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp)
{
    struct linted_spawn_attr *attr = malloc(sizeof *attr);
    if (NULL == attr) {
        return errno;
    }

    memset(attr, 0, sizeof *attr);

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
    free(attr);
}

linted_error
linted_spawn_file_actions_init(struct linted_spawn_file_actions **file_actionsp)
{
    struct linted_spawn_file_actions *file_actions =
        malloc(sizeof *file_actions);
    if (NULL == file_actions) {
        return errno;
    }

    memset(file_actions, 0, sizeof *file_actions);

    *file_actionsp = file_actions;
    return 0;
}

linted_error linted_spawn_file_actions_adddup2(
    struct linted_spawn_file_actions **file_actionsp, int oldfildes,
    int newfildes)
{
    struct linted_spawn_file_actions *file_actions;
    struct linted_spawn_file_actions *new_file_actions;
    union file_action *new_action;
    size_t old_count;
    size_t new_count;

    file_actions = *file_actionsp;

    old_count = file_actions->action_count;
    new_count = old_count + 1;
    new_file_actions =
        realloc(file_actions, sizeof *file_actions +
                                  new_count * sizeof file_actions->actions[0]);
    if (NULL == new_file_actions) {
        return errno;
    }

    new_file_actions->action_count = new_count;

    new_action = &new_file_actions->actions[old_count];

    new_action->type = FILE_ACTION_ADDDUP2;
    new_action->adddup2.oldfildes = oldfildes;
    new_action->adddup2.newfildes = newfildes;

    *file_actionsp = new_file_actions;

    return 0;
}

void linted_spawn_file_actions_destroy(
    struct linted_spawn_file_actions *file_actions)
{
    free(file_actions);
}

linted_error linted_spawn(pid_t *childp, int dirfd, char const *path,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char *const argv[], char *const envp[])
{
    bool is_relative_path = path[0] != '/';
    bool at_fdcwd = AT_FDCWD == dirfd;
    if (is_relative_path && !at_fdcwd && dirfd < 0) {
        return EBADF;
    }

    /*
     * So adddup2 works use memory mapping instead of a pipe to
     * communicate an error.
     */
    size_t spawn_error_length = align_to_page_size(sizeof(struct spawn_error));

    volatile struct spawn_error *spawn_error =
        mmap(NULL, spawn_error_length, PROT_READ | PROT_WRITE,
             MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == spawn_error) {
        return errno;
    }
    spawn_error->errnum = 0;

    sigset_t fullset;
    sigfillset(&fullset);

    sigset_t oldset;
    pthread_sigmask(SIG_BLOCK, &fullset, &oldset);

    pid_t child = fork();
    if (child != 0) {
        pthread_sigmask(SIG_SETMASK, &oldset, NULL);

        linted_error error_status = 0;

        if (-1 == child) {
            error_status = errno;
            goto unmap_spawn_error;
        }

        if (attr != NULL) {
            if (attr->setpgroup) {
                if (-1 == setpgid(child, attr->pgroup)) {
                    linted_error errnum = errno;

                    assert(errnum != EINVAL);
                    assert(EACCES == errnum || EPERM == errnum ||
                           ESRCH == errnum);
                }

                /**
                 * @todo return the fds used to kill child process
                 * groups back.
                 */
                int kill_fd_read;
                int kill_fd_write;
                {
                    int kill_fds[2];
                    if (-1 == pipe2(kill_fds, O_CLOEXEC)) {
                        error_status = errno;
                        goto unmap_spawn_error;
                    }
                    kill_fd_read = kill_fds[0];
                    kill_fd_write = kill_fds[1];
                }

                if (-1 == fcntl(kill_fd_read, F_SETSIG, (long)SIGKILL)) {
                    error_status = errno;
                    goto unmap_spawn_error;
                }

                struct f_owner_ex ex = { .type = F_OWNER_PGRP,
                                         .pid = 0 == attr->pgroup
                                                    ? child
                                                    : attr->pgroup };
                if (-1 == fcntl(kill_fd_read, F_SETOWN_EX, &ex)) {
                    error_status = errno;
                    goto unmap_spawn_error;
                }

                if (-1 == fcntl(kill_fd_read, F_SETFL, (long)O_ASYNC)) {
                    error_status = errno;
                    goto unmap_spawn_error;
                }

                /*
                 * Duplicate the read fd so that it is closed after the write
                 * fd.
                 */
                int kill_fd_read_copy =
                    fcntl(kill_fd_read, F_DUPFD_CLOEXEC, (long)kill_fd_write);
                if (-1 == kill_fd_read_copy) {
                    error_status = errno;
                    goto unmap_spawn_error;
                }

                if (-1 == linted_ko_close(kill_fd_read)) {
                    error_status = errno;
                    goto unmap_spawn_error;
                }
            }
        }

        {
            siginfo_t info;
            {
                linted_error errnum;
                do {
                    int wait_status =
                        waitid(P_PID, child, &info, WEXITED | WSTOPPED);
                    errnum = -1 == wait_status ? errno : 0;
                } while (EINTR == errnum);
                if (errnum != 0) {
                    error_status = errnum;
                    goto unmap_spawn_error;
                }
            }

            switch (info.si_code) {
            case CLD_EXITED: {
                linted_error exit_status = info.si_status;
                switch (exit_status) {
                case 0:
                    error_status = EINVAL;
                    goto unmap_spawn_error;

                default:
                    error_status = ENOSYS;
                    goto unmap_spawn_error;
                }
            }

            case CLD_DUMPED:
            case CLD_KILLED: {
#if defined __linux__
                linted_error signo = info.si_status;
                switch (signo) {
                case SIGKILL:
                    error_status = ENOMEM;
                    break;

                case SIGSEGV:
                    /* Corrupted elf file */
                    error_status = ENOEXEC;
                    break;

                case SIGTERM:
                    /* Exited with error and passed an error code */
                    error_status = spawn_error->errnum;
                    break;

                default:
                    error_status = ENOSYS;
                    break;
                }
                goto unmap_spawn_error;
#else
#error The exit status of an aborted execve is very OS specific
#endif
            }

            case CLD_STOPPED:
                if (-1 == kill(child, SIGCONT)) {
                    linted_error errnum = errno;
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
            if (0 == error_status) {
                error_status = errno;
            }
        }

        *childp = child;
        return error_status;
    }

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

    pthread_sigmask(SIG_SETMASK, &oldset, NULL);

    if (attr != NULL) {
        if (attr->setpgroup) {
            if (-1 == setpgid(0, attr->pgroup)) {
                linted_error errnum = errno;
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
        for (size_t ii = 0; ii < file_actions->action_count; ++ii) {
            union file_action const *action = &file_actions->actions[ii];
            switch (action->type) {
            case FILE_ACTION_ADDDUP2: {
                int newfildes = action->adddup2.newfildes;

                if (is_relative_path && !at_fdcwd) {
                    if (dirfd_copy == newfildes) {
                        dirfd_copy =
                            fcntl(dirfd_copy, F_DUPFD_CLOEXEC, (long)0);
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
        int stop_fds[2];
        if (-1 == pipe2(stop_fds, O_CLOEXEC)) {
            exit_with_error(spawn_error, errno);
        }
        stop_fd_read = stop_fds[0];
        stop_fd_write = stop_fds[1];
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
     * Duplicate the read fd so that it is closed after the write
     * fd.
     */
    if (-1 == fcntl(stop_fd_read, F_DUPFD_CLOEXEC, (long)stop_fd_write)) {
        exit_with_error(spawn_error, errno);
    }

    if (is_relative_path && !at_fdcwd) {
        char *new_path =
            malloc(strlen("/proc/self/fd/") + 10 + strlen(path) + 1);
        if (NULL == new_path) {
            exit_with_error(spawn_error, errno);
        }
        sprintf(new_path, "/proc/self/fd/%i/%s", dirfd_copy, path);
        path = new_path;
    }

    execve(path, argv, envp);

    exit_with_error(spawn_error, errno);
    return 0;
}

static size_t align_to_page_size(size_t size)
{
    size_t page_size = sysconf(_SC_PAGESIZE);

    /* This should always be true */
    assert(page_size > 0);

    /* Round up to a multiple of page size */
    return (size / page_size + 1u) * page_size;
}

static void exit_with_error(volatile struct spawn_error *spawn_error,
                            linted_error errnum)
{
    spawn_error->errnum = errnum;

    /* Stop the SIG_IGN handler from catching SIGTERM */
    {
        struct sigaction action;
        memset(&action, 0, sizeof 0);
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
