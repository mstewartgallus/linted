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

#include "linted/io.h"

#include <assert.h>
#include <errno.h>
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

struct adddup2 {
    enum file_action_type type;
    int oldfildes;
    int newfildes;
};

union file_action {
    enum file_action_type type;
    struct adddup2 adddup2;
};

struct linted_spawn_file_actions {
    size_t action_count;
    union file_action actions[];
};

struct linted_spawn_attr {
    pid_t pgroup;
    bool setpgroup : 1;
};

struct spawn_error {
    errno_t errnum;
};

static void exit_with_error(volatile struct spawn_error* spawn_error,
                            errno_t errnum);

errno_t linted_spawn_attr_init(struct linted_spawn_attr** attrp)
{
    struct linted_spawn_attr* attr = malloc(sizeof *attr);
    if (NULL == attr) {
        return errno;
    }

    memset(attr, 0, sizeof *attr);

    *attrp = attr;
    return 0;
}

void linted_spawn_attr_setpgroup(struct linted_spawn_attr* attr, pid_t pgroup)
{
    attr->setpgroup = true;
    attr->pgroup = pgroup;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr* attr)
{
    free(attr);
}

errno_t linted_spawn_file_actions_init(
    struct linted_spawn_file_actions** file_actionsp)
{
    struct linted_spawn_file_actions* file_actions = malloc(sizeof *file_actions);
    if (NULL == file_actions) {
        return errno;
    }

    memset(file_actions, 0, sizeof *file_actions);

    *file_actionsp = file_actions;
    return 0;
}

errno_t linted_spawn_file_actions_adddup2(
    struct linted_spawn_file_actions** file_actionsp, int oldfildes,
    int newfildes)
{
    struct linted_spawn_file_actions* file_actions;
    struct linted_spawn_file_actions* new_file_actions;
    union file_action* new_action;
    size_t old_count;
    size_t new_count;

    file_actions = *file_actionsp;

    old_count = file_actions->action_count;
    new_count = old_count + 1;
    new_file_actions = realloc(file_actions, sizeof *file_actions + new_count * sizeof file_actions->actions[0]);
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
    struct linted_spawn_file_actions* file_actions)
{
    free(file_actions);
}

errno_t linted_spawn(pid_t* childp, int dirfd, char const* path,
                     struct linted_spawn_file_actions const* file_actions,
                     struct linted_spawn_attr const* attr, char* const argv[],
                     char* const envp[])
{
    /*
   * So adddup2 works use memory mapping instead of a pipe to
   * communicate an error.
   */
    long page_size = sysconf(_SC_PAGESIZE);

    /* Align size to the page length  */
    size_t spawn_error_length = ((sizeof(struct spawn_error) + page_size - 1) / page_size) * page_size;

    volatile struct spawn_error* spawn_error = mmap(NULL, spawn_error_length, PROT_READ | PROT_WRITE,
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

        errno_t error_status = 0;

        if (-1 == child) {
            error_status = errno;
            goto unmap_spawn_error;
        }

        {
            siginfo_t info;
            {
                errno_t errnum;
                do {
                    int wait_status = waitid(P_PID, child, &info, WEXITED | WSTOPPED);
                    errnum = -1 == wait_status ? errno : 0;
                } while (EINTR == errnum);
                if (errnum != 0) {
                    error_status = errnum;
                    goto unmap_spawn_error;
                }
            }

            switch (info.si_code) {
            case CLD_EXITED: {
                errno_t exit_status = info.si_status;
                switch (exit_status) {
                case 0:
                    error_status = EINVAL;
                    goto unmap_spawn_error;

                default:
                    error_status = ENOSYS;
                    goto unmap_spawn_error;
                }
            }

            case CLD_KILLED: {
#if defined __linux__
                errno_t signo = info.si_status;
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

            case CLD_DUMPED:
                error_status = ENOSYS;
                goto unmap_spawn_error;

            case CLD_STOPPED:
                if (-1 == kill(child, SIGCONT)) {
                    errno_t errnum = errno;
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
        if (-1 == munmap((void*)spawn_error, spawn_error_length)) {
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
                errno_t errnum = errno;
                if (errnum != EACCES) {
                    exit_with_error(spawn_error, errnum);
                }
            }
        }
    }

    /* This must be done before file actions to prevent the wrong
   * directory being used.
   */
    if (-1 == fchdir(dirfd)) {
        exit_with_error(spawn_error, errno);
    }

    if (file_actions != NULL) {
        for (size_t ii = 0; ii < file_actions->action_count; ++ii) {
            union file_action const* action = &file_actions->actions[ii];
            switch (action->type) {
            case FILE_ACTION_ADDDUP2:
                if (-1 == dup2(action->adddup2.oldfildes, action->adddup2.newfildes)) {
                    exit_with_error(spawn_error, errno);
                }
                break;

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

    execve(path, argv, envp);

    exit_with_error(spawn_error, errno);
    return 0;
}

static void exit_with_error(volatile struct spawn_error* spawn_error,
                            errno_t errnum)
{
    spawn_error->errnum = errnum;

    sigset_t termset;
    sigemptyset(&termset);
    sigaddset(&termset, SIGTERM);

    pthread_sigmask(SIG_UNBLOCK, &termset, NULL);

    raise(SIGTERM);

    assert(false);
}
