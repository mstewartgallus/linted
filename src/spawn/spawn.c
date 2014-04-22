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
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

enum file_action_type { FILE_ACTION_ADDDUP2 };

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

struct attr_flags {
    bool setpgroup: 1;
};

struct linted_spawn_attr {
    struct attr_flags flags;
    pid_t pgroup;
};

static void exit_with_error(int error_status_fd_write, errno_t errnum);

errno_t linted_spawn_attr_init(struct linted_spawn_attr ** attrp) {
    struct linted_spawn_attr * attr = malloc(sizeof *attr);
    if (NULL == attr) {
        return errno;
    }

    memset(attr, 0, sizeof *attr);

    *attrp = attr;
    return 0;
}

void linted_spawn_attr_setpgroup(struct linted_spawn_attr * attr, pid_t pgroup)
{
    attr->flags.setpgroup = true;
    attr->pgroup = pgroup;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr * attr) {
    free(attr);
}

errno_t linted_spawn_file_actions_init(struct linted_spawn_file_actions ** file_actionsp) {
    struct linted_spawn_file_actions * file_actions = malloc(sizeof *file_actions);
    if (NULL == file_actions) {
        return errno;
    }

    memset(file_actions, 0, sizeof *file_actions);

    *file_actionsp = file_actions;
    return 0;
}

errno_t linted_spawn_file_actions_adddup2(struct linted_spawn_file_actions ** file_actionsp,
                                          int oldfildes,
                                          int newfildes) {
    struct linted_spawn_file_actions * file_actions;
    struct linted_spawn_file_actions * new_file_actions;
    union file_action * new_action;
    size_t old_count;
    size_t new_count;

    file_actions = *file_actionsp;

    old_count = file_actions->action_count;
    new_count = old_count + 1;
    new_file_actions = realloc(file_actions,
                               sizeof *file_actions
                               + new_count * sizeof file_actions->actions[0]);
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

void linted_spawn_file_actions_destroy(struct linted_spawn_file_actions * file_actions) {
    free(file_actions);
}

errno_t linted_spawn(pid_t * childp, char const * path,
                     struct linted_spawn_file_actions const * file_actions,
                     struct linted_spawn_attr const * attr,
                     char * const argv[], char * const envp[])
{
    int error_status_fd_read;
    int error_status_fd_write;
    {
        int error_status_fds[2];
        if (-1 == pipe2(error_status_fds, O_CLOEXEC)) {
            return errno;
        }
        error_status_fd_read = error_status_fds[0];
        error_status_fd_write = error_status_fds[1];
    }

    pid_t child = fork();
    if (child != 0) {
        errno_t error_status = 0;

        if (-1 == child) {
            error_status = errno;
            goto close_fds;
        }

        {
            siginfo_t info;
            {
                errno_t errnum;
                do {
                    int wait_status = waitid(P_PID, child, &info,
                                             WEXITED | WSTOPPED);
                    errnum = -1 == wait_status ? errno : 0;
                } while (EINTR == errnum);
                if (errnum != 0) {
                    error_status = errnum;
                    goto close_fds;
                }
            }

            switch (info.si_code) {
                {
                case CLD_EXITED:;
                    errno_t exit_status = info.si_status;
                    switch (exit_status) {
                    case 0:
                        error_status = EINVAL;
                        goto close_fds;

                    default:
                        error_status = ENOSYS;
                        goto close_fds;
                    }
                }

                {
                case CLD_KILLED:;
                    errno_t signo = info.si_status;
                    if (signo != SIGKILL) {
                        error_status = ENOSYS;
                        goto close_fds;
                    }

                    errno_t errnum;

                    errno_t read_status = linted_io_read_all(error_status_fd_read, NULL,
                                                             &errnum, sizeof errnum);
                    if (read_status != 0) {
                        error_status = read_status;
                    } else {
                        error_status = errnum;
                    }
                    goto close_fds;
                }

            case CLD_DUMPED:
                error_status = ENOSYS;
                goto close_fds;

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

    close_fds:
        {
            errno_t errnum = linted_io_close(error_status_fd_read);
            if (0 == error_status) {
                error_status = errnum;
            }
        }

        {
            errno_t errnum = linted_io_close(error_status_fd_write);
            if (0 == error_status) {
                error_status = errnum;
            }
        }

        *childp = child;
        return error_status;
    }

    if (attr->flags.setpgroup) {
        if (-1 == setpgid(0, attr->pgroup)) {
            errno_t errnum = errno;
            if (errnum != EACCES) {
                exit_with_error(error_status_fd_write, errnum);
            }
        }
    }

    for (size_t ii = 0; ii < file_actions->action_count; ++ii) {
        union file_action const * action = &file_actions->actions[ii];
        switch (action->type) {
        case FILE_ACTION_ADDDUP2:
            if (-1 == dup2(action->adddup2.oldfildes,
                           action->adddup2.newfildes)) {
                exit_with_error(error_status_fd_write, errno);
            }
            break;

        default:
            exit_with_error(error_status_fd_write, EINVAL);
        }
    }

    int stop_fd_read;
    int stop_fd_write;
    {
        int stop_fds[2];
        if (-1 == pipe2(stop_fds, O_CLOEXEC)) {
            exit_with_error(error_status_fd_write, errno);
        }
        stop_fd_read = stop_fds[0];
        stop_fd_write = stop_fds[1];
    }

    if (-1 == fcntl(stop_fd_read, F_SETSIG, (long) SIGSTOP)) {
        exit_with_error(error_status_fd_write, errno);
    }

    if (-1 == fcntl(stop_fd_read, F_SETOWN, (long) getpid())) {
        exit_with_error(error_status_fd_write, errno);
    }

    if (-1 == fcntl(stop_fd_read, F_SETFL, O_ASYNC)) {
        exit_with_error(error_status_fd_write, errno);
    }

    /*
     * Duplicate the read fd so that it is closed after the write
     * fd.
     */
    if (-1 == fcntl(stop_fd_read, F_DUPFD_CLOEXEC, (long) stop_fd_write)) {
        exit_with_error(error_status_fd_write, errno);
    }

    execve(path, argv, envp);

    exit_with_error(error_status_fd_write, errno);
    return 0;
}

static void exit_with_error(int error_status_fd_write, errno_t errnum)
{
    linted_io_write_all(error_status_fd_write, NULL, &errnum, sizeof errnum);

    raise(SIGKILL);

    assert(false);
}
