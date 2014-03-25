/*
 * Copyright 2013 Steven Stewart-Gallus
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
#include "config.h"

#include "linted/spawner.h"

#include "linted/io.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <pthread.h>
#include <stdbool.h>
#include <sys/prctl.h>
#include <sys/signalfd.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <unistd.h>

/*
 * We fork from a known good state and serve out forks of this known
 * good state. This avoids several problems with inheritance of
 * corrupted state that aren't even fixable with exec.
 *
 * We send over function pointers which allows us to avoid the nasty
 * command line interface exec forces us into.
 *
 * Posix requires an exact copy of process memory so I believe passing
 * around function pointers through pipes is allowed.
 *
 * A great amount of work is done so that only the processes spawned
 * by this function and not concurrently or before are waited on.
 *
 * We create and move processes to a new process group so that only
 * these specific processes are waited on instead of processes spawned
 * before this function is invoked or spawned by concurrent
 * threads. Both the spawner and the child must set the process group
 * to avoid a race condition.
 *
 * We wait on a separate thread so that we don't have to use signal
 * handling (which messes with global state).
 *
 * A prctl call is used so that the child knows about the parents
 * death.
 *
 * Note that it makes sense for spawner forks to directly exit on
 * errors.
 *
 * TODO: Move away from Linux specifics such as prctl and signalfd.
 */

struct waiter_arguments {
    id_t process_group;
    int waiter_pipe;
};

static void handle_child_info(siginfo_t * child_info);

int linted_process_spawner_run(linted_spawner inbox, void *context)
{
    int exit_status = -1;

    sigset_t sigchld_sigset;
    sigemptyset(&sigchld_sigset);
    sigaddset(&sigchld_sigset, SIGCHLD);

    sigset_t old_sigset;
    if (-1 == pthread_sigmask(SIG_BLOCK, &sigchld_sigset, &old_sigset)) {
        if (EINVAL == errno) {
            LINTED_IMPOSSIBLE_ERROR("could not set the signal mask: %s",
                                    linted_error_string_alloc(errno));
        }

        return -1;
    }

    int sfd = signalfd(-1, &sigchld_sigset, SFD_CLOEXEC);
    if (-1 == sfd) {
        goto restore_sigmask;
    }

    /*
     * This is a little bit confusing but the first process's pid is
     * reused for the process group. We create a new process group so
     * we can wait on that only.
     */
    pid_t process_group = -1;

    for (;;) {
        int greatest = (inbox > sfd) ? inbox : sfd;
        fd_set watched_fds;
        int select_status;

        do {
            FD_ZERO(&watched_fds);
            FD_SET(inbox, &watched_fds);
            FD_SET(sfd, &watched_fds);

            select_status = select(greatest + 1, &watched_fds,
                                   NULL, NULL, NULL);
        } while (-1 == select_status && EINTR == errno);
        if (-1 == select_status) {
            if (EINVAL == errno) {
                LINTED_IMPOSSIBLE_ERROR("could not select over file descriptors: %s",
                                        linted_error_string_alloc(errno));
            }

            goto kill_processes;
        }

        if (FD_ISSET(sfd, &watched_fds)) {
            pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);

            /*
             * Don't directly consume the signals but let them be
             * consumed if they should be. If the current thread was
             * blocking SIGCHLD in the beginning it'll still be
             * blocking here.
             */
            pthread_sigmask(SIG_BLOCK, &sigchld_sigset, NULL);

            for (;;) {
                siginfo_t child_info;

                /* Set to zero so it can be checked later */
                child_info.si_pid = 0;

                int wait_status;
                do {
                    wait_status = waitid(P_PGID, process_group, &child_info,
                                         WEXITED | WNOHANG);
                } while (-1 == wait_status && EINTR == errno);
                if (-1 == wait_status) {
                    /*
                     * POSIX errors are ECHILD, EINTR and EINVAL but
                     * more could be defined by the system.
                     */
                    assert(errno != EINVAL);

                    if (errno != ECHILD) {
                        goto kill_processes;
                    }
                    break;
                }

                if (0 == child_info.si_pid) {
                    break;
                }

                handle_child_info(&child_info);
            }
        }

        if (FD_ISSET(inbox, &watched_fds)) {
            int connection_status = -1;
            linted_spawner_future connection;

            ssize_t bytes_read;
            do {
                bytes_read = linted_spawner_recv_future(inbox, &connection);
            } while (-1 == bytes_read && EINTR == errno);
            if (-1 == bytes_read) {
                goto kill_processes;
            }

            if (0 == bytes_read) {
                goto wait_for_processes_exit;
            }

            /* Luckily, because we already must fork for a fork server
             * we get asynchronous behaviour for free.
             */
            pid_t child = fork();
            if (-1 == process_group) {
                /* The first process id is used for the process group */
                process_group = child;
            }
            switch (child) {
            case 0:
                {
                    if (-1 == pthread_sigmask(SIG_SETMASK, &old_sigset, NULL)) {
                        exit(errno);
                    }

                    if (-1 == prctl(PR_SET_PDEATHSIG, SIGHUP)) {
                        exit(errno);
                    }

                    if (-1 == setpgid(child, process_group)) {
                        exit(errno);
                    }

                    if (-1 == close(sfd)) {
                        exit(errno);
                    }

                    if (-1 == close(inbox)) {
                        exit(errno);
                    }

                    struct linted_spawner_request request;
                    if (-1 == linted_spawner_recv_request(connection, &request)) {
                        exit(errno);
                    }

                    if (-1 == request.task(context, request.fildes)) {
                        exit(errno);
                    }

                    exit(0);
                }

            case -1:
                if (-1 == linted_spawner_deny_request(connection, errno)) {
                    goto close_connection;
                }
                break;

            default:
                if (-1 == setpgid(child, process_group) && errno != EACCES) {
                    if (EINVAL == errno || EPERM == errno || ESRCH == errno) {
                        LINTED_IMPOSSIBLE_ERROR(
                            "could not setpgid with a valid pid and pgid: %s",
                            linted_error_string_alloc(errno));
                    } else {
                        goto close_connection;
                    }
                }
                break;
            }

            connection_status = 0;

         close_connection:
            {
                int errnum = errno;

                if (-1 == close(connection)) {
                    if (EBADF == errno) {
                        LINTED_IMPOSSIBLE_ERROR(
                            "could not close open connection: %s",
                            linted_error_string_alloc(errno));
                    }
                    /*
                     * If we get another type of error we were sent a
                     * bad connection. That's the client's problem and
                     * not ours.
                     */
                }

                errno = errnum;
            }

            if (-1 == connection_status) {
                goto kill_processes;
            }
        }
    }

 wait_for_processes_exit:
    if (process_group != -1) {
        for (;;) {
            siginfo_t child_info;

            int wait_status;
            do {
                wait_status = waitid(P_PGID, process_group, &child_info,
                                     WEXITED);
            } while (-1 == wait_status && EINTR == errno);
            if (-1 == wait_status) {
                /* POSIX errors are ECHILD, EINTR and EINVAL */
                assert(errno != EINVAL);

                if (errno != ECHILD) {
                    goto kill_processes;
                }
                break;
            }

            handle_child_info(&child_info);
        }
    }

    exit_status = 0;

 kill_processes:
    /* Notify all spawned processes on error */
    if (-1 == exit_status && process_group != -1) {
        int errnum = errno;

        if (-1 == kill(-process_group, SIGHUP)) {
            if (EINVAL == errno || ESRCH == errno || EPERM == errno) {
                LINTED_IMPOSSIBLE_ERROR(
                    "could not kill processes with a valid process group: %s",
                    linted_error_string_alloc(errno));
            }
            /*
             * We can only report one error at a time. Ignore this
             * other one.
             */
        }

        errno = errnum;
    }

    {
        int errnum = errno;
        int close_status = close(sfd);
        if (-1 == close_status && EBADF == errno) {
            LINTED_IMPOSSIBLE_ERROR("could not close open file: %s",
                                    linted_error_string_alloc(errno));
        }

        if (-1 == exit_status) {
            errno = errnum;
        }

        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 restore_sigmask:
    {
        int errnum = errno;
        pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);
        errno = errnum;
    }

    return exit_status;
}

static void handle_child_info(siginfo_t * child_info)
{
    pid_t child = child_info->si_pid;

    /* Waited on a dead process. Log and wait for more. */
    switch (child_info->si_code) {
    case CLD_EXITED:{
            int return_value = child_info->si_status;
            if (0 == return_value) {
                linted_io_write_format_string(STDERR_FILENO, NULL,
                                              "child %ju executed normally\n",
                                              (uintmax_t) child);
            } else {
                char const *error = linted_error_string_alloc(return_value);
                linted_io_write_format_string(STDERR_FILENO, NULL,
                                              "child %ju executed with error: %s\n",
                                              (uintmax_t) child, error);
                linted_error_string_free(error);
            }
            break;
        }

    case CLD_DUMPED:
    case CLD_KILLED:
        linted_io_write_format_string(STDERR_FILENO, NULL,
                                      "child %ju was terminated with signal number %i\n",
                                      (uintmax_t) child, child_info->si_status);
        break;

    default:
        /* TODO: Possibly assert that this shouldn't happen */
        linted_io_write_format_string(STDERR_FILENO, NULL,
                                      "child %ju executed abnormally\n",
                                      (uintmax_t) child);
        break;
    }
}
