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
#include <inttypes.h>
#include <stddef.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
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
 * We create and move processes to a new process group so that only
 * these specific processes are waited on instead of processes spawned
 * before this function is invoked or spawned by concurrent
 * threads. Both the spawner and the child must set the process group
 * to avoid a race condition.
 *
 * A prctl call is used so that the child knows about the parents
 * death.
 *
 * Note that code running under the spawner or the spawner forks must
 * be able to run without the system logger active.
 *
 * Note that it makes sense for spawner forks to directly exit on
 * errors.
 *
 * TODO: Move away from Linux specifics such as signalfd, and prctl.
 *
 * TODO: Does signalfd listen to child death of processes spawned by
 * other threads? If so, find a way to only listen to SIGCHLD for
 * processes spawned by my processes.
 */
static void exec_task(linted_spawner_task task, int const fildes[]);

static int wait_on_children(id_t process_group);

int linted_process_spawner_run(linted_spawner inbox,
                               int const preserved_fildes[],
                               size_t preserved_fildes_size,
                               linted_spawner_task main_loop,
                               int const fildes[], size_t fildes_size)
{
    int exit_status = -1;

    sigset_t old_sigset;
    int sigchld_fd;
    pid_t process_group;

    {
        sigset_t sigchld_set;

        int empty_set_status = sigemptyset(&sigchld_set);
        assert(empty_set_status != -1);

        int sigchld_set_status = sigaddset(&sigchld_set, SIGCHLD);
        assert(sigchld_set_status != -1);

        int mask_status = pthread_sigmask(SIG_BLOCK, &sigchld_set, &old_sigset);
        assert(0 == mask_status);

        sigset_t sigchld_mask;

        int sigchld_empty_status = sigemptyset(&sigchld_mask);
        assert(sigchld_empty_status != -1);

        int sigchld_add_status = sigaddset(&sigchld_mask, SIGCHLD);
        assert(sigchld_add_status != -1);

        sigchld_fd = signalfd(-1, &sigchld_mask, SFD_CLOEXEC);
        if (-1 == sigchld_fd) {
            goto restore_signal_mask;
        }
    }

    /*
     * This is a little bit confusing but the first processes pid is
     * reused for the process group. We create a new process group so
     * we can wait on that only.
     */
    process_group = fork();
    switch (process_group) {
    case 0:{
            int mask_status = pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);
            assert(0 == mask_status);

            if (-1 == prctl(PR_SET_PDEATHSIG, SIGHUP)) {
                exit(errno);
            }

            if (-1 == setpgid(process_group, process_group)) {
                exit(errno);
            }

            fd_set fds_not_to_close;
            FD_ZERO(&fds_not_to_close);

            for (size_t ii = 0; ii < fildes_size; ++ii) {
                FD_SET(fildes[ii], &fds_not_to_close);
            }

            for (size_t ii = 0; ii < preserved_fildes_size; ++ii) {
                FD_SET(preserved_fildes[ii], &fds_not_to_close);
            }

            if (-1 == linted_io_close_fds_except(&fds_not_to_close)) {
                exit(errno);
            }

            exec_task(main_loop, fildes);
        }

    case -1:
        goto close_sigchld_fd;
    }

    if (-1 == setpgid(process_group, process_group) && errno != EACCES) {
        goto close_sigchld_fd;
    }

    for (;;) {
        int greatest = (inbox > sigchld_fd) ? inbox : sigchld_fd;
        fd_set watched_fds;
        int select_status;

        do {
            FD_ZERO(&watched_fds);
            FD_SET(inbox, &watched_fds);
            FD_SET(sigchld_fd, &watched_fds);

            select_status = select(greatest + 1, &watched_fds,
                                   NULL, NULL, NULL);
        } while (-1 == select_status && EINTR == errno);
        if (-1 == select_status) {
            goto close_sigchld_fd;
        }

        if (FD_ISSET(sigchld_fd, &watched_fds)) {
            struct signalfd_siginfo siginfo;
            if (-1 == linted_io_read_all(sigchld_fd, NULL,
                                         &siginfo, sizeof siginfo)) {
                goto close_sigchld_fd;
            }

            /* We received a SIGCHLD */
            if (-1 == wait_on_children(process_group)) {
                if (ECHILD == errno) {
                    goto exit_fork_server;
                }

                goto close_sigchld_fd;
            }
        }

        if (FD_ISSET(inbox, &watched_fds)) {
            int connection_status = -1;
            linted_server_conn connection;

            ssize_t bytes_read;
            do {
                bytes_read = linted_spawner_recv_future(inbox,
                                                        &connection);

            } while (-1 == bytes_read && EINTR == errno);
            if (-1 == bytes_read) {
                goto restore_signal_mask;
            }

            /* Luckily, because we already must fork for a fork server we
             * get asynchronous behaviour for free.
             */
            pid_t child = fork();
            switch (child) {
            case 0:
            {
                int mask_status =
                    pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);
                assert(0 == mask_status);

                if (-1 == prctl(PR_SET_PDEATHSIG, SIGHUP)) {
                    exit(errno);
                }

                if (-1 == setpgid(child, process_group)) {
                    exit(errno);
                }

                fd_set fds_not_to_close;
                FD_ZERO(&fds_not_to_close);

                for (size_t ii = 0; ii < preserved_fildes_size; ++ii) {
                    FD_SET(preserved_fildes[ii], &fds_not_to_close);
                }

                FD_SET(connection, &fds_not_to_close);

                if (-1 == linted_io_close_fds_except(&fds_not_to_close)) {
                    exit(errno);
                }

                struct linted_spawner_request request;
                if (-1 == linted_spawner_recv_request(connection,
                                                      &request)) {
                    exit(errno);
                }

                exec_task(request.task, request.fildes);
            }

            case -1:
                if (-1 == linted_spawner_deny_request(connection, errno)) {
                    goto close_connection;
                }
            }

            if (-1 == setpgid(child, process_group) && errno != EACCES) {
                goto close_connection;
            }

            connection_status = 0;

        close_connection:
            {
                int errnum = errno;

                int close_status = linted_server_conn_close(connection);
                if (-1 == connection_status) {
                    errno = errnum;
                }

                if (-1 == close_status) {
                    connection_status = -1;
                }

                if (-1 == connection_status) {
                    goto close_sigchld_fd;
                }
            }
        }
    }

 exit_fork_server:
    exit_status = 0;

 close_sigchld_fd:
    {
        int errnum = errno;
        int close_status = close(sigchld_fd);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 restore_signal_mask:
    {
        int errnum = errno;

        int mask_status = pthread_sigmask(SIG_SETMASK, &old_sigset, NULL);
        assert(0 == mask_status);

        errno = errnum;
    }

    return exit_status;
}

static int wait_on_children(id_t process_group)
{
    for (;;) {
        siginfo_t child_info;

        {
            /*
             * Zero out the si_pid field so it can be checked for zero
             * and if no processes have exited.
             */
            pid_t *const si_pidp = &child_info.si_pid;
            memset(si_pidp, 0, sizeof *si_pidp);
        }

        int wait_status;
        do {
            wait_status = waitid(P_PGID, process_group, &child_info,
                                 WEXITED | WNOHANG);
        } while (-1 == wait_status && EINTR == errno);
        if (-1 == wait_status) {
            return -1;
        }

        pid_t const child = child_info.si_pid;
        if (0 == child) {
            return 0;
        }

        /* Waited on a dead process. Log and wait for more. */
        switch (child_info.si_code) {
        case CLD_EXITED:{
                int return_value = child_info.si_status;
                if (0 == return_value) {
                    fprintf(stderr, "child %ju executed normally\n",
                           (uintmax_t) child);
                } else {
                    char const *error = linted_error_string_alloc(return_value);
                    fprintf(stderr, "child %ju executed with error: %s\n",
                           (uintmax_t) child, error);
                    linted_error_string_free(error);
                }
                break;
            }

        case CLD_KILLED:
            fprintf(stderr, "child %ju was terminated with signal number %i\n",
                   (uintmax_t) child, child_info.si_status);
            break;

        default:
            /* TODO: Possibly assert that this shouldn't happen */
            fprintf(stderr, "child %ju executed abnormally\n",
                   (uintmax_t) child);
            break;
        }
    }
}

static void exec_task(linted_spawner_task task, int const fildes[])
{
    task(fildes);

    exit(EXIT_SUCCESS);
}
