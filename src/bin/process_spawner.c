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
 * TODO: Move away from Linux specifics such as prctl.
 */
static void exec_task(linted_spawner_task task, int const fildes[]);

int linted_process_spawner_run(linted_spawner inbox,
                               int const preserved_fildes[],
                               size_t preserved_fildes_size)
{
    int exit_status = -1;

    pid_t process_group = -1;

    for (;;) {
        int connection_status = -1;
        linted_server_conn connection;

        ssize_t bytes_read;
        do {
            bytes_read = linted_spawner_recv_future(inbox,
                                                    &connection);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            goto exit;
        }

        if (0 == bytes_read) {
            break;
        }

        /* Luckily, because we already must fork for a fork server
         * we get asynchronous behaviour for free.
         */
        pid_t child = fork();
        if (-1 == process_group) {
            /*
             * This is a little bit confusing but the first processes
             * pid is reused for the process group. We create a new
             * process group so we can wait on that only.
             */
            process_group = child;
        }
        switch (child) {
        case 0:
        {
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
                goto exit;
            }
        }
    }

    for (;;) {
        siginfo_t child_info;

        int wait_status;
        do {
            wait_status = waitid(P_PGID, process_group, &child_info, WEXITED);
        } while (-1 == wait_status && EINTR == errno);
        if (-1 == wait_status) {
            if (ECHILD == errno) {
                break;
            }
            goto exit;
        }

        pid_t const child = child_info.si_pid;

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

    exit_status = 0;

 exit:
    return exit_status;
}

static void exec_task(linted_spawner_task task, int const fildes[])
{
    task(fildes);

    exit(EXIT_SUCCESS);
}
