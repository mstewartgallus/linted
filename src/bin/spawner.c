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
#include "linted/server.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
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
 * TODO: Don't wait on all processes but only on ones spawned by the
 * fork server.
 *
 * TODO: Don't modify the global SIGCHLD signal handler.
 *
 * TODO: Don't have a global state to use with the signal handler.
 */

struct request_header {
    linted_spawner_task_t func;
    size_t fildes_count;
};

struct reply {
    int error_status;
};

static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[]);
static void exec_task_from_connection(linted_spawner_t spawner,
                                      linted_server_conn_t connection);
static void on_sigchld(int signal_number);

static volatile sig_atomic_t sigchld_was_set;

int linted_spawner_run(linted_spawner_task_t main_loop, int const fildes[])
{
    int exit_status = -1;

    linted_server_t sockets[2];
    if (-1 == linted_server(sockets)) {
        return -1;
    }

    linted_spawner_t const spawner = sockets[0];
    linted_server_t const inbox = sockets[1];

    switch (fork()) {
    case 0:
        if (-1 == linted_server_close(inbox)) {
            LINTED_LAZY_DEV_ERROR("Could not close inbox: %s",
                                  linted_error_string_alloc(errno));
        }

        exec_task(main_loop, spawner, fildes);

    case -1:
        goto close_sockets;
    }

    sigset_t old_sigset;
    {
        sigset_t sigchld_set;

        int empty_set_status = sigemptyset(&sigchld_set);
        assert(empty_set_status != -1);

        int sigchld_set_status = sigaddset(&sigchld_set, SIGCHLD);
        assert(sigchld_set_status != -1);

        int mask_status = sigprocmask(SIG_BLOCK, &sigchld_set, &old_sigset);
        assert(mask_status != -1);
    }

    struct sigaction old_action;
    {
        sigset_t during_sigchld_mask;

        int fullset_status = sigemptyset(&during_sigchld_mask);
        assert(fullset_status != -1);

        struct sigaction new_action;
        memset(&new_action, 0, sizeof new_action);

        new_action.sa_handler = on_sigchld;
        new_action.sa_flags = SA_NOCLDSTOP;
        new_action.sa_mask = during_sigchld_mask;

        int const sigset_status = sigaction(SIGCHLD, &new_action, &old_action);
        assert(sigset_status != -1);
    }

    /*
     * We received a SIGCHLD. Alternatively, we are doing this once at
     * the start.
     */
 retry_wait:
    sigchld_was_set = false;

    {
        int child_exit_status;
        pid_t child = waitpid(-1, &child_exit_status, WNOHANG);
        switch (child) {
        case -1:
            switch (errno) {
            case ECHILD:
                goto exit_fork_server;

            case EINTR:
                /* Implausible but some weird system might do this */
                goto retry_wait;

            default:
                goto restore_sigstatus;
            }

        case 0:
            /* Have processes that aren't dead yet to wait on,
             * don't exit yet.
             */
            break;

        default:{
                /* Waited on a dead process. Wait for more. */

                if (WIFEXITED(child_exit_status)) {
                    int retvalue = WEXITSTATUS(child_exit_status);
                    if (0 == retvalue) {
                        syslog(LOG_INFO,
                               "child %ju executed normally",
                               (uintmax_t) child);
                    } else {
                        char const * error = linted_error_string_alloc(retvalue);
                        syslog(LOG_INFO,
                               "child %ju executed with error: %s",
                               (uintmax_t) child, error);
                        linted_error_string_free(error);
                    }
                } else if (WIFSIGNALED(child_exit_status)) {
                    syslog(LOG_INFO,
                           "child %ju was terminated with signal number %i",
                           (uintmax_t) child, WTERMSIG(child_exit_status));
                } else {
                    syslog(LOG_INFO,"child %ju executed abnormally",
                           (uintmax_t) child);
                }
                goto retry_wait;
            }
        }
    }

    for (;;) {
        int connection_status = -1;
        linted_server_conn_t connection;

        /*
         * Done dealing with SIGCHLD signals. Now we can unblock the mask
         * and wait for more (and do other stuff).
         */
        {
         retry_pselect:;
            sigset_t sigchld_unblocked_mask = old_sigset;

            int sigchld_rm_status = sigdelset(&sigchld_unblocked_mask, SIGCHLD);
            assert(sigchld_rm_status != -1);

            fd_set watched_fds;

            FD_ZERO(&watched_fds);
            FD_SET(inbox, &watched_fds);

            int poll_status = pselect(inbox + 1, &watched_fds,
                                      NULL,
                                      NULL,
                                      NULL,
                                      &sigchld_unblocked_mask);
            if (-1 == poll_status) {
                if (EINTR == errno) {
                    if (sigchld_was_set) {
                        goto retry_wait;
                    }
                    goto retry_pselect;
                }

                goto restore_sigstatus;
            }
        }

        ssize_t bytes_read;
        do {
            bytes_read = linted_io_recv_fildes(&connection, inbox);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            goto restore_sigstatus;
        }

        /* Luckily, because we already must fork for a fork server we
         * get asynchronous behaviour for free.
         */
        switch (fork()) {
        case 0:
            {
                int const sigrestore_status = sigaction(SIGCHLD, &old_action,
                                                        NULL);
                assert(sigrestore_status != -1);

                if (-1 == linted_server_close(inbox)) {
                    LINTED_LAZY_DEV_ERROR("Could not close inbox: %s",
                                          linted_error_string_alloc(errno));
                }

                exec_task_from_connection(spawner, connection);
            }

        case -1:{
                struct reply reply = {.error_status = errno };

                if (-1 == linted_io_write_all(connection, NULL,
                                              &reply, sizeof reply)) {
                    goto close_connection;
                }
            }
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
                goto restore_sigstatus;
            }
        }
    }

 exit_fork_server:
    exit_status = 0;

 restore_sigstatus:
    {
        int errnum = errno;

        int const sigrestore_status = sigaction(SIGCHLD, &old_action, NULL);
        assert(sigrestore_status != -1);

        if (-1 == exit_status) {
            errno = errnum;
        }
    }

    {
        int mask_status = sigprocmask(SIG_SETMASK, &old_sigset, NULL);
        assert(mask_status != -1);
    }

 close_sockets:
    {
        int errnum = errno;

        int const close_status = linted_server_close(inbox);
        if (-1 == exit_status) {
            errno = errnum;
        }

        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;

        int close_status = linted_spawner_close(spawner);
        if (-1 == exit_status) {
            errno = errnum;
        }

        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}

int linted_spawner_close(linted_spawner_t spawner)
{
    return linted_server_close(spawner);
}

int linted_spawner_spawn(linted_spawner_t const spawner,
                         linted_spawner_task_t const func,
                         int const fildes_to_send[])
{
    int spawn_status = -1;

    size_t fildes_count = 0;
    for (; fildes_to_send[fildes_count] != -1; ++fildes_count) {
        /* Do nothing */
    }

    int const connection = linted_server_connect(spawner);
    if (-1 == connection) {
        goto cleanup_nothing;
    }

    {
        struct request_header request_header = {
            .func = func,
            .fildes_count = fildes_count
        };

        if (-1 == linted_io_write_all(connection, NULL,
                                      &request_header, sizeof request_header)) {
            goto cleanup_connection;
        }
    }

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int send_status;
        do {
            send_status = linted_io_send_fildes(connection, fildes_to_send[ii]);
        } while (-1 == send_status && EINTR == errno);
        if (-1 == send_status) {
            goto cleanup_connection;
        }
    }

    {
        struct reply reply;
        size_t bytes_read;
        if (-1 == linted_io_read_all(connection, &bytes_read,
                                     &reply, sizeof reply)) {
            goto cleanup_connection;
        }
        if (bytes_read != sizeof reply) {
            /* The connection hung up on us instead of replying */
            errno = EIO;
            goto cleanup_connection;
        }

        int const reply_error_status = reply.error_status;
        if (reply_error_status != 0) {
            errno = reply_error_status;
            goto cleanup_connection;
        }
    }

    spawn_status = 0;

 cleanup_connection:
    {
        int errnum = errno;

        int close_status = linted_server_conn_close(connection);

        if (-1 == spawn_status) {
            errno = errnum;
        }

        if (-1 == close_status) {
            spawn_status = -1;
        }
    }

 cleanup_nothing:
    return spawn_status;
}

static void on_sigchld(int signal_number)
{
    sigchld_was_set = true;
}

#define MAX_FORKER_FILDES_COUNT 20
static void exec_task_from_connection(linted_spawner_t const spawner,
                                      linted_server_conn_t connection)
{
    linted_spawner_task_t task;
    size_t fildes_count;
    {
        struct request_header request_header;
        size_t bytes_read;
        if (-1 == linted_io_read_all(connection, &bytes_read,
                                     &request_header, sizeof request_header)) {
            goto reply_with_error;
        }
        if (bytes_read != sizeof request_header) {
            /* The connection hung up on us instead of replying */
            errno = EINVAL;
            goto reply_with_error;
        }

        task = request_header.func;
        fildes_count = request_header.fildes_count;
    }

    if (fildes_count > MAX_FORKER_FILDES_COUNT) {
        errno = EINVAL;
        goto reply_with_error;
    }

    {
        int sent_inboxes[MAX_FORKER_FILDES_COUNT + 1];
        sent_inboxes[fildes_count] = -1;
        for (size_t ii = 0; ii < fildes_count; ++ii) {
            int fildes;
            ssize_t bytes_read;

            do {
                bytes_read = linted_io_recv_fildes(&fildes, connection);
            } while (-1 == bytes_read && EINTR == errno);
            switch (bytes_read) {
            case -1:
                goto reply_with_error;

            case 0:
                errno = EINVAL;
                goto reply_with_error;
            }

            sent_inboxes[ii] = fildes;
        }

        {
            struct reply reply = {.error_status = 0 };

            if (-1 ==
                linted_io_write_all(connection, NULL, &reply, sizeof reply)) {
                LINTED_LAZY_DEV_ERROR
                    ("Fork server could not reply to request: %s",
                     linted_error_string_alloc(errno));
            }
        }

        if (-1 == linted_server_conn_close(connection)) {
            LINTED_LAZY_DEV_ERROR("Forked child could not close connection: %s",
                                  linted_error_string_alloc(errno));
        }

        exec_task(task, spawner, sent_inboxes);
    }

 reply_with_error:;
    struct reply reply = {.error_status = errno };

    if (-1 == linted_io_write_all(connection, NULL, &reply, sizeof reply)) {
        LINTED_LAZY_DEV_ERROR("Fork server could not reply to request: %s",
                              linted_error_string_alloc(errno));
    }

    exit(EXIT_FAILURE);
}

static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[])
{
    task(spawner, fildes);

    exit(EXIT_SUCCESS);
}
