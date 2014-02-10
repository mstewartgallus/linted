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

#include "linted/fildes.h"
#include "linted/server.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
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
 * TODO: Don't wait on all processes but only on one's spawned by the
 * fork server.
 *
 * TODO: Don't modify the global SIGCHLD signal handler.
 */

struct request_header {
    linted_spawner_task_t func;
    size_t fildes_count;
};

struct reply_data {
    int error_status;
};

static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[]);
static void exec_task_from_connection(linted_spawner_t spawner,
                                      linted_server_conn_t connection);
static void on_sigchld(int signal_number);

static int echild_write;

int linted_spawner_run(linted_spawner_task_t main_loop, int const fildes[])
{
    int echild_fds[2];

    if (-1 == pipe(echild_fds)) {
        return -1;
    }
    echild_write = echild_fds[1];
    int const echild_read = echild_fds[0];

    struct sigaction new_action;
    memset(&new_action, 0, sizeof new_action);

    new_action.sa_handler = on_sigchld;
    new_action.sa_flags = SA_NOCLDSTOP;

    struct sigaction old_action;
    int const sigset_status = sigaction(SIGCHLD, &new_action, &old_action);
    assert(sigset_status != -1);

    linted_server_t sockets[2];
    if (-1 == linted_server(sockets)) {
        goto exit_with_error_and_close_pipes;
    }

    {
        linted_spawner_t const spawner = sockets[0];
        linted_server_t const inbox = sockets[1];

        switch (fork()) {
        case 0:
        {
            int const sigrestore_status = sigaction(SIGCHLD, &old_action, NULL);
            assert(sigrestore_status != -1);
        }

        if (-1 == close(echild_read)) {
            LINTED_ERROR("Could not close echild read end of pipe: %s",
                         linted_error_string_alloc(errno));
        }

        if (-1 == close(echild_write)) {
            LINTED_ERROR("Could not close echild write end of pipe: %s",
                         linted_error_string_alloc(errno));
        }

        if (-1 == linted_server_close(inbox)) {
            LINTED_ERROR("Could not close inbox: %s",
                         linted_error_string_alloc(errno));
        }

        exec_task(main_loop, spawner, fildes);

        case -1:
            goto exit_with_error_and_close_sockets;
        }

        for (;;) {
            struct pollfd watched_fds[] = {
                (struct pollfd) { .fd = echild_read, .events = POLLIN, .revents = 0 },
                (struct pollfd) { .fd = inbox, .events = POLLIN, .revents = 0 }
            };

            int fds_active;
            do {
                fds_active = poll(watched_fds, LINTED_ARRAY_SIZE(watched_fds), -1);
            } while (-1 == fds_active && EINTR == errno);
            if (-1 == fds_active) {
                goto exit_with_error_and_close_sockets;
            }

            if ((watched_fds[0].revents & POLLIN) != 0) {
                goto exit_fork_server;
            }


            if (!((watched_fds[1].revents & POLLIN) != 0)) {
                continue;
            }

            linted_server_conn_t connection;
            ssize_t bytes_read;
            do {
                bytes_read = linted_fildes_recv(&connection, inbox);
            } while (-1 == bytes_read && EINTR == errno);
            if (-1 == bytes_read) {
                goto exit_with_error_and_close_sockets;
            }

            /* Luckily, because we already must fork for a fork server we
             * get asynchronous behaviour for free.
             */
            switch (fork()) {
            case 0:
            {
                int const sigrestore_status = sigaction(SIGCHLD, &old_action, NULL);
                assert(sigrestore_status != -1);
            }

            if (-1 == close(echild_read)) {
                LINTED_ERROR("Could not close echild read end of pipe: %s",
                             linted_error_string_alloc(errno));
            }

            if (-1 == close(echild_write)) {
                LINTED_ERROR("Could not close echild write end of pipe: %s",
                             linted_error_string_alloc(errno));
            }

            if (-1 == linted_server_close(inbox)) {
                LINTED_ERROR("Could not close inbox: %s",
                             linted_error_string_alloc(errno));
            }

            exec_task_from_connection(spawner, connection);

            case -1:{
                struct reply_data reply = {.error_status = errno };

                int write_status;
                do {
                    write_status = write(connection, &reply, sizeof reply);
                } while (-1 == write_status && EINTR == errno);
                if (-1 == write_status) {
                    goto exit_with_error_and_close_sockets;
                }
            }
            }

            if (-1 == linted_server_conn_close(connection)) {
                goto exit_with_error_and_close_sockets;
            }
        }

    exit_fork_server:

        if (-1 == linted_server_close(inbox)) {
            goto exit_with_error_and_close_spawner;
        }

        if (-1 == linted_spawner_close(spawner)) {
            goto exit_with_error_and_close_pipes;
        }

        if (-1 == close(echild_write)) {
            goto exit_with_error_and_close_read_pipe;
        }

        if (-1 == close(echild_read)) {
            goto exit_with_error;
        }

        return 0;

     exit_with_error_and_close_sockets:
        {
            int errnum = errno;
            linted_server_close(inbox);
            errno = errnum;
        }

     exit_with_error_and_close_spawner:
        {
            int errnum = errno;
            linted_spawner_close(spawner);
            errno = errnum;
        }
    }

 exit_with_error_and_close_pipes:
    {
        int errnum = errno;
        close(echild_write);
        errno = errnum;
    }

 exit_with_error_and_close_read_pipe:
    {
        int errnum = errno;
        close(echild_read);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}

int linted_spawner_close(linted_spawner_t spawner)
{
    return linted_server_close(spawner);
}

int linted_spawner_spawn(linted_spawner_t const spawner,
                         linted_spawner_task_t const func, int const fildes_to_send[])
{
    size_t fildes_count = 0;
    for (; fildes_to_send[fildes_count] != -1; ++fildes_count) {
        /* Do nothing */
    }

    int const connection = linted_server_connect(spawner);
    if (-1 == connection) {
        goto exit_with_error;
    }

    {
        struct request_header request_header = {
            .func = func,
            .fildes_count = fildes_count
        };

        ssize_t bytes_sent;
        do {
            bytes_sent = send(connection, &request_header, sizeof request_header, 0);
        } while (-1 == bytes_sent && EINTR == errno);
        if (-1 == bytes_sent) {
            goto exit_with_error_and_close_connection;
        }
    }

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int send_status;
        do {
            send_status = linted_fildes_send(connection, fildes_to_send[ii]);
        } while (-1 == send_status && EINTR == errno);
        if (-1 == send_status) {
            goto exit_with_error_and_close_connection;
        }
    }

    {
        struct reply_data reply_data;
        ssize_t bytes_read;
        do {
            bytes_read = read(connection, &reply_data, sizeof reply_data);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            goto exit_with_error_and_close_connection;
        }

        int const reply_error_status = reply_data.error_status;
        if (reply_error_status != 0) {
            errno = reply_error_status;
            goto exit_with_error_and_close_connection;
        }
    }

    if (-1 == linted_server_conn_close(connection)) {
        goto exit_with_error;
    }

    return 0;

 exit_with_error_and_close_connection:
    {
        int errnum = errno;
        linted_server_conn_close(connection);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}


static void on_sigchld(int signal_number)
{
    int const old_errnum = errno;
retry_wait:
    switch (waitpid(-1, NULL, WNOHANG)) {
    case -1:
        switch (errno) {
        case ECHILD:{
            /* No more proceses to wait on */
            char dummy = 0;

            ssize_t write_status;
            do {
                write_status = write(echild_write, &dummy, sizeof dummy);
            } while (-1 == write_status && EINTR == errno);
            if (-1 == write_status) {
                /* TOOO: Find a signal safe way of erroring out */
                assert(false);
            }
            break;
        }

        case EINTR:
            /* Implausible but some weird system might do this */
            goto retry_wait;

        default:
            /* TOOO: Find a signal safe way of erroring out */
            assert(false);
        }

    case 0:
        /* Have processes that aren't dead yet to wait on, don't exit
         * yet.
         */
        break;

    default:
        /* Waited on a dead process. Wait for more. */
        goto retry_wait;
    }

    /* Suppose, a system call fails and leaves an error in
     * errno. Suppose this signal handler is run before the value in
     * errno is checked. Then this signal handler could clobber the
     * old value of errno. So restore the old value of it here.
     */
    errno = old_errnum;
}

static void exec_task_from_connection(linted_spawner_t const spawner,
                                      linted_server_conn_t connection)
{
    linted_spawner_task_t task;
    size_t fildes_count;
    {
        struct request_header request_header;

        ssize_t bytes_received;
        do {
            bytes_received = recv(connection,
                                  &request_header, sizeof request_header, MSG_WAITALL);
        } while (-1 == bytes_received && EINTR == errno);
        if (-1 == bytes_received) {
            goto reply_with_error;
        }

        task = request_header.func;
        fildes_count = request_header.fildes_count;
    }

    if (fildes_count > 20) {
        errno = EINVAL;
        goto reply_with_error;
    }

    {
        int sent_inboxes[fildes_count + 1];
        sent_inboxes[fildes_count] = -1;
        for (size_t ii = 0; ii < fildes_count; ++ii) {
            int fildes;
            ssize_t bytes_read;

            do {
                bytes_read = linted_fildes_recv(&fildes, connection);
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
            struct reply_data reply = {.error_status = 0 };

            int write_status;
            do {
                write_status = write(connection, &reply, sizeof reply);
            } while (-1 == write_status && EINTR == errno);
            if (-1 == write_status) {
                LINTED_ERROR("Fork server could not reply to request: %s",
                             linted_error_string_alloc(errno));
            }
        }

        if (-1 == linted_server_conn_close(connection)) {
            LINTED_ERROR("Forked child could not close connection: %s",
                         linted_error_string_alloc(errno));
        }

        exec_task(task, spawner, sent_inboxes);
    }

 reply_with_error:;
    struct reply_data reply = {.error_status = errno };

    int write_status;
    do {
        write_status = write(connection, &reply, sizeof reply);
    } while (-1 == write_status && EINTR == errno);
    if (-1 == write_status) {
        LINTED_ERROR("Fork server could not reply to request: %s",
                     linted_error_string_alloc(errno));
    }

    exit(EXIT_FAILURE);
}

static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[])
{
    exit(task(spawner, fildes));
}
