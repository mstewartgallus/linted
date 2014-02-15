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
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
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

struct reply_data {
    int error_status;
};

static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[]);
static void exec_task_from_connection(linted_spawner_t spawner,
                                      linted_server_conn_t connection);
static void on_sigchld(int signal_number);


/**
 * The read_all function repeatedly reads from fd until buf is full or
 * an error occurs (except for EINTR).
 *
 * The read may be succesful and read less than count if the end of file is
 * reached.
 *
 * For example, a bit could be read and then fd could
 * be closed and an error would be returned but some bytes would still
 * have been read.
 *
 * @param fd The file to be read from.
 *
 * @param bytes_read The amount read. Is set to no larger than count
 *                   bytes and at least zero bytes. If NULL then is
 *                   not set.
 *
 * @param buf The buffer to be read into. Must be at least count bytes
 *            long.
 *
 * @param count The amount to read. No more than these many bytes is
 *              read.
 *
 * @returns Zero on success. -1 on error, and errno is set
 *          appropriately.
 *
 * @error EAGAIN The file descriptor has been marked nonblocking and
 *               one of the reads would block.
 *
 * @error EWOULDBLOCK Means the same as EAGAIN but may be a different
 *                    value.
 *
 * @error EBADF fd is not a valid file descriptor or is not open for
 *              reading.
 *
 * @error EFAULT buf is not accessible.
 *
 * @error EINVAL fd is not readable or the file was opened with
 *               O_DIRECT and alignment restrictions weren't
 *               satisfied.
 *
 * @error EIO I/O error.
 *
 * @error EISDIR fd is a directory.
 */
static int read_all(int fd, size_t * bytes_read, void *buf, size_t count);

/**
 * The write_all function repeatedly writes to fd until of buf is
 * written or an error occurs (except for EINTR).
 *
 * For example, a bit could be written and then fd could be closed and
 * an error would be returned but some bytes would still have been
 * written.
 *
 * @param fd The file to be written to.
 *
 * @param bytes_wrote The amount written. Is set to no larger than
 *                    count bytes and at least zero bytes. If NULL
 *                    then is not set.
 *
 * @param buf The buffer to be written from. Must be at least count bytes
 *            long.
 *
 * @param count The amount to write. No more than these many bytes is
 *              written.
 *
 * @returns Zero on success. -1 on error, and errno is set
 *          appropriately.
 *
 * @error EAGAIN The file descriptor has been marked nonblocking and
 *               one of the writes would block.
 *
 * @error EBADF fd is not a valid file descriptor or is not open for
 *              writing.
 *
 * @error EWOULDBLOCK Means the same as EAGAIN but may be a different
 *                    value.
 *
 * @error EFAULT buf is not accessible.
 *
 * @error EINVAL fd is not writable or the file was opened with
 *               O_DIRECT and alignment restrictions weren't
 *               satisfied.
 *
 * @error EIO I/O error.
 *
 * @error ENOSPC No room for the data.
 *
 * @error EPIPE fd is a pipe or socket with a closed read end.
 *
 * @error EISDIR fd is a directory.
 */
static int write_all(int fd, size_t * bytes_wrote,
                     void const *buf, size_t count);

static jmp_buf sigchld_jump_buffer;

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

    struct sigaction old_action;
    if (0 == sigsetjmp(sigchld_jump_buffer, true)) {
        struct sigaction new_action;
        memset(&new_action, 0, sizeof new_action);

        new_action.sa_handler = on_sigchld;
        new_action.sa_flags = SA_NOCLDSTOP;

        int const sigset_status = sigaction(SIGCHLD, &new_action, &old_action);
        assert(sigset_status != -1);
    }
    /* We received a SIGCHLD */
    /* Alternatively, we are doing this once at the start */
 retry_wait:
    switch (waitpid(-1, NULL, WNOHANG)) {
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

    default:
        /* Waited on a dead process. Wait for more. */
        goto retry_wait;
    }

    for (;;) {
        int connection_status = -1;
        linted_server_conn_t connection;
        ssize_t bytes_read;
        do {
            bytes_read = linted_fildes_recv(&connection, inbox);
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
                struct reply_data reply = {.error_status = errno };

                if (-1 == write_all(connection, NULL, &reply, sizeof reply)) {
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

        if (-1 == write_all(connection, NULL,
                            &request_header, sizeof request_header)) {
            goto cleanup_connection;
        }
    }

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int send_status;
        do {
            send_status = linted_fildes_send(connection, fildes_to_send[ii]);
        } while (-1 == send_status && EINTR == errno);
        if (-1 == send_status) {
            goto cleanup_connection;
        }
    }

    {
        struct reply_data reply_data;
        size_t bytes_read;
        if (-1 == read_all(connection, &bytes_read,
                           &reply_data, sizeof reply_data)) {
            goto cleanup_connection;
        }
        if (bytes_read != sizeof reply_data) {
            /* The connection hung up on us instead of replying */
            errno = EIO;
            goto cleanup_connection;
        }

        int const reply_error_status = reply_data.error_status;
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
    siglongjmp(sigchld_jump_buffer, signal_number);
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
        if (-1 == read_all(connection, &bytes_read,
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

            if (-1 == write_all(connection, NULL, &reply, sizeof reply)) {
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
    struct reply_data reply = {.error_status = errno };

    if (-1 == write_all(connection, NULL, &reply, sizeof reply)) {
        LINTED_LAZY_DEV_ERROR
            ("Fork server could not reply to request: %s",
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

static int read_all(int fd, size_t * bytes_read_out, void *buf, size_t count)
{
    int exit_status = -1;
    size_t total_bytes_read = 0;

    do {
        ssize_t bytes_read = read(fd, (char *) buf + total_bytes_read,
                                  count - total_bytes_read);
        if (-1 == bytes_read) {
            if (EINTR == errno) {
                continue;
            }

            goto output_bytes_read;
        }

        if (0 == bytes_read) {
            /* File empty or pipe hangup */
            exit_status = 0;
            goto output_bytes_read;
        }

        total_bytes_read += bytes_read;
    } while (total_bytes_read != count);

    exit_status = 0;

 output_bytes_read:
    if (bytes_read_out != NULL) {
        *bytes_read_out = total_bytes_read;
    }
    return exit_status;
}

static int write_all(int fd, size_t * bytes_wrote_out,
                     void const *buf, size_t count)

{
    int exit_status = -1;
    size_t total_bytes_wrote = 0;

    do {
        ssize_t bytes_wrote = write(fd, (char const *) buf + total_bytes_wrote,
                                    count - total_bytes_wrote);
        if (-1 == bytes_wrote) {
            if (EINTR == errno) {
                continue;
            }

            goto output_bytes_wrote;
        }

        total_bytes_wrote += bytes_wrote;
    } while (total_bytes_wrote != count);

    exit_status = 0;

 output_bytes_wrote:
     if (bytes_wrote_out != NULL) {
        *bytes_wrote_out = total_bytes_wrote;
    }
    return exit_status;
}
