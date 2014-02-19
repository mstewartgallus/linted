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
#include <dirent.h>
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
struct request_header {
    linted_spawner_task func;
    size_t fildes_count;
};

struct reply {
    int error_status;
};

static void exec_task(linted_spawner_task task, int const fildes[]);
static void exec_task_from_connection(linted_server_conn connection);

static int wait_on_children(id_t process_group);

int linted_spawner_pair(linted_spawner spawners[2])
{
    return linted_server_pair(spawners);
}

int linted_spawner_run(linted_spawner inbox,
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

            if (-1 == linted_io_close_fds_except(fildes, fildes_size)) {
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
                bytes_read = linted_io_recv_fildes(&connection, inbox);
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

                int fds_not_to_close[] = {
                    connection,
                    STDERR_FILENO
                };
                if (-1 == linted_io_close_fds_except(fds_not_to_close,
                                                     LINTED_ARRAY_SIZE(fds_not_to_close))) {
                    exit(errno);
                }

                exec_task_from_connection(connection);
            }

            case -1:{
                struct reply reply = {.error_status = errno };

                if (-1 == linted_io_write_all(connection, NULL,
                                              &reply, sizeof reply)) {
                    goto close_connection;
                }
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

int linted_spawner_close(linted_spawner spawner)
{
    return linted_server_close(spawner);
}

int linted_spawner_spawn(linted_spawner const spawner,
                         linted_spawner_task const func,
                         int const fildes_to_send[], size_t fildes_count)
{
    int spawn_status = -1;

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

#define MAX_FORKER_FILDES_COUNT 20
static void exec_task_from_connection(linted_server_conn connection)
{
    linted_spawner_task task;
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
        int sent_inboxes[MAX_FORKER_FILDES_COUNT];
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

            if (-1 == linted_io_write_all(connection, NULL,
                                          &reply, sizeof reply)) {
                exit(errno);
            }
        }

        if (-1 == linted_server_conn_close(connection)) {
            exit(errno);
        }

        exec_task(task, sent_inboxes);
    }

 reply_with_error:;
    int errnum = errno;
    struct reply reply = {.error_status = errnum };

    if (-1 == linted_io_write_all(connection, NULL, &reply, sizeof reply)) {
        exit(errno);
    }

    exit(errnum);
}

static void exec_task(linted_spawner_task task, int const fildes[])
{
    task(fildes);

    exit(EXIT_SUCCESS);
}
