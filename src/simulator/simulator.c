/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

#include "linted/controller.h"
#include "linted/io.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define CONTROLLER_OPTION "--controller"
#define SHUTDOWNER_OPTION "--shutdowner"
#define UPDATER_OPTION "--updater"

struct controller_state {
    bool x_left:1;
    bool x_right:1;

    bool y_up:1;
    bool y_down:1;
};

struct simulator_state {
    int_fast32_t x_position;
    int_fast32_t y_position;

    int_fast32_t x_velocity;
    int_fast32_t y_velocity;

    bool update_pending:1;
};

static errno_t on_timer_readable(int timer,
                                 struct controller_state const
                                 *controller_state,
                                 struct simulator_state *simulator_state);

static errno_t on_updater_writeable(linted_updater updater,
                                    struct simulator_state *simulator_state);

static errno_t on_shutdowner_readable(linted_shutdowner shutdowner,
                                      bool * should_exit);

static errno_t on_controller_readable(linted_controller controller,
                                      struct controller_state
                                      *controller_state);

static int_fast32_t saturate(int_fast64_t x);
static int_fast32_t min(int_fast32_t x, int_fast32_t y);
static int_fast32_t sign(int_fast32_t x);

int main(int argc, char *argv[])
{
    if (argc < 1) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing process name\n",
                               PACKAGE_TARNAME "-simulator");
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;
    char const *bad_option = NULL;

    char const *controller_name = NULL;
    char const *shutdowner_name = NULL;
    char const *updater_name = NULL;
    for (unsigned ii = 1; ii < (unsigned)argc; ++ii) {
        char *argument = argv[ii];

        if (0 == strcmp(HELP_OPTION, argument)) {
            need_help = true;
        } else if (0 == strcmp(VERSION_OPTION, argument)) {
            need_version = true;
        } else if (0 == strncmp(argument, CONTROLLER_OPTION "=",
                                strlen(CONTROLLER_OPTION "="))) {

            controller_name = argument + strlen(CONTROLLER_OPTION "=");

        } else if (0 == strncmp(argument, SHUTDOWNER_OPTION "=",
                                strlen(SHUTDOWNER_OPTION "="))) {

            shutdowner_name = argument + strlen(SHUTDOWNER_OPTION "=");

        } else if (0 == strncmp(argument, UPDATER_OPTION "=",
                                strlen(UPDATER_OPTION "="))) {

            updater_name = argument + strlen(UPDATER_OPTION "=");

        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        linted_io_write_format(STDOUT_FILENO, NULL, "Usage: %s [OPTIONS]\n",
                               program_name);

        linted_io_write_format(STDOUT_FILENO, NULL,
                               "Run the %s program simulator.\n", PACKAGE_NAME);

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --controller        the controller message queue file descriptor\n\
  --updater           the updater message queue file descriptor\n\
  --shutdowner        the shutdowner message queue file descriptor\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_format(STDOUT_FILENO, NULL, "Report bugs to <%s>\n",
                               PACKAGE_BUGREPORT);
        linted_io_write_format(STDOUT_FILENO, NULL, "%s home page: <%s>\n",
                               PACKAGE_NAME, PACKAGE_URL);

        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: urecognized option '%s'\n",
                               program_name, bad_option);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_STRING));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n\n"));

        linted_io_write_format(STDOUT_FILENO, NULL, "\
Copyright (C) %s Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n", COPYRIGHT_YEAR);

        return EXIT_SUCCESS;
    }

    if (NULL == controller_name) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: missing %s option\n",
                               program_name, CONTROLLER_OPTION);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    if (NULL == shutdowner_name) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: missing %s option\n",
                               program_name, SHUTDOWNER_OPTION);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    if (NULL == updater_name) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: missing %s option\n",
                               program_name, UPDATER_OPTION);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    linted_controller controller;
    {
        int fd;
        int errnum = linted_io_strtofd(controller_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   CONTROLLER_OPTION,
                                   linted_error_string_alloc(errnum));
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s %s' for more information.\n",
                                   program_name, HELP_OPTION);
            return EXIT_FAILURE;
        }
        controller = fd;
    }

    linted_shutdowner shutdowner;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(shutdowner_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   SHUTDOWNER_OPTION,
                                   linted_error_string_alloc(errnum));
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s %s' for more information.\n",
                                   program_name, HELP_OPTION);
            return EXIT_FAILURE;
        }
        shutdowner = fd;
    }

    linted_updater updater;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(updater_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   UPDATER_OPTION,
                                   linted_error_string_alloc(errnum));
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s %s' for more information.\n",
                                   program_name, HELP_OPTION);
            return EXIT_FAILURE;
        }
        updater = fd;
    }

    fcntl(updater, F_SETFD, fcntl(updater, F_GETFD) | FD_CLOEXEC);
    fcntl(shutdowner, F_SETFD, fcntl(shutdowner, F_GETFD) | FD_CLOEXEC);
    fcntl(controller, F_SETFD, fcntl(controller, F_GETFD) | FD_CLOEXEC);

    {
        fd_set essential_fds;
        FD_ZERO(&essential_fds);

        FD_SET(STDERR_FILENO, &essential_fds);
        FD_SET(fileno(stdin), &essential_fds);
        FD_SET(fileno(stdout), &essential_fds);

        FD_SET(controller, &essential_fds);
        FD_SET(updater, &essential_fds);
        FD_SET(shutdowner, &essential_fds);

        errno_t errnum = linted_util_sanitize_environment(&essential_fds);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s", program_name, linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
    }

    errno_t error_status = 0;

    struct controller_state controller_state = {
        .x_left = 0,
        .x_right = 0,

        .y_up = 0,
        .y_down = 0
    };

    struct simulator_state simulator_state = {
        .update_pending = false,

        .x_position = 0,
        .y_position = 0,

        .x_velocity = 0,
        .y_velocity = 0
    };

    int timer = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (-1 == timer) {
        error_status = errno;
        goto exit;
    }

    {
        struct itimerspec itimer_spec;
        memset(&itimer_spec, 0, sizeof itimer_spec);

        long const second = 1000000000;

        /* Strangely, it_value has to be nozero */
        itimer_spec.it_value.tv_nsec = 1;

        itimer_spec.it_interval.tv_sec = 0;
        itimer_spec.it_interval.tv_nsec = second / 60;

        if (-1 == timerfd_settime(timer, 0, &itimer_spec, NULL)) {
            error_status = errno;
            goto close_timer;
        }
    }

    for (;;) {
        enum {
            SHUTDOWNER,
            TIMER,
            CONTROLLER,

            UPDATER
        };
        size_t fds_size;
        struct pollfd * fds;

        struct pollfd fds_with_updater[] = {
            [SHUTDOWNER] = {.fd = shutdowner,.events = POLLIN},
            [TIMER] = {.fd = timer,.events = POLLIN},
            [CONTROLLER] = {.fd = controller,.events = POLLIN},

            [UPDATER] = {.fd = updater,.events = POLLOUT},
        };

        struct pollfd fds_without_updater[] = {
            [SHUTDOWNER] = {.fd = shutdowner,.events = POLLIN},
            [TIMER] = {.fd = timer,.events = POLLIN},
            [CONTROLLER] = {.fd = controller,.events = POLLIN}
        };

        if (simulator_state.update_pending) {
            fds = fds_with_updater;
            fds_size = LINTED_ARRAY_SIZE(fds_with_updater);
        } else {
            fds = fds_without_updater;
            fds_size = LINTED_ARRAY_SIZE(fds_without_updater);
        }

        errno_t poll_status;
        do {
            int fds_active = poll(fds, fds_size, -1);
            poll_status = -1 == fds_active ? errno : 0;
        } while (EINTR == poll_status);
        if (poll_status != 0) {
            error_status = poll_status;
            goto close_timer;
        }

        if ((fds[SHUTDOWNER].revents & POLLIN) != 0) {
            bool should_exit;
            errno_t errnum = on_shutdowner_readable(shutdowner, &should_exit);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
            if (should_exit) {
                goto exit_main_loop;
            }
        }

        if ((fds[TIMER].revents & POLLIN) != 0) {
            errno_t errnum = on_timer_readable(timer, &controller_state,
                                               &simulator_state);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
        }

        if ((fds[CONTROLLER].revents & POLLIN) != 0) {
            errno_t errnum = on_controller_readable(controller,
                                                    &controller_state);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
        }

        if (simulator_state.update_pending
            && (fds[UPDATER].revents & POLLOUT) != 0) {
            errno_t errnum = on_updater_writeable(updater, &simulator_state);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
        }
    }

 exit_main_loop:

 close_timer:
    {
        errno_t errnum = linted_io_close(timer);
        if (0 == error_status) {
            assert(errnum != EBADF);

            error_status = errnum;
        }
    }

 exit:
    return error_status;
}

static errno_t on_timer_readable(int timer,
                                 struct controller_state const
                                 *controller_state,
                                 struct simulator_state *simulator_state)
{
    uint64_t ticks;
    if (-1 == read(timer, &ticks, sizeof ticks)) {
        return errno;
    }

    for (size_t ii = 0; ii < ticks; ++ii) {
        int_fast32_t x_position = simulator_state->x_position;
        int_fast32_t y_position = simulator_state->y_position;

        int_fast32_t x_velocity = simulator_state->x_velocity;
        int_fast32_t y_velocity = simulator_state->y_velocity;

        int_fast32_t x_thrust = 2 * ((int_fast32_t) controller_state->x_right
                                     - (int_fast32_t) controller_state->x_left);
        int_fast32_t y_thrust = 2 * ((int_fast32_t) controller_state->y_up
                                     - (int_fast32_t) controller_state->y_down);

        int_fast32_t guess_x_velocity =
            saturate((int_fast64_t) x_thrust + x_velocity);
        int_fast32_t guess_y_velocity =
            saturate((int_fast64_t) y_thrust + y_velocity);

        int_fast32_t x_friction = min(imaxabs(guess_x_velocity), 1)
            * sign(guess_x_velocity);
        int_fast32_t y_friction = min(imaxabs(guess_y_velocity), 1)
            * sign(guess_y_velocity);

        int_fast32_t new_x_velocity =
            saturate((int_fast64_t) guess_x_velocity + x_friction);
        int_fast32_t new_y_velocity =
            saturate((int_fast64_t) guess_y_velocity + y_friction);

        int_fast32_t new_x_position =
            saturate((int_fast64_t) x_position + new_x_velocity);
        int_fast32_t new_y_position =
            saturate((int_fast64_t) y_position + new_y_velocity);

        simulator_state->update_pending |= x_position != new_x_position
            || y_position != new_y_position;

        simulator_state->x_position = new_x_position;
        simulator_state->y_position = new_y_position;

        simulator_state->x_velocity = new_x_velocity;
        simulator_state->y_velocity = new_y_velocity;
    }

    return 0;
}

static errno_t on_updater_writeable(linted_updater updater,
                                    struct simulator_state *simulator_state)
{
    struct linted_updater_update update = {
        .x_position = simulator_state->x_position,
        .y_position = simulator_state->y_position
    };

    errno_t update_status;
    do {
        update_status = linted_updater_send_update(updater, &update);
    } while (EINTR == update_status);

    if (EAGAIN == update_status) {
        return 0;
    }

    if (update_status != 0) {
        return update_status;
    }

    simulator_state->update_pending = false;

    return 0;
}

static errno_t on_shutdowner_readable(linted_shutdowner shutdowner,
                                      bool * should_exit)
{
    errno_t read_status;
    do {
        read_status = linted_shutdowner_receive(shutdowner);
    } while (EINTR == read_status);

    if (EAGAIN == read_status) {
        *should_exit = false;
        return 0;
    }

    if (read_status != 0) {
        return read_status;
    }

    *should_exit = true;
    return 0;
}

static errno_t on_controller_readable(linted_controller controller,
                                      struct controller_state *controller_state)
{
    struct linted_controller_message message;

    errno_t read_status;
    do {
        read_status = linted_controller_receive(controller, &message);
    } while (EINTR == read_status);

    if (EAGAIN == read_status) {
        return 0;
    }

    if (read_status != 0) {
        return read_status;
    }

    controller_state->x_left = message.left;
    controller_state->x_right = message.right;
    controller_state->y_up = message.up;
    controller_state->y_down = message.down;

    return 0;
}

static int_fast32_t min(int_fast32_t x, int_fast32_t y)
{
    return x < y ? x : y;
}

static int_fast32_t sign(int_fast32_t x)
{
    return x < 0 ? 1 : 0 == x ? 0 : -1;
}

static int_fast32_t saturate(int_fast64_t x)
{
    if (x > INT32_MAX) {
        return INT32_MAX;
    }

    if (x < INT32_MIN) {
        return INT32_MIN;
    }

    return x;
}
