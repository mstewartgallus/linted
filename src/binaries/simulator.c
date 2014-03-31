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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
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

static int on_timer_readable(int timer,
                             struct controller_state const *controller_state,
                             struct simulator_state *simulator_state);

static int on_updater_writeable(linted_updater updater,
                                struct simulator_state *simulator_state);

static int on_shutdowner_readable(linted_shutdowner shutdowner,
                                  bool * should_exit);

static int on_controller_readable(linted_controller controller,
                                  struct controller_state *controller_state);

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

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  --controller        the controller message queue file descriptor\n\
  --updater           the updater message queue file descriptor\n\
  --shutdowner        the shutdowner message queue file descriptor\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

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
        linted_io_write_string(STDERR_FILENO, NULL, PACKAGE_STRING);

        linted_io_write_string(STDERR_FILENO, NULL, "\n\n");

        linted_io_write_format(STDERR_FILENO, NULL, "\
Copyright (C) %d Steven Stewart-Gallus\n\
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

    linted_controller const controller = linted_io_strtofd(controller_name);
    if (-1 == controller) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                               program_name,
                               CONTROLLER_OPTION,
                               linted_error_string_alloc(errno));
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    linted_shutdowner const shutdowner = linted_io_strtofd(shutdowner_name);
    if (-1 == shutdowner) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                               program_name,
                               SHUTDOWNER_OPTION,
                               linted_error_string_alloc(errno));
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    linted_updater const updater = linted_io_strtofd(updater_name);
    if (-1 == updater) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                               program_name,
                               UPDATER_OPTION,
                               linted_error_string_alloc(errno));
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
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

        if (-1 == linted_util_sanitize_environment(&essential_fds)) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s", program_name, linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }
    }

    int exit_status = -1;

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
            goto close_timer;
        }
    }

    for (;;) {
        int read_fds[] = { controller, shutdowner, timer };
        int write_fds[] = { updater };
        int greatest = -1;

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(read_fds); ++ii) {
            if (greatest < read_fds[ii]) {
                greatest = read_fds[ii];
            }
        }

        if (simulator_state.update_pending) {
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(write_fds); ++ii) {
                if (greatest < write_fds[ii]) {
                    greatest = write_fds[ii];
                }
            }
        }

        fd_set watched_read_fds;
        fd_set watched_write_fds;
        int select_status;

        do {
            FD_ZERO(&watched_read_fds);
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(read_fds); ++ii) {
                FD_SET(read_fds[ii], &watched_read_fds);
            }

            FD_ZERO(&watched_write_fds);
            if (simulator_state.update_pending) {
                for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(write_fds); ++ii) {
                    FD_SET(write_fds[ii], &watched_write_fds);
                }
            }

            select_status = select(greatest + 1, &watched_read_fds,
                                   &watched_write_fds, NULL, NULL);
        } while (-1 == select_status && EINTR == errno);
        if (-1 == select_status) {
            goto close_timer;
        }

        if (FD_ISSET(shutdowner, &watched_read_fds)) {
            bool should_exit;
            if (-1 == on_shutdowner_readable(shutdowner, &should_exit)) {
                goto close_timer;
            }
            if (should_exit) {
                goto exit_main_loop;
            }
        }

        if (FD_ISSET(timer, &watched_read_fds)) {
            if (-1 ==
                on_timer_readable(timer, &controller_state, &simulator_state)) {
                goto close_timer;
            }
        }

        if (FD_ISSET(controller, &watched_read_fds)) {
            if (-1 == on_controller_readable(controller, &controller_state)) {
                goto close_timer;
            }
        }

        if (simulator_state.update_pending) {
            if (FD_ISSET(updater, &watched_write_fds)) {
                if (-1 == on_updater_writeable(updater, &simulator_state)) {
                    goto close_timer;
                }
            }
        }
    }

 exit_main_loop:
    exit_status = 0;

 close_timer:
    {
        int errnum = errno;
        int close_status = linted_io_close(timer);
        if (-1 == close_status) {
            assert(errno != EBADF);
        }

        if (-1 == exit_status) {
            errno = errnum;
        }

        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 exit:
    return -1 == exit_status ? errno : EXIT_SUCCESS;
}

static int on_timer_readable(int timer,
                             struct controller_state const *controller_state,
                             struct simulator_state *simulator_state)
{
    uint64_t ticks;
    if (-1 == read(timer, &ticks, sizeof ticks)) {
        return -1;
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

static int on_updater_writeable(linted_updater updater,
                                struct simulator_state *simulator_state)
{
    struct linted_updater_update update = {
        .x_position = simulator_state->x_position,
        .y_position = simulator_state->y_position
    };

    int update_status;
    do {
        update_status = linted_updater_send_update(updater, &update);
    } while (-1 == update_status && EINTR == errno);
    if (-1 == update_status) {
        if (EAGAIN == errno) {
            return 0;
        }

        return -1;
    }

    simulator_state->update_pending = false;

    return 0;
}

static int on_shutdowner_readable(linted_shutdowner shutdowner,
                                  bool * should_exit)
{
    int read_status;
    do {
        read_status = linted_shutdowner_receive(shutdowner);
    } while (-1 == read_status && EINTR == errno);
    if (-1 == read_status) {
        if (EAGAIN == errno) {
            *should_exit = false;
            return 0;
        }

        return -1;
    }

    *should_exit = true;
    return 0;
}

static int on_controller_readable(linted_controller controller,
                                  struct controller_state *controller_state)
{
    struct linted_controller_message message;

    int read_status;
    do {
        read_status = linted_controller_receive(controller, &message);
    } while (-1 == read_status && EINTR == errno);
    if (-1 == read_status) {
        if (EAGAIN == errno) {
            return 0;
        }

        return -1;
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
