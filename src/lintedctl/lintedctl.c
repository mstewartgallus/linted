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
#include "config.h"

#include "linted/io.h"
#include "linted/manager.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

static void write_version(void);

int main(int argc, char **argv)
{
    if (argc < 1) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing process name\n",
                               PACKAGE_TARNAME "-lintedctl");
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;
    char const *command = NULL;
    unsigned last_index = 1;
    for (; last_index < (unsigned)argc; ++last_index) {
        char const *argument = argv[last_index];

        if (0 == strncmp(argument, "--", strlen("--"))) {
            if (0 == strcmp(argument, "--help")) {
                need_help = true;
            } else if (0 == strcmp(argument, "--version")) {
                need_version = true;
            } else {
                bad_option = argument;
            }
        } else {
            command = argument;
            break;
        }
    }
    ++last_index;

    if (need_help) {
        linted_io_write_format(STDOUT_FILENO, NULL,
                               "Usage: %s [OPTIONS] COMMAND\n", program_name);

        linted_io_write_format(STDOUT_FILENO, NULL,
                               "Send commands to the game.\n", PACKAGE_NAME);

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  LINTED_PID          the process id of the linted game\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  start               start the gui service\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_format(STDOUT_FILENO, NULL, "Report bugs to <%s>\n",
                               PACKAGE_BUGREPORT);
        linted_io_write_format(STDOUT_FILENO, NULL, "%s home page: <%s>\n",
                               PACKAGE_NAME, PACKAGE_URL);

        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: unrecognized option '%s'\n",
                               program_name, bad_option);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s --help' for more information.\n",
                               program_name);
        return EXIT_FAILURE;
    }

    if (need_version) {
        write_version();
        return EXIT_SUCCESS;
    }

    if (NULL == command) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing COMMAND\n", program_name);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s --help' for more information.\n",
                               program_name);
        return EXIT_FAILURE;
    }

    if (0 == strcmp("start", command)) {
        bool need_add_help = false;
        char const *bad_argument = NULL;
        for (; last_index < (unsigned)argc; ++last_index) {
            char const *argument = argv[last_index];

            if (0 == strncmp(argument, "--", strlen("--"))) {
                if (0 == strcmp(argument, "--help")) {
                    need_add_help = true;
                } else if (0 == strcmp(argument, "--version")) {
                    need_version = true;
                } else {
                    bad_option = argument;
                }
            } else {
                bad_argument = argument;
                break;
            }
        }

        if (need_add_help) {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "Usage: %s start [OPTIONS]\n", program_name);

            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "Start the gui service.\n", PACKAGE_NAME);

            linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

            linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));

            linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

            linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  LINTED_PID          the process id of the linted game\n"));

            linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

            linted_io_write_format(STDOUT_FILENO, NULL, "Report bugs to <%s>\n",
                                   PACKAGE_BUGREPORT);
            linted_io_write_format(STDOUT_FILENO, NULL, "%s home page: <%s>\n",
                                   PACKAGE_NAME, PACKAGE_URL);

            return EXIT_SUCCESS;
        }

        if (bad_option != NULL) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: unrecognized option '%s'\n",
                                   program_name, bad_option);
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s --help' for more information.\n",
                                   program_name);
            return EXIT_FAILURE;
        }

        if (bad_argument != NULL) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: too many arguments: '%s'\n",
                                   program_name, bad_argument);
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s --help' for more information.\n",
                                   program_name);
            return EXIT_FAILURE;
        }

        if (need_version) {
            write_version();
            return EXIT_SUCCESS;
        }

        char const *pid_string = getenv("LINTED_PID");
        if (NULL == pid_string) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: missing LINTED_PID\n", program_name);
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s --help' for more information.\n",
                                   program_name);
            return EXIT_FAILURE;
        }

        pid_t pid;
        {
            int pid_out;
            errno_t errnum = linted_io_strtofd(pid_string, &pid_out);
            if (errnum != 0) {
                linted_io_write_format(STDERR_FILENO, NULL,
                                       "%s: LINTED_PID='%s': %s\n",
                                       program_name, pid_string,
                                       linted_error_string_alloc(errnum));
                linted_io_write_format(STDERR_FILENO, NULL,
                                       "Try `%s --help' for more information.\n",
                                       program_name);
                return EXIT_FAILURE;
            }
            pid = pid_out;
        }

        struct linted_manager_start_req request;

        request.type = LINTED_MANAGER_START;
        request.args.service = LINTED_MANAGER_SERVICE_GUI;

        linted_io_write_format(STDOUT_FILENO, NULL,
                               "%s: sending start request for the gui\n",
                               program_name);

        errno_t errnum = linted_manager_send_request(pid, (void *)&request);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: could not send request: %s\n",
                                   program_name,
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }

        if (request.reply.is_up) {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "%s: gui is (probably) up\n", program_name);
        } else {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "%s: the gui is down\n", program_name);
        }

        return EXIT_SUCCESS;
    } else {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: unrecognized command '%s'\n",
                               program_name, command);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s --help' for more information.\n",
                               program_name);
        return EXIT_FAILURE;
    }
}

static void write_version(void)
{
    linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_STRING));

    linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n\n"));

    linted_io_write_format(STDOUT_FILENO, NULL, "\
Copyright (C) %d Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n", COPYRIGHT_YEAR);

}
