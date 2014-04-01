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

    char const * bad_option = NULL;
    char const * command = NULL;
    unsigned last_index = 1;
    for (; last_index < (unsigned) argc; ++last_index) {
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

    if (need_help) {
        linted_io_write_format(STDOUT_FILENO, NULL,
                               "Usage: %s [OPTIONS] COMMAND\n",
                               program_name);

        linted_io_write_format(STDOUT_FILENO, NULL,
                               "Send commands to the game.\n",
                               PACKAGE_NAME);

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  LINTED_PID          the process id of the linted game\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  add                 give a command to the server to perform addition\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

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
        linted_io_write_string(STDERR_FILENO, NULL, PACKAGE_STRING);

        linted_io_write_string(STDERR_FILENO, NULL, "\n\n");

        linted_io_write_format(STDERR_FILENO, NULL, "\
Copyright (C) %d Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n", COPYRIGHT_YEAR);

        return EXIT_SUCCESS;
    }

    if (NULL == command) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing COMMAND\n",
                               program_name);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s --help' for more information.\n",
                               program_name);
        return EXIT_FAILURE;
    }

    if (0 == strcmp("add", command)) {
        if (last_index + 1 != (unsigned) argc) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: too many arguments\n",
                                   program_name);
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "Try `%s --help' for more information.\n",
                                   program_name);
            return EXIT_FAILURE;
        }

        pid_t pid = atoi(getenv("LINTED_PID"));

        struct linted_manager_reply reply;
        struct linted_manager_request request;

        request.reply = &reply;
        request.number = 5;

        linted_io_write_format(STDOUT_FILENO, NULL, "%s: sending %i\n",
                               program_name,
                               request.number);

        if (-1 == linted_manager_send_request(pid, &request)) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: could not send request: %s\n",
                                   program_name,
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }

        linted_io_write_format(STDOUT_FILENO, NULL, "%s: received %i\n",
                               program_name, reply.number);

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
