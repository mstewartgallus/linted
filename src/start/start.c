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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/start.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/locale.h"
#include "linted/ko.h"

#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    /* First we check if we are run with proper security */
    uid_t const uid = getuid();
    uid_t const euid = geteuid();
    if (0 == euid || 0 == uid) {
        linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\
Bad administrator!\n\
It is insecure to run a game as root!\n"));
        return EXIT_FAILURE;
    }

    if (argc < 1) {
        linted_locale_missing_process_name(
            STDERR_FILENO, linted_start_config.canonical_process_name);
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    int cwd;
    if (linted_start_config.open_current_working_directory) {
        cwd = open("./", O_DIRECTORY | O_CLOEXEC);
        if (-1 == cwd) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not open the current working directory: %s\n",
                                   program_name,
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }
    } else {
        cwd = -1;
    }

    if (-1 == chdir("/")) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not change to the root directory: %s\n",
                               program_name, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    /* Start sandboxing a bit */
    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        linted_error errnum = errno;

        assert(errnum != EINVAL);

        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not drop ability to raise privileges through execve: %s\n",
                               program_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    return linted_start(cwd, program_name, argc, (char const * const *)argv);
}
