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
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <unistd.h>

#if defined __linux__
#define FDS_DIR "/proc/self/fd"
#else
#error no open files directory known for this platform
#endif

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

    char const *const program_name = argv[0u];

    size_t kos_size = linted_start_config.kos_size;
    linted_ko *kos = linted_start_config.kos;

    DIR *const fds_dir = opendir(FDS_DIR);
    if (NULL == fds_dir) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: %s: %s\n",
                               program_name, FDS_DIR,
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    /**
     * @todo give an error if too many files are passed
     */
    size_t kos_found = 0u;
    for (; kos_found < kos_size;) {
        /*
         * Use readdir because this function isn't thread safe
         * anyways and readdir_r has a very broken interface.
         */
        errno = 0;
        struct dirent *const result = readdir(fds_dir);
        {
            int errnum = errno;
            if (errnum != 0) {
                linted_io_write_format(STDERR_FILENO, NULL, "\
%s: %s: %s\n",
                                       program_name, FDS_DIR,
                                       linted_error_string_alloc(errnum));
                return EXIT_FAILURE;
            }
        }

        if (NULL == result) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: too little argument files\n",
                                   program_name);
            return EXIT_FAILURE;
        }

        char const *const d_name = result->d_name;
        if (0 == strcmp(d_name, ".")) {
            continue;
        }

        if (0 == strcmp(d_name, "..")) {
            continue;
        }

        int const fd = atoi(d_name);

        /*
         * This is Linux specific code so we can rely on dirfd to
         * not return ENOTSUP here.
         */

        if (fd == dirfd(fds_dir)) {
            continue;
        }

        {
            int flags = fcntl(fd, F_GETFD);
            if (-1 == flags) {
                linted_io_write_format(STDERR_FILENO, NULL, "\
%s: fcntl: F_GETFD: %s\n",
                                       linted_error_string_alloc(errno));
                return EXIT_FAILURE;
            }

            if (-1 == fcntl(fd, F_SETFD, flags | FD_CLOEXEC)) {
                linted_io_write_format(STDERR_FILENO, NULL, "\
%s: fcntl: F_SETFD: %s\n",
                                       linted_error_string_alloc(errno));
                return EXIT_FAILURE;
            }
        }

        {
            int flags = fcntl(fd, F_GETFL);
            if (-1 == flags) {
                linted_io_write_format(STDERR_FILENO, NULL, "\
%s: fcntl: F_GETFL: %s\n",
                                       linted_error_string_alloc(errno));
                return EXIT_FAILURE;
            }

            if (-1 == fcntl(fd, F_SETFL, flags | O_NONBLOCK)) {
                linted_io_write_format(STDERR_FILENO, NULL, "\
%s: fcntl: F_SETFL: %s\n",
                                       linted_error_string_alloc(errno));
                return EXIT_FAILURE;
            }
        }

        kos[kos_found] = fd;
        ++kos_found;
    }

    for (;;) {
        errno = 0;
        struct dirent *const result = readdir(fds_dir);
        {
            int errnum = errno;
            if (errnum != 0) {
                linted_io_write_format(STDERR_FILENO, NULL, "\
%s: %s: %s\n",
                                       program_name, FDS_DIR,
                                       linted_error_string_alloc(errnum));
                return EXIT_FAILURE;
            }
        }

        if (NULL == result) {
            break;
        }

        char const *const d_name = result->d_name;
        if (0 == strcmp(d_name, ".")) {
            continue;
        }

        if (0 == strcmp(d_name, "..")) {
            continue;
        }

        int const fd = atoi(d_name);

        /*
         * This is Linux specific code so we can rely on dirfd to
         * not return ENOTSUP here.
         */

        if (fd == dirfd(fds_dir)) {
            continue;
        }

        /* Don't check for errors, an error just means someone leaked
         * a handle to /dev/full.
         */
        linted_ko_close(fd);
    }

    if (-1 == closedir(fds_dir)) {
        int errnum = errno;
        assert(errnum != 0);
        assert(errnum != EBADF);

        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: %s: %s\n",
                               program_name, FDS_DIR,
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    /* Sort the fds from smallest to largest */
    for (size_t ii = 0u; ii < kos_size; ++ii) {
        for (size_t jj = ii + 1u; jj < kos_size; ++jj) {
            if (kos[ii] > kos[jj]) {
                linted_ko temp = kos[ii];
                kos[ii] = kos[jj];
                kos[jj] = temp;
            }
        }
    }

    linted_ko cwd;
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
