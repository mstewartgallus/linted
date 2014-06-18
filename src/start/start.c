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
#define _GNU_SOURCE

#include "config.h"

#include "linted/start.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/locale.h"
#include "linted/mem.h"
#include "linted/ko.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
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

    size_t fds_count = 0u;
    for (;;) {
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

        ++fds_count;
    }

    if (fds_count < kos_size) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: too little fds\n", program_name);
        return EXIT_FAILURE;
    }

    rewinddir(fds_dir);

    linted_ko * fds;
    {
        linted_error errnum;
        fds = linted_mem_alloc(&errnum, fds_count);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: %s: %s\n",
                                       program_name, FDS_DIR,
                                       linted_error_string_alloc(errnum));
                return EXIT_FAILURE;
        }
    }

    size_t kos_found = 0u;
    for (; kos_found < fds_count;) {
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

        fds[kos_found] = fd;
        ++kos_found;
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
    for (size_t ii = 0u; ii < kos_found; ++ii) {
        for (size_t jj = ii + 1u; jj < kos_found; ++jj) {
            if (fds[ii] > fds[jj]) {
                linted_ko temp = fds[ii];
                fds[ii] = fds[jj];
                fds[jj] = temp;
            }
        }
    }

    for (size_t ii = 0u; ii < kos_size; ++ii) {
        kos[ii] = fds[ii];
    }

    for (size_t ii = kos_size; ii < kos_found; ++ii) {
        /* Don't check for errors, could just be a leaked /dev/full handle */
        linted_ko_close(fds[ii]);
    }
    linted_mem_free(fds);

    /* Sanitize the fds */
    for (size_t ii = 0u; ii < kos_size; ++ii) {
        linted_ko fd = kos[ii];

        int oflags = fcntl(fd, F_GETFL);
        if (-1 == oflags) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: fcntl: F_GETFL: %s\n",
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }

        char pathname[sizeof "/proc/self/fd/" + 10u];
        sprintf(pathname, "/proc/self/fd/%i", fd);

        int errnum;
        int new_fd;
        do {
            new_fd = openat(-1, pathname, oflags | O_NONBLOCK | O_CLOEXEC);
            if (-1 == new_fd) {
                errnum = errno;
                assert(errnum != 0);
            } else {
                errnum = 0;
            }
        } while (EINTR == errnum);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: openat: %s\n",
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }

        if ((errnum = linted_ko_close(fd)) != 0) {
             linted_io_write_format(STDERR_FILENO, NULL, "\
%s: linted_ko_close: %s\n",
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }

        if (-1 == dup3(new_fd, fd, O_CLOEXEC)) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: dup3: %s\n",
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }

        if ((errnum = linted_ko_close(new_fd)) != 0) {
             linted_io_write_format(STDERR_FILENO, NULL, "\
%s: linted_ko_close: %s\n",
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
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
