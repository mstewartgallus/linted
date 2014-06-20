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
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/random.h"

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

static bool is_open(linted_ko ko);
static linted_error find_open_kos(linted_ko **kosp, size_t *size);
static void sort_kos(linted_ko *ko, size_t size);
static linted_error get_system_entropy(unsigned *entropyp);

int main(int argc, char *argv[])
{
    linted_error errnum;

    if (!is_open(STDERR_FILENO)) {
        /* Sadly, this is all we can do */
        return EXIT_FAILURE;
    }

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

    linted_ko *open_kos;
    size_t open_kos_size;
    {
        linted_ko *xx;
        size_t yy;
        if ((errnum = find_open_kos(&xx, &yy)) != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: couldn't find open files: %s\n",
                                   program_name,
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
        open_kos = xx;
        open_kos_size = yy;
    }

    /* Sort the fds from smallest to largest */
    sort_kos(open_kos, open_kos_size);

    size_t kos_size = linted_start_config.kos_size;
    linted_ko *kos = linted_start_config.kos;

    if (open_kos_size < kos_size) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: too little files passed in\n",
                               program_name);
        return EXIT_FAILURE;
    }

    for (size_t ii = 0u; ii < kos_size; ++ii) {
        kos[ii] = open_kos[ii];
    }

    for (size_t ii = kos_size; ii < open_kos_size; ++ii) {
        /* Don't check for errors, could just be a leaked /dev/full handle */
        linted_ko_close(open_kos[ii]);
    }
    linted_mem_free(open_kos);

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

        char pathname[sizeof FDS_DIR + 10u];
        sprintf(pathname, FDS_DIR "/%i", fd);

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
        errnum = errno;

        assert(errnum != EINVAL);

        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not drop ability to raise privileges through execve: %s\n",
                               program_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    {
        unsigned entropy;
        if ((errnum = get_system_entropy(&entropy)) != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not read a source of system entropy: %s\n",
                                   program_name,
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
        linted_random_seed_generator(entropy);
    }

    return linted_start(cwd, program_name, argc, (char const * const *)argv);
}

static bool is_open(linted_ko ko)
{
    return fcntl(ko, F_GETFD) != -1;
}

static void sort_kos(linted_ko *kos, size_t size)
{
    for (size_t ii = 0u; ii < size; ++ii) {
        for (size_t jj = ii + 1u; jj < size; ++jj) {
            linted_ko kos_ii = kos[ii];
            linted_ko kos_jj = kos[jj];

            if (kos_ii > kos_jj) {
                kos[ii] = kos_jj;
                kos[jj] = kos_ii;
            }
        }
    }
}

static linted_error find_open_kos(linted_ko **kosp, size_t *sizep)
{
    linted_error errnum = 0;
    size_t size = 0u;
    linted_ko *fds = NULL;

    /*
     * Use readdir because this function isn't thread safe anyways and
     * readdir_r has a very broken interface.
     */
    /*
     * This is Linux specific code so we can rely on dirfd to not
     * return ENOTSUP here.
     */

    DIR *const fds_dir = opendir(FDS_DIR);
    if (NULL == fds_dir) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    for (;;) {
        errno = 0;
        struct dirent *const result = readdir(fds_dir);
        {
            errnum = errno;
            if (errnum != 0) {
                goto close_fds_dir;
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

        if (fd == dirfd(fds_dir)) {
            continue;
        }

        ++size;
    }

    rewinddir(fds_dir);

    {
        linted_error xx;
        fds = linted_mem_alloc(&xx, size);
        errnum = xx;
    }
    if (errnum != 0) {
        goto close_fds_dir;
    }

    for (size_t ii = 0u; ii < size;) {
        errno = 0;
        struct dirent *const result = readdir(fds_dir);
        {
            errnum = errno;
            if (errnum != 0) {
                goto free_fds;
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

        if (fd == dirfd(fds_dir)) {
            continue;
        }

        fds[ii] = fd;
        ++ii;
    }

free_fds:
    if (errnum != 0) {
        linted_mem_free(fds);
        fds = NULL;
    }

close_fds_dir:
    if (-1 == closedir(fds_dir)) {
        linted_error close_errnum = errno;
        assert(close_errnum != 0);
        assert(close_errnum != EBADF);

        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    *sizep = size;
    *kosp = fds;

    return errnum;
}

static linted_error get_system_entropy(unsigned *entropyp)
{
    /*
     * Use /dev/random so we block when asking for entropy after
     * startup.
     */
    linted_error errnum;
    linted_ko random;
    unsigned entropy;

    {
        linted_ko xx;
        if ((errnum = linted_ko_open(&xx, -1, "/dev/random", LINTED_KO_RDONLY))
            != 0) {
            return errnum;
        }
        random = xx;
    }

    {
        unsigned xx;
        if ((errnum = linted_io_read_all(random, NULL, &xx, sizeof xx)) != 0) {

            return errnum;
        }
        entropy = xx;
    }

    if ((errnum = linted_ko_close(random)) != 0) {
        return errnum;
    }

    *entropyp = entropy;
    return 0;
}
