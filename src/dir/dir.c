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
#define _GNU_SOURCE

#include "config.h"

#include "linted/dir.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

linted_error linted_dir_create(linted_ko *kop, linted_ko dirko,
                               char const *pathname, unsigned long flags,
                               mode_t mode)
{
    linted_error errnum;

    if ((flags & ~LINTED_DIR_EXCL) != 0UL) {
        return EINVAL;
    }

    bool dir_excl = (flags & LINTED_DIR_EXCL) != 0UL;

make_directory:
    if (-1 == mkdirat(dirko, pathname, mode)) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        if (EEXIST == errnum && !dir_excl) {
            goto open_directory;
        }
        return errnum;
    }

open_directory:;
    int fildes;
    do {
        fildes = openat(dirko, pathname,
                        O_CLOEXEC | O_NONBLOCK | O_DIRECTORY | O_NOFOLLOW,
                        mode);
        if (-1 == fildes) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EINTR == errnum);

    if (ENOENT == errnum) {
        goto make_directory;
    }

    if (errnum != 0) {
        return errnum;
    }

    *kop = fildes;
    return 0;
}
