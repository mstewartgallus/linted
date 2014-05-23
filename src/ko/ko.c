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

#include "linted/ko.h"

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

linted_error linted_ko_strtofd(char const *str, int *fd)
{
    size_t length = strlen(str);
    unsigned position = 1u;

    if ('0' == str[0u] && length != 1u) {
        return EINVAL;
    }

    unsigned total = 0u;
    for (; length > 0u; --length) {
        char const digit = str[length - 1u];

        if ('0' <= digit && digit <= '9') {
            unsigned long sum = total + ((unsigned)(digit - '0')) * position;
            if (sum > INT_MAX) {
                return ERANGE;
            }

            total = sum;
        } else {
            return EINVAL;
        }

        unsigned long next_position = 10u * position;
        if (next_position > INT_MAX) {
            return ERANGE;
        }
        position = next_position;
    }

    *fd = total;
    return 0;
}

linted_error linted_ko_dummy(linted_ko *kop)
{
    int fildes = openat(-1, "/dev/null", O_RDONLY | O_CLOEXEC);
    if (-1 == fildes) {
        return errno;
    }

    *kop = fildes;
    return 0;
}

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, linted_ko_flags flags)
{
    if ((flags & ~LINTED_KO_RDONLY & ~LINTED_KO_WRONLY & ~LINTED_KO_RDWR) !=
        0u) {
        return EINVAL;
    }

    bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0u;
    bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0u;
    bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0u;

    if (ko_rdonly && ko_wronly) {
        return EINVAL;
    }

    if (ko_rdwr && ko_rdonly) {
        return EINVAL;
    }

    if (ko_rdwr && ko_wronly) {
        return EINVAL;
    }

    int oflags = O_CLOEXEC;

    if (ko_rdonly) {
        oflags |= O_RDONLY;
    }

    if (ko_wronly) {
        oflags |= O_WRONLY;
    }

    if (ko_rdwr) {
        oflags |= O_RDWR;
    }

    int fildes = openat(dirko, pathname, oflags);
    if (-1 == fildes) {
        return errno;
    }

    *kop = fildes;
    return 0;
}

linted_error linted_ko_close(linted_ko ko)
{
    /*
     * The state of a file descriptor after close gives an EINTR error
     * is unspecified by POSIX so this function avoids the problem by
     * simply blocking all signals.
     */

    sigset_t fullset;
    sigfillset(&fullset);

    sigset_t old_set;
    pthread_sigmask(SIG_BLOCK, &fullset, &old_set);

    int close_status = close(ko);

    pthread_sigmask(SIG_SETMASK, &old_set, NULL);

    return -1 == close_status ? errno : 0;
}
