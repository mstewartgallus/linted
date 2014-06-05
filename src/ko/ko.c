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

#include "linted/ko.h"

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
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

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop)
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

    *kop = total;
    return 0;
}

linted_error linted_ko_dummy(linted_ko *kop)
{
    return linted_ko_open(kop, -1, "/dev/null", LINTED_KO_RDONLY);
}

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, int flags)
{
    linted_error errnum;

    if ((flags & ~LINTED_KO_RDONLY & ~LINTED_KO_WRONLY & ~LINTED_KO_RDWR) !=
        0u) {
        return EINVAL;
    }

    bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0u;
    bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0u;
    bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0u;

    bool ko_sync = (flags & LINTED_KO_SYNC) != 0u;

    if (ko_rdonly && ko_wronly) {
        return EINVAL;
    }

    if (ko_rdwr && ko_rdonly) {
        return EINVAL;
    }

    if (ko_rdwr && ko_wronly) {
        return EINVAL;
    }

    /*
     * Always, be safe for execs and use O_NONBLOCK because asynch
     * functions handle that anyways and open may block otherwise.
     */
    int oflags = O_CLOEXEC | O_NONBLOCK;

    if (ko_rdonly) {
        oflags |= O_RDONLY;
    }

    if (ko_wronly) {
        oflags |= O_WRONLY;
    }

    if (ko_rdwr) {
        oflags |= O_RDWR;
    }

    if (ko_sync) {
        oflags |= O_SYNC;
    }

    int fildes;
    do {
        fildes = openat(dirko, pathname, oflags);
        if (-1 == fildes) {
            errnum = errno;
            assert(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EINTR == errnum);
    if (errnum != 0) {
        return errnum;
    }

    *kop = fildes;

    return 0;
}

linted_error linted_ko_reopen(linted_ko *kop, int flags)
{
    linted_ko ko = *kop;

    char pathname[sizeof "/proc/self/fd/" + 10];
    sprintf(pathname, "/proc/self/fd/%i", ko);
    return linted_ko_open(kop, -1, pathname, flags);
}

linted_error linted_ko_close(linted_ko ko)
{
    linted_error errnum;
    /*
     * The state of a file descriptor after close gives an EINTR error
     * is unspecified by POSIX so this function avoids the problem by
     * simply blocking all signals.
     */

    sigset_t fullset;
    sigfillset(&fullset);

    sigset_t old_set;
    pthread_sigmask(SIG_BLOCK, &fullset, &old_set);

    if (-1 == close(ko)) {
        errnum = errno;
        assert(errnum != 0);
    } else {
        errnum = 0;
    }

    pthread_sigmask(SIG_SETMASK, &old_set, NULL);

    return errnum;
}
