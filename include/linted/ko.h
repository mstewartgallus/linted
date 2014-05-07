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
#ifndef LINTED_KO_H
#define LINTED_KO_H

#include "linted/error.h"

/**
 * @file
 *
 * This module abstracts over the concept of a kernel object.
 */

#if defined _WIN32
#include <windows/wtypes.h>
#endif

#if defined __unix__
typedef int linted_ko;
#elif defined _WIN32
struct linted_ko_vtable;

typedef struct linted_ko
{
    struct linted_ko_vtable const* vtable;
    char contents[];
}* linted_ko;

struct linted_ko_vtable
{
    linted_error (*close)(linted_ko ko);
};

#else
#error no known most primitive platform kernel object type
#endif

linted_error linted_ko_strtofd(char const* ptr, linted_ko* ko);

linted_error linted_ko_dummy(linted_ko* kop, int flags);

/**
 * The linted_ko_close function closes a file descriptor. The state of
 * a file descriptor after close gives an EINTR error is unspecified
 * by POSIX so this function avoids the problem by simply blocking all
 * signals.
 *
 * @warning This function blocks on close.
 *
 * @param fd The file to close.
 *
 * @returns Zero on success or an error code.
 *
 * @error EIO I/O error.
 *
 * @error EBADF Not a valid file descriptor.
 */
linted_error linted_ko_close(linted_ko ko);

#endif /* LINTED_KO_H */
