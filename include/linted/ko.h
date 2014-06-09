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
 * Abstracts over the concept of a kernel object.
 */

#if defined HAVE_WINDOWS_H
typedef void *linted_ko;
#elif defined __linux__
typedef int linted_ko;
#else
#error no known most primitive platform kernel object type
#endif

#define LINTED_KO_RDONLY 1
#define LINTED_KO_WRONLY (1 << 2)
#define LINTED_KO_RDWR (1 << 3)

#define LINTED_KO_SYNC (1 << 4)

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop);

linted_error linted_ko_dummy(linted_ko *kop);

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, int flags);

linted_error linted_ko_reopen(linted_ko *kop, int flags);

/**
 * The linted_ko_close function closes a kernel object.
 *
 * @param ko The kernel object to close.
 *
 * @returns Zero on success or an error code.
 *
 * @error EIO I/O error.
 *
 * @error EBADF Not a valid file descriptor.
 */
linted_error linted_ko_close(linted_ko ko);

#endif /* LINTED_KO_H */
