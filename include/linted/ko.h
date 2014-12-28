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

#if defined _WIN32 || defined _WIN64
#include "ko-windows.h"
#elif defined __linux__
#include "ko-linux.h"
#else
#error kernel object support has not beeen implemented for this platform
#endif

/**
 * @file
 *
 * Abstracts over the concept of a kernel object.
 */

#define LINTED_KO_RDONLY 1UL
#define LINTED_KO_WRONLY (1UL << 1U)
#define LINTED_KO_RDWR (1UL << 2U)

#define LINTED_KO_APPEND (1UL << 3U)

#define LINTED_KO_SYNC (1UL << 4U)

#define LINTED_KO_DIRECTORY (1UL << 5U)

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop);

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags);

/**
 * @bug Does not work for sockets. Sockets are complicated because one
 *      can bind or connect to them. Only one socket can be bound to a
 *      socket at a time but many can connect. Checking for the socket
 *      type with fstat should work and connecting through
 *      /proc/self/fd but dealing with bind is more complicated and
 *      not something I fully understand.
 */
linted_error linted_ko_reopen(linted_ko *kooutp, linted_ko koin,
                              unsigned long flags);

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
