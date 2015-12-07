/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_KO_H
#define LNTD_KO_H

#include "lntd/error.h"
#include "lntd/util.h"

#if defined HAVE_WINDOWS_API
#include "ko-windows.h"
#elif defined HAVE_POSIX_API
#include "ko-posix.h"
#else
#error kernel object support has not beeen implemented for this platform
#endif

/**
 * @file
 *
 * Abstracts over the concept of a kernel object.
 */

#define LNTD_KO_RDONLY 1UL
#define LNTD_KO_WRONLY (1UL << 1U)
#define LNTD_KO_RDWR (1UL << 2U)

#define LNTD_KO_APPEND (1UL << 3U)

#define LNTD_KO_SYNC (1UL << 4U)

#define LNTD_KO_DIRECTORY (1UL << 5U)
#define LNTD_KO_FIFO (1UL << 6U)

lntd_error lntd_ko_open(lntd_ko *kop, lntd_ko dirko,
                        char const *pathname,
                        unsigned long flags) LNTD_WARN_UNUSED;

/**
 * The lntd_ko_close function closes a kernel object.
 *
 * @param ko The kernel object to close.
 *
 * @returns Zero on success or an error code.
 *
 * @error EIO I/O error.
 *
 * @error EBADF Not a valid file descriptor.
 */
lntd_error lntd_ko_close(lntd_ko ko);

lntd_error lntd_ko_change_directory(char const *pathname);

lntd_error lntd_ko_symlink(char const *oldpath, char const *newpath);

lntd_error lntd_ko_real_path(char **resultp, lntd_ko dirko,
                             char const *pathname) LNTD_WARN_UNUSED;

#endif /* LNTD_KO_H */
