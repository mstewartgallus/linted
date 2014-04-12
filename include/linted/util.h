/*
 * Copyright 2013 Steven Stewart-Gallus
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
#ifndef LINTED_UTIL_H
#define LINTED_UTIL_H

#include <errno.h>
#include <stdlib.h>
#include <syslog.h>
#include <sys/select.h>

#define LINTED_SIZEOF_MEMBER(type, member) (sizeof ((type *) 0)->member)

#define LINTED_ARRAY_SIZE(array) ((sizeof (array)) / sizeof ((array)[0]))

/**
 * A useful utility macro for exiting a task upon failing to
 * accomplish a function that must be accomplished to proceed.
 *
 * Should be used really, really rarely.
 */
#define LINTED_FATAL_FAILURE(errnum, format_string, ...)                \
    do {                                                                \
        syslog(LOG_ERR, "fatal failure in file %s, function %s, and line %i: " format_string, \
               __FILE__, __func__, __LINE__,  __VA_ARGS__);             \
        exit(errnum);                                                   \
    } while (0)

/**
 * A useful utility macro for errors that should never happen.
 *
 * This should only be used for errors that are caused by
 * unrecoverable problems such as memory corruption. The conditions
 * that raise this error should be documented if they are triggerable
 * by a caller of one's function. Think twice before using this macro
 * as aborting a function on invalid input makes that function just
 * that little bit less useful.
 *
 * Permissible errors to use this for may include EINVAL or EBADF.
 *
 * Nonpermissible errors to use this for may include EMFILE, ENFILE
 * and ENOMEM. These cases should be handled properly.
 */
#define LINTED_IMPOSSIBILITY(format_string, ...)                     \
    do {                                                                \
        syslog(LOG_ERR, "impossible error in file %s, function %s, and line %i: " format_string, \
               __FILE__, __func__, __LINE__,  __VA_ARGS__);             \
        abort();                                                        \
    } while (0)

/**
 * A useful utility macro to abort to the process for errors that the
 * developer is too lazy to handle properly. This macro should only
 * ever be used during development and not during release.
 */
#define LINTED_LAZY_DEV(format_string, ...)                       \
    do {                                                                \
        syslog(LOG_ERR, "lazy developer error in file %s, function %s, and line %i:" format_string, \
               __FILE__, __func__, __LINE__,  __VA_ARGS__);             \
        abort();                                                        \
    } while (0)

char const *linted_error_string_alloc(int errnum);

void linted_error_string_free(char const *error_string);

errno_t linted_util_sanitize_environment(fd_set const *essential_fds);

#endif                          /* LINTED_UTIL_H */
