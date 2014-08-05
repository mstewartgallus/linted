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

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>

/**
 * @file
 *
 * Various utility macroes and functions.
 *
 * @todo Factor this out and clean this up.
 */

#define LINTED_SIZEOF_MEMBER(type, member) (sizeof((type *)0)->member)

#define LINTED_ARRAY_SIZE(...) ((sizeof __VA_ARGS__) / sizeof __VA_ARGS__[0])

#define LINTED_UPCAST(X) (&(X)->parent)
#define LINTED_DOWNCAST(T, X) ((T *)(((char *)(X)) - offsetof(T, parent)))

/**
 * A useful utility macro for exiting a task upon failing to
 * accomplish a function that must be accomplished to proceed.
 *
 * Should be used really, really rarely.
 */
#define LINTED_FATAL_FAILURE(errnum, format_string, ...)                       \
    do {                                                                       \
        linted_io_write_format(STDERR_FILENO, NULL, "\
fatal failure in file %s, function %s, and line %i: " format_string,           \
                               __FILE__, __func__, __LINE__, __VA_ARGS__);     \
        exit(errnum);                                                          \
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
#define LINTED_IMPOSSIBILITY(format_string, ...)                               \
    do {                                                                       \
        linted_io_write_format(STDERR_FILENO, NULL, "\
impossible error in file %s, function %s, and line %i: " format_string,        \
                               __FILE__, __func__, __LINE__, __VA_ARGS__);     \
        abort();                                                               \
    } while (0)

/**
 * A useful utility macro to abort to the process for errors that the
 * developer is too lazy to handle properly. This macro should only
 * ever be used during development and not during release.
 */
#define LINTED_LAZY_DEV(format_string, ...)                                    \
    do {                                                                       \
        linted_io_write_format(STDERR_FILENO, NULL, "\
lazy developer error in file %s, function %s, and line %i:" format_string,     \
                               __FILE__, __func__, __LINE__, __VA_ARGS__);     \
        abort();                                                               \
    } while (0)

/* These are defined to ignore impossible warnings in release builds */
#ifndef NDEBUG

#define LINTED_ASSUME_UNREACHABLE() assert(0)
#define LINTED_ASSUME(X) assert(X)

#else

#ifdef __GNUC__
#define LINTED_ASSUME_UNREACHABLE() __builtin_unreachable()
#else
#define LINTED_ASSUME_UNREACHABLE()                                            \
    do {                                                                       \
    } while (0)
#endif

#define LINTED_ASSUME(X)                                                       \
    do {                                                                       \
        if (!(X)) {                                                            \
            LINTED_ASSUME_UNREACHABLE();                                       \
        }                                                                      \
    } while (0)
#endif

static inline int_fast32_t linted_uint32_to_int32(uint_fast32_t positive)
{
    /*
   * Section 6.3.1.2 "Signed and unsigned integers" of the C99
   * standard specifies that the behaviour is implementation-defined
   * (or that a signal could be raised) if the new type is signed
   * and the value can't be represented in it so we do this.
   */
    if (positive > (int_fast64_t)INT32_MAX) {
        return -(uint_fast32_t)((UINT32_MAX - (int_fast64_t)positive) + 1U);
    }

    return positive;
}

linted_error linted_util_sanitize_environment(void);

#endif /* LINTED_UTIL_H */
