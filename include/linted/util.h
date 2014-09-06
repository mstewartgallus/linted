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
#ifndef LINTED_UTIL_H
#define LINTED_UTIL_H

#include <assert.h>
#include <stddef.h>

/**
 * @file
 *
 * Various utility macroes and functions.
 */

#define LINTED_FIELD_SIZEOF(type, member) (sizeof((type *)0)->member)

#define LINTED_ARRAY_SIZE(...) ((sizeof __VA_ARGS__) / sizeof __VA_ARGS__[0])

#define LINTED_UPCAST(X) (&(X)->parent)
#define LINTED_DOWNCAST(T, X) ((T *)(((char *)(X)) - offsetof(T, parent)))

#ifndef NDEBUG

#define LINTED_ASSUME_UNREACHABLE() assert(0)
#define LINTED_ASSUME(X) assert(X)

#else

#if !defined __frama_c__ && defined __GNUC__
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

#if defined __GNUC__
#define LINTED_FORMAT_ANNOT(X, Y) __attribute__ ((format (printf, X, Y)))
#else
#define LINTED_FORMAT_ANNOT(X, Y)
#endif

#endif /* LINTED_UTIL_H */
