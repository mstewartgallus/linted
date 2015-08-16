/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
#ifndef LINTED_UTIL_H
#define LINTED_UTIL_H

#ifndef NDEBUG
#include <assert.h>
#endif

#include <stddef.h>
#include <limits.h>

/**
 * @file
 *
 * Various utility macroes and functions.
 */

#if defined __GNUC__ && !defined __clang__ && !defined __CHECKER__
#define LINTED__IS_GCC 1
#else
#define LINTED__IS_GCC 0
#endif

#if defined __has_attribute
#define LINTED_UTIL_HAS_ATTRIBUTE(X) __has_attribute(X)
#else
#define LINTED_UTIL_HAS_ATTRIBUTE(X) 0
#endif

#if defined __has_builtin
#define LINTED_UTIL_HAS_BUILTIN(X) __has_builtin(X)
#else
#define LINTED_UTIL_HAS_BUILTIN(X) 0
#endif

#define LINTED_FIELD_SIZEOF(type, member) (sizeof((type){0}).member)

#define LINTED_ARRAY_SIZE(...)                                         \
	((sizeof __VA_ARGS__) / sizeof __VA_ARGS__[0])

#define LINTED_UPCAST(X) (&(X)->parent)
#define LINTED_DOWNCAST(T, X)                                          \
	((T *)(((char *)(X)) - offsetof(T, parent)))

#define LINTED_NUMBER_TYPE_STRING_SIZE(T)                              \
	((CHAR_BIT * sizeof(T) - 1U) / 3U + 2U)

#ifndef NDEBUG

#define LINTED_ASSUME_UNREACHABLE()                                    \
	do {                                                           \
		extern void abort(void);                               \
		abort();                                               \
	} while (0)
#define LINTED_ASSUME(X) assert(X)

#else

#if LINTED_UTIL_HAS_BUILTIN(__builtin_unreachable) || LINTED__IS_GCC
#define LINTED_ASSUME_UNREACHABLE() __builtin_unreachable()
#else
#define LINTED_ASSUME_UNREACHABLE()                                    \
	do {                                                           \
	} while (0)
#endif

#if LINTED_UTIL_HAS_BUILTIN(__builtin_assume)
#define LINTED_ASSUME(X) __builtin_assume(X)
#elif LINTED__IS_GCC
#define LINTED_ASSUME(X)                                               \
	do {                                                           \
		if (!(X))                                              \
			LINTED_ASSUME_UNREACHABLE();                   \
	} while (0)
#else
#define LINTED_ASSUME(X)                                               \
	do {                                                           \
	} while (0)
#endif

#endif

#if LINTED_UTIL_HAS_ATTRIBUTE(__warn_unused_result__) || LINTED__IS_GCC
#define LINTED_WARN_UNUSED __attribute__((__warn_unused_result__))
#else
#define LINTED_WARN_UNUSED
#endif

#if LINTED_UTIL_HAS_ATTRIBUTE(__format__) || LINTED__IS_GCC
#define LINTED_FORMAT(X, Y, Z) __attribute__((__format__(X, Y, Z)))
#else
#define LINTED_FORMAT(X, Y, Z)
#endif

#if LINTED_UTIL_HAS_ATTRIBUTE(__noclone__) || LINTED__IS_GCC
#define LINTED_NOCLONE __attribute__((__noclone__))
#else
#define LINTED_NOCLONE
#endif

#if LINTED_UTIL_HAS_ATTRIBUTE(__noinline__) || LINTED__IS_GCC
#define LINTED_NOINLINE __attribute__((__noinline__))
#else
#define LINTED_NOINLINE
#endif

#if LINTED_UTIL_HAS_ATTRIBUTE(__no_sanitize_address__) || LINTED__IS_GCC
#define LINTED_NO_SANITIZE_ADDRESS                                     \
	__attribute__((__no_sanitize_address__))
#else
#define LINTED_NO_SANITIZE_ADDRESS
#endif

#define LINTED_STATIC_ASSERT(...)                                      \
	static char                                                    \
	    _static_assertion_dummy[2U * ((_Bool)__VA_ARGS__) + 1U]

#endif /* LINTED_UTIL_H */
