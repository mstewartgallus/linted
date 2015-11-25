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

#define LINTED_FIELD_SIZEOF(TYPE, MEMBER) (sizeof((TYPE){0}).MEMBER)

#define LINTED_ARRAY_SIZE(...)                                         \
	((sizeof __VA_ARGS__) / sizeof __VA_ARGS__[0])

#define LINTED_NUMBER_TYPE_STRING_SIZE(T)                              \
	((CHAR_BIT * sizeof(T) - 1U) / 3U + 2U)

#if LINTED_UTIL_HAS_BUILTIN(__builtin_trap) || LINTED__IS_GCC
#define LINTED_CRASH_FAST() __builtin_trap()
#else
#define LINTED_CRASH_FAST()                                            \
	do {                                                           \
		extern void abort(void);                               \
		abort();                                               \
	} while (0)
#endif

/**
 * We need our own `LINTED_ASSERT` because the real `assert` uses
 * `__FILE__`
 * which is nondeterministic.
 */
#if defined NDEBUG
#define LINTED_ASSERT(...)                                             \
	do {                                                           \
	} while (0)
#else
#define LINTED_ASSERT(...)                                             \
	do {                                                           \
		if (!(__VA_ARGS__))                                    \
			LINTED_CRASH_FAST();                           \
	} while (0)
#endif

/*
 * This is needed because other uses of null can be optimized
 * dangerously or because deep array accesses can index into mapped
 * memory.
 */
#if defined NDEBUG
#define LINTED_ASSERT_NOT_NULL(...)                                    \
	do {                                                           \
	} while (0)
#else
#define LINTED_ASSERT_NOT_NULL(...)                                    \
	do {                                                           \
		*((char const volatile *)(__VA_ARGS__));               \
	} while (0)
#endif

#if defined NDEBUG

#if LINTED_UTIL_HAS_BUILTIN(__builtin_unreachable) || LINTED__IS_GCC
#define LINTED_ASSUME_UNREACHABLE() __builtin_unreachable()
#else
#define LINTED_ASSUME_UNREACHABLE()                                    \
	do {                                                           \
	} while (0)
#endif

#else

#define LINTED_ASSUME_UNREACHABLE()                                    \
	do {                                                           \
		LINTED_CRASH_FAST();                                   \
	} while (0)

#endif

#if defined NDEBUG

#if LINTED_UTIL_HAS_BUILTIN(__builtin_assume)
#define LINTED_ASSUME(...) __builtin_assume((__VA_ARGS__))
#elif LINTED__IS_GCC
#define LINTED_ASSUME(...)                                             \
	do {                                                           \
		if (!(__VA_ARGS__))                                    \
			LINTED_ASSUME_UNREACHABLE();                   \
	} while (0)

#else
#define LINTED_ASSUME(X)                                               \
	do {                                                           \
	} while (0)

#endif

#else

#define LINTED_ASSUME(X) LINTED_ASSERT(X)

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

#define LINTED_STATIC_ASSERT_CONCAT_(A, B) A##B
#define LINTED_STATIC_ASSERT_CONCAT(A, B)                              \
	LINTED_STATIC_ASSERT_CONCAT_(A, B)
#define LINTED_STATIC_ASSERT(...)                                      \
	enum { LINTED_STATIC_ASSERT_CONCAT(LINTED_ASSERT_line_,        \
		                           __LINE__) =                 \
		   1U / (!!(__VA_ARGS__)) }

#endif /* LINTED_UTIL_H */
