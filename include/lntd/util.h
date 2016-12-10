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
#ifndef LNTD_UTIL_H
#define LNTD_UTIL_H

#include <limits.h>
#include <stddef.h>

/**
 * @file
 *
 * Various utility macroes and functions.
 */

#if defined __GNUC__ && !defined __clang__ && !defined __CHECKER__
#define LNTD__IS_GCC 1
#else
#define LNTD__IS_GCC 0
#endif

#if defined __has_attribute
#define LNTD_UTIL_HAS_ATTRIBUTE(X) __has_attribute(X)
#else
#define LNTD_UTIL_HAS_ATTRIBUTE(X) 0
#endif

#if defined __has_builtin
#define LNTD_UTIL_HAS_BUILTIN(X) __has_builtin(X)
#else
#define LNTD_UTIL_HAS_BUILTIN(X) 0
#endif

#define LNTD_FIELD_SIZEOF(TYPE, MEMBER) (sizeof((TYPE){0}).MEMBER)

#define LNTD_ARRAY_SIZE(...)                                           \
	((sizeof __VA_ARGS__) / sizeof __VA_ARGS__[0])

#define LNTD_NUMBER_TYPE_STRING_SIZE(T)                                \
	((CHAR_BIT * sizeof(T) - 1U) / 3U + 2U)

#if !defined __CHECKER__ &&                                            \
    (LNTD_UTIL_HAS_BUILTIN(__builtin_trap) || LNTD__IS_GCC)
#define LNTD_CRASH_FAST() __builtin_trap()
#else
#define LNTD_CRASH_FAST()                                              \
	do {                                                           \
		extern void abort(void);                               \
		abort();                                               \
	} while (0)
#endif

/**
 * We need our own `LNTD_ASSERT` because the real `assert` uses
 * `__FILE__`
 * which is nondeterministic.
 */
#if defined NDEBUG
#define LNTD_ASSERT(...)                                               \
	do {                                                           \
	} while (0)
#else
#define LNTD_ASSERT(...)                                               \
	do {                                                           \
		if (!(__VA_ARGS__))                                    \
			LNTD_CRASH_FAST();                             \
	} while (0)
#endif

/*
 * This is needed because other uses of null can be optimized
 * dangerously or because deep array accesses can index into mapped
 * memory.
 */
#if defined NDEBUG
#define LNTD_ASSERT_NOT_NULL(...)                                      \
	do {                                                           \
	} while (0)
#else
#define LNTD_ASSERT_NOT_NULL(...)                                      \
	do {                                                           \
		if (0 == (__VA_ARGS__))                                \
			LNTD_CRASH_FAST();                             \
	} while (0)
#endif

#if defined NDEBUG

#if LNTD_UTIL_HAS_BUILTIN(__builtin_unreachable) || LNTD__IS_GCC
#define LNTD_ASSUME_UNREACHABLE() __builtin_unreachable()
#else
#define LNTD_ASSUME_UNREACHABLE()                                      \
	do {                                                           \
	} while (0)
#endif

#else

#define LNTD_ASSUME_UNREACHABLE()                                      \
	do {                                                           \
		LNTD_CRASH_FAST();                                     \
	} while (0)

#endif

#if defined NDEBUG

#if LNTD_UTIL_HAS_BUILTIN(__builtin_assume)
#define LNTD_ASSUME(...) __builtin_assume((__VA_ARGS__))
#elif LNTD__IS_GCC
#define LNTD_ASSUME(...)                                               \
	do {                                                           \
		if (!(__VA_ARGS__))                                    \
			LNTD_ASSUME_UNREACHABLE();                     \
	} while (0)

#else
#define LNTD_ASSUME(X)                                                 \
	do {                                                           \
	} while (0)

#endif

#else

#define LNTD_ASSUME(X) LNTD_ASSERT(X)

#endif

#if LNTD_UTIL_HAS_ATTRIBUTE(__warn_unused_result__) || LNTD__IS_GCC
#define LNTD_WARN_UNUSED __attribute__((__warn_unused_result__))
#else
#define LNTD_WARN_UNUSED
#endif

#if LNTD_UTIL_HAS_ATTRIBUTE(__format__) || LNTD__IS_GCC
#define LNTD_FORMAT(X, Y, Z) __attribute__((__format__(X, Y, Z)))
#else
#define LNTD_FORMAT(X, Y, Z)
#endif

#if LNTD_UTIL_HAS_ATTRIBUTE(__noclone__) || LNTD__IS_GCC
#define LNTD_NOCLONE __attribute__((__noclone__))
#else
#define LNTD_NOCLONE
#endif

#if LNTD_UTIL_HAS_ATTRIBUTE(__noinline__) || LNTD__IS_GCC
#define LNTD_NOINLINE __attribute__((__noinline__))
#else
#define LNTD_NOINLINE
#endif

#if LNTD_UTIL_HAS_ATTRIBUTE(__no_sanitize_address__) || LNTD__IS_GCC
#define LNTD_NO_SANITIZE_ADDRESS                                       \
	__attribute__((__no_sanitize_address__))
#else
#define LNTD_NO_SANITIZE_ADDRESS
#endif

#define LNTD_STATIC_ASSERT_CONCAT_(A, B) A##B
#define LNTD_STATIC_ASSERT_CONCAT(A, B) LNTD_STATIC_ASSERT_CONCAT_(A, B)
#define LNTD_STATIC_ASSERT(...)                                        \
	enum { LNTD_STATIC_ASSERT_CONCAT(LNTD_ASSERT_line_,            \
		                         __LINE__) =                   \
		   1U / (!!(__VA_ARGS__)) }

#endif /* LNTD_UTIL_H */
