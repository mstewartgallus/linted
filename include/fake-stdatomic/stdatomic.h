/*
 * Copyright 2015 Steven Stewart-Gallus
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
#ifndef LNTD_STDATOMIC_H
#define LNTD_STDATOMIC_H

#include <stddef.h>
#include <stdint.h>

#define _Atomic(T) struct {volatile __typeof__(T) __val; }

#define ATOMIC_VAR_INIT(value)                                         \
	{                                                              \
		.__val = (value)                                       \
	}
#define atomic_init(obj, value)                                        \
	do {                                                           \
		(obj)->__val = (value);                                \
	} while (0)

enum memory_order {
	memory_order_relaxed,
	memory_order_consume,
	memory_order_acquire,
	memory_order_release,
	memory_order_acq_rel,
	memory_order_seq_cst
};
typedef enum memory_order memory_order;

void atomic_thread_fence(enum memory_order order);
void atomic_signal_fence(enum memory_order order);

#define atomic_is_lock_free(obj) 1

/*
 * 7.17.6 Atomic integer types.
 */

typedef _Atomic(_Bool) atomic_bool;
typedef _Atomic(char) atomic_char;
typedef _Atomic(signed char) atomic_schar;
typedef _Atomic(unsigned char) atomic_uchar;
typedef _Atomic(short) atomic_short;
typedef _Atomic(unsigned short) atomic_ushort;
typedef _Atomic(int) atomic_int;
typedef _Atomic(unsigned int) atomic_uint;
typedef _Atomic(long) atomic_long;
typedef _Atomic(unsigned long) atomic_ulong;
typedef _Atomic(long long) atomic_llong;
typedef _Atomic(unsigned long long) atomic_ullong;
typedef _Atomic(wchar_t) atomic_wchar_t;
typedef _Atomic(int_least8_t) atomic_int_least8_t;
typedef _Atomic(uint_least8_t) atomic_uint_least8_t;
typedef _Atomic(int_least16_t) atomic_int_least16_t;
typedef _Atomic(uint_least16_t) atomic_uint_least16_t;
typedef _Atomic(int_least32_t) atomic_int_least32_t;
typedef _Atomic(uint_least32_t) atomic_uint_least32_t;
typedef _Atomic(int_least64_t) atomic_int_least64_t;
typedef _Atomic(uint_least64_t) atomic_uint_least64_t;
typedef _Atomic(int_fast8_t) atomic_int_fast8_t;
typedef _Atomic(uint_fast8_t) atomic_uint_fast8_t;
typedef _Atomic(int_fast16_t) atomic_int_fast16_t;
typedef _Atomic(uint_fast16_t) atomic_uint_fast16_t;
typedef _Atomic(int_fast32_t) atomic_int_fast32_t;
typedef _Atomic(uint_fast32_t) atomic_uint_fast32_t;
typedef _Atomic(int_fast64_t) atomic_int_fast64_t;
typedef _Atomic(uint_fast64_t) atomic_uint_fast64_t;
typedef _Atomic(intptr_t) atomic_intptr_t;
typedef _Atomic(uintptr_t) atomic_uintptr_t;
typedef _Atomic(size_t) atomic_size_t;
typedef _Atomic(ptrdiff_t) atomic_ptrdiff_t;
typedef _Atomic(intmax_t) atomic_intmax_t;
typedef _Atomic(uintmax_t) atomic_uintmax_t;

uintmax_t atomic_compare_exchange_strong_explicit(
    void *object, void *expected, uintmax_t desired,
    enum memory_order success, enum memory_order failure);

uintmax_t atomic_compare_exchange_weak_explicit(
    void *object, void *expected, uintmax_t desired,
    enum memory_order success, enum memory_order failure);

uintmax_t atomic_exchange_explicit(void *object, uintmax_t desired,
                                   enum memory_order order);

uintmax_t atomic_fetch_add_explicit(void *object, uintmax_t desired,
                                    enum memory_order order);
uintmax_t atomic_fetch_and_explicit(void *object, uintmax_t desired,
                                    enum memory_order order);
uintmax_t atomic_fetch_or_explicit(void *object, uintmax_t desired,
                                   enum memory_order order);
uintmax_t atomic_fetch_sub_explicit(void *object, uintmax_t desired,
                                    enum memory_order order);
uintmax_t atomic_fetch_xor_explicit(void *object, uintmax_t desired,
                                    enum memory_order order);

uintmax_t atomic_load_explicit(void *object, enum memory_order order);
void atomic_store_explicit(void *object, uintmax_t desired,
                           enum memory_order order);

uintmax_t atomic_compare_exchange_strong(void *object, void *expected,
                                         uintmax_t desired);
uintmax_t atomic_compare_exchange_weak(void *object, void *expected,
                                       uintmax_t desired);

uintmax_t atomic_exchange(void *object, uintmax_t desired);

uintmax_t atomic_fetch_add(void *object, uintmax_t desired);
uintmax_t atomic_fetch_and(void *object, uintmax_t desired);
uintmax_t atomic_fetch_or(void *object, uintmax_t desired);
uintmax_t atomic_fetch_sub(void *object, uintmax_t desired);
uintmax_t atomic_fetch_xor(void *object, uintmax_t desired);

uintmax_t atomic_load(void *object);
void atomic_store(void *object, void *desired);

typedef atomic_bool atomic_flag;

#define ATOMIC_FLAG_INIT ATOMIC_VAR_INIT(0)

void atomic_flag_clear_explicit(atomic_flag *flag,
                                enum memory_order order);
_Bool atomic_flag_test_and_set_explicit(atomic_flag *flag,
                                        enum memory_order order);
void atomic_flag_clear(atomic_flag *flag);
_Bool atomic_flag_test_and_set(atomic_flag *flag);

#endif /* LNTD_STDATOMIC_H */
