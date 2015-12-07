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
#define _GNU_SOURCE

#include "config.h"

#include "lntd/trigger.h"

#include "lntd/error.h"
#include "lntd/sched.h"

#include <errno.h>
#include <stdatomic.h>
#include <time.h>
#include <unistd.h>

#if defined HAVE_POSIX_API
#include <syscall.h>

#include <linux/futex.h>
#endif

static lntd_error wait_until_different(atomic_int const *uaddr,
                                       int val);
static lntd_error hint_wakeup(atomic_int const *uaddr);

void lntd_trigger_create(struct lntd_trigger *trigger)
{
	atomic_int zero = ATOMIC_VAR_INIT(0);
	trigger->_triggered = zero;
}

void lntd_trigger_destroy(struct lntd_trigger *trigger)
{
}

void lntd_trigger_set(struct lntd_trigger *trigger)
{
	atomic_thread_fence(memory_order_release);

	atomic_store_explicit(&trigger->_triggered, 1,
	                      memory_order_relaxed);
	hint_wakeup(&trigger->_triggered);
}

void lntd_trigger_wait(struct lntd_trigger *trigger)
{
	wait_until_different(&trigger->_triggered, 0);

	atomic_store_explicit(&trigger->_triggered, 0,
	                      memory_order_release);

	atomic_thread_fence(memory_order_acquire);
}

#if defined HAVE_POSIX_API
static lntd_error futex_wait(atomic_int const *uaddr, int val,
                             struct timespec const *timeout);
static lntd_error futex_wake(unsigned *restrict wokeupp,
                             atomic_int const *uaddr, int val);

static inline lntd_error wait_until_different(atomic_int const *uaddr,
                                              int val)
{
	return futex_wait(uaddr, val, NULL);
}

static inline lntd_error hint_wakeup(atomic_int const *uaddr)
{
	return futex_wake(NULL, uaddr, 1);
}
#else
static inline lntd_error wait_until_different(atomic_int const *uaddr,
                                              int val)
{
	for (;;) {
		if (atomic_load_explicit(uaddr, memory_order_relaxed) !=
		    val)
			return 0;
		lntd_sched_light_yield();
	}

	atomic_thread_fence(memory_order_acquire);
}

static inline lntd_error hint_wakeup(atomic_int const *uaddr)
{
	return 0;
}
#endif

#if defined HAVE_POSIX_API
static inline lntd_error futex_wait(atomic_int const *uaddr, int val,
                                    struct timespec const *timeout)
{
	int xx =
	    syscall(__NR_futex, (intptr_t)uaddr, (intptr_t)FUTEX_WAIT,
	            (intptr_t)val, (intptr_t)timeout);
	if (xx < 0) {
		return errno;
	}

	return 0;
}

static inline lntd_error futex_wake(unsigned *restrict wokeupp,
                                    atomic_int const *uaddr, int val)
{
	int xx = syscall(__NR_futex, (intptr_t)uaddr,
	                 (intptr_t)FUTEX_WAKE, (intptr_t)val);
	if (xx < 0) {
		return errno;
	}

	if (wokeupp != NULL) {
		*wokeupp = xx;
	}
	return 0;
}
#endif