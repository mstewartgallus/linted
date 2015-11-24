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

#include "linted/trigger.h"

#include "linted/error.h"

#include <errno.h>
#include <stdatomic.h>
#include <syscall.h>
#include <time.h>
#include <unistd.h>

#include <linux/futex.h>

static linted_error wait_until_different(_Atomic int const *uaddr,
                                         int val);
static linted_error hint_wakeup(_Atomic int const *uaddr);

static linted_error futex_wait(_Atomic int const *uaddr, int val,
                               struct timespec const *timeout);
static linted_error futex_wake(unsigned *restrict wokeupp,
                               _Atomic int const *uaddr, int val);

void linted_trigger_create(struct linted_trigger *trigger)
{
	trigger->_triggered = 0;
}

void linted_trigger_destroy(struct linted_trigger *trigger)
{
}

void linted_trigger_set(struct linted_trigger *trigger)
{
	atomic_store_explicit(&trigger->_triggered, 1,
	                      memory_order_release);
	hint_wakeup(&trigger->_triggered);
}

void linted_trigger_wait(struct linted_trigger *trigger)
{
	wait_until_different(&trigger->_triggered, 0);

	atomic_store_explicit(&trigger->_triggered, 0,
	                      memory_order_release);
}

static linted_error wait_until_different(_Atomic int const *uaddr,
                                         int val)
{
	return futex_wait(uaddr, val, NULL);
}

static linted_error hint_wakeup(_Atomic int const *uaddr)
{
	return futex_wake(NULL, uaddr, 1);
}

static linted_error futex_wait(_Atomic int const *uaddr, int val,
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

static linted_error futex_wake(unsigned *restrict wokeupp,
                               _Atomic int const *uaddr, int val)
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
