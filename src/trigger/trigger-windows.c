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
#include "linted/sched.h"

#include <errno.h>

static linted_error wait_until_different(int const *uaddr, int val);
static linted_error hint_wakeup(int const *uaddr);

void linted_trigger_create(struct linted_trigger *trigger)
{
	trigger->_triggered = 0;
}

void linted_trigger_destroy(struct linted_trigger *trigger)
{
}

void linted_trigger_set(struct linted_trigger *trigger)
{
	__atomic_store_n(&trigger->_triggered, 1, __ATOMIC_RELEASE);
	hint_wakeup(&trigger->_triggered);
}

void linted_trigger_wait(struct linted_trigger *trigger)
{
	wait_until_different(&trigger->_triggered, 0);

	__atomic_store_n(&trigger->_triggered, 0, __ATOMIC_RELEASE);
}

static linted_error wait_until_different(int const *uaddr, int val)
{
	for (;;) {
		if (__atomic_load_n(uaddr, __ATOMIC_ACQUIRE) != val)
			return 0;
		linted_sched_light_yield();
	}
}

static linted_error hint_wakeup(int const *uaddr)
{
	return 0;
}
