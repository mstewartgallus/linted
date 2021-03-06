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
#ifndef LNTD_PRCTL_H
#define LNTD_PRCTL_H

#include "lntd/error.h"
#include "lntd/util.h"

#include <stdint.h>
#include <sys/prctl.h>

static inline lntd_error lntd_prctl_set_death_sig(int signum);
static inline lntd_error lntd_prctl_set_name(char const *name);
static inline lntd_error lntd_prctl_set_child_subreaper(_Bool v);
static inline lntd_error lntd_prctl_set_timerslack(unsigned long v);
static inline lntd_error lntd_prctl_set_no_new_privs(_Bool v);

#ifdef PR_SET_PDEATHSIG
static inline lntd_error lntd_prctl_set_death_sig(int signum)
{
	lntd_error err;

	if (-1 == prctl(PR_SET_PDEATHSIG, (unsigned long)signum, 0UL,
	                0UL, 0UL)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PR_SET_NAME
static inline lntd_error lntd_prctl_set_name(char const *name)
{
	lntd_error err;

	if (-1 ==
	    prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL)) {
		err = errno;
		LNTD_ASSUME(err != 0);

		LNTD_ASSERT(err != LNTD_ERROR_INVALID_PARAMETER);

		return err;
	}
	return 0;
}
#endif

#ifdef PR_SET_CHILD_SUBREAPER
static inline lntd_error lntd_prctl_set_child_subreaper(_Bool v)
{
	lntd_error err;

	if (-1 == prctl(PR_SET_CHILD_SUBREAPER, (unsigned long)v, 0UL,
	                0UL, 0UL)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PR_SET_TIMERSLACK
static inline lntd_error lntd_prctl_set_timerslack(unsigned long v)
{
	lntd_error err;

	if (-1 ==
	    prctl(PR_SET_TIMERSLACK, (unsigned long)v, 0UL, 0UL, 0UL)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PR_SET_NO_NEW_PRIVS
static lntd_error lntd_prctl_set_no_new_privs(_Bool b)
{
	lntd_error err;

	if (-1 == prctl(PR_SET_NO_NEW_PRIVS, (unsigned long)b, 0UL, 0UL,
	                0UL)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		LNTD_ASSERT(err != EINVAL);
		return err;
	}

	return 0;
}
#endif

#endif /* LNTD_PRCTL_H */
