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
#ifndef LINTED_PTRACE_H
#define LINTED_PTRACE_H

#include "linted/error.h"
#include "linted/pid.h"
#include "linted/util.h"

#include <stdint.h>

#if defined HAVE_POSIX_API
#include <sys/ptrace.h>
#endif

static inline linted_error linted_ptrace_detach(linted_pid pid,
                                                int signo);
static inline linted_error linted_ptrace_setoptions(linted_pid pid,
                                                    unsigned options);
static inline linted_error
linted_ptrace_geteventmsg(linted_pid pid, unsigned long *msg);
static inline linted_error linted_ptrace_getsiginfo(linted_pid pid,
                                                    void *infop);
static inline linted_error linted_ptrace_seize(linted_pid pid,
                                               uint_fast32_t options);
static inline linted_error linted_ptrace_cont(linted_pid pid,
                                              int signo);

#ifdef PT_DETACH
static inline linted_error linted_ptrace_detach(linted_pid pid,
                                                int signo)
{
	linted_error err;

	if (-1 == ptrace(PT_DETACH, (pid_t)pid, (void *)0,
	                 (void *)(intptr_t)signo)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_SETOPTIONS
static inline linted_error linted_ptrace_setoptions(linted_pid pid,
                                                    unsigned options)
{
	linted_error err;

	if (-1 == ptrace(PT_SETOPTIONS, (pid_t)pid, (void *)0,
	                 (void *)(uintptr_t)options)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_GETEVENTMSG
static inline linted_error linted_ptrace_geteventmsg(linted_pid pid,
                                                     unsigned long *msg)
{
	linted_error err;

	if (-1 == ptrace(PT_GETEVENTMSG, (pid_t)pid, (void *)0,
	                 (void *)(uintptr_t)msg)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_GETSIGINFO
static inline linted_error linted_ptrace_getsiginfo(linted_pid pid,
                                                    void *infop)
{
	linted_error err;

	if (-1 == ptrace(PT_GETSIGINFO, (pid_t)pid, (void *)0, infop)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PTRACE_SEIZE
static inline linted_error linted_ptrace_seize(linted_pid pid,
                                               uint_fast32_t options)
{
	linted_error err;

	if (-1 == ptrace(PTRACE_SEIZE, (pid_t)pid, (void *)0,
	                 (void *)(uintptr_t)options)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_CONTINUE
static inline linted_error linted_ptrace_cont(linted_pid pid, int signo)
{
	linted_error err;

	if (-1 == ptrace(PT_CONTINUE, (pid_t)pid, (void *)0,
	                 (void *)(intptr_t)signo)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#endif /* LINTED_PTRACE_H */
