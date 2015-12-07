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
#ifndef LNTD_PTRACE_H
#define LNTD_PTRACE_H

#include "lntd/error.h"
#include "lntd/pid.h"
#include "lntd/util.h"

#include <stdint.h>

#if defined HAVE_POSIX_API
#include <sys/ptrace.h>
#endif

static inline lntd_error lntd_ptrace_detach(lntd_pid pid, int signo);
static inline lntd_error lntd_ptrace_setoptions(lntd_pid pid,
                                                unsigned options);
static inline lntd_error lntd_ptrace_geteventmsg(lntd_pid pid,
                                                 unsigned long *msg);
static inline lntd_error lntd_ptrace_getsiginfo(lntd_pid pid,
                                                void *infop);
static inline lntd_error lntd_ptrace_seize(lntd_pid pid,
                                           uint_fast32_t options);
static inline lntd_error lntd_ptrace_cont(lntd_pid pid, int signo);

#ifdef PT_DETACH
static inline lntd_error lntd_ptrace_detach(lntd_pid pid, int signo)
{
	lntd_error err;

	if (-1 == ptrace(PT_DETACH, (pid_t)pid, (void *)0,
	                 (void *)(intptr_t)signo)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_SETOPTIONS
static inline lntd_error lntd_ptrace_setoptions(lntd_pid pid,
                                                unsigned options)
{
	lntd_error err;

	if (-1 == ptrace(PT_SETOPTIONS, (pid_t)pid, (void *)0,
	                 (void *)(uintptr_t)options)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_GETEVENTMSG
static inline lntd_error lntd_ptrace_geteventmsg(lntd_pid pid,
                                                 unsigned long *msg)
{
	lntd_error err;

	if (-1 == ptrace(PT_GETEVENTMSG, (pid_t)pid, (void *)0,
	                 (void *)(uintptr_t)msg)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_GETSIGINFO
static inline lntd_error lntd_ptrace_getsiginfo(lntd_pid pid,
                                                void *infop)
{
	lntd_error err;

	if (-1 == ptrace(PT_GETSIGINFO, (pid_t)pid, (void *)0, infop)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PTRACE_SEIZE
static inline lntd_error lntd_ptrace_seize(lntd_pid pid,
                                           uint_fast32_t options)
{
	lntd_error err;

	if (-1 == ptrace(PTRACE_SEIZE, (pid_t)pid, (void *)0,
	                 (void *)(uintptr_t)options)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#ifdef PT_CONTINUE
static inline lntd_error lntd_ptrace_cont(lntd_pid pid, int signo)
{
	lntd_error err;

	if (-1 == ptrace(PT_CONTINUE, (pid_t)pid, (void *)0,
	                 (void *)(intptr_t)signo)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}
#endif

#endif /* LNTD_PTRACE_H */
