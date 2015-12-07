/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#ifndef LNTD_PID_H
#define LNTD_PID_H

#include "lntd/error.h"

#include <stddef.h>
#include <stdint.h>

#if defined HAVE_POSIX_API
typedef uintmax_t lntd_pid;
#else
/* `DWORD`s are 32-bit unsigned integers */
typedef uint_fast32_t lntd_pid;
#endif

/**
 * @file
 *
 * System processes.
 */

#define LNTD_PID_COMM_MAX 16U

struct lntd_pid_stat {
	lntd_pid pid;
	char comm[LNTD_PID_COMM_MAX + 1U];
	char state;
	int ppid;
	int pgrp;
	int session;
	int tty_nr;
	int tpgid;
	unsigned flags;
	unsigned long minflt;
	unsigned long cminflt;
	unsigned long majflt;
	unsigned long cmajflt;
	unsigned long utime;
	unsigned long stime;
	long cutime;
	long cstime;
	long priority;
	long nice;
	long num_threads;
	long itrealvalue;
	unsigned long long starttime;
	unsigned long vsize;
	long rss;
	unsigned long rsslim;
	unsigned long startcode;
	unsigned long endcode;
	unsigned long startstack;
	unsigned long kstkesp;
	unsigned long kstkeip;
	unsigned long signal;
	unsigned long blocked;
	unsigned long sigignore;
	unsigned long sigcatch;
	unsigned long wchan;
	unsigned long nswap;
	unsigned long cnswap;
	int exit_signal;
	int processor;
	unsigned rt_priority;
	unsigned policy;
	unsigned long long delayacct_blkio_ticks;
	unsigned long guest_time;
	long cguest_time;
};

lntd_error lntd_pid_kill(lntd_pid pid, int signo);
lntd_error lntd_pid_terminate(lntd_pid pid);
lntd_error lntd_pid_continue(lntd_pid pid);

/**
 * @warning The `comm` field is attacker controllable. See
 *          go-beyond.org/post/argv-for-no-fun-and-no-profit for
 *          potential problems naive display of it can cause.  Not only
 *          could a process be named UTF-8 hackery but it could also use
 *          nondisplayable characters or even terminal control
 * sequences.
 */
lntd_error lntd_pid_stat(lntd_pid pid, struct lntd_pid_stat *buf);
lntd_error lntd_pid_children(lntd_pid pid, lntd_pid **childrenp,
                             size_t *lenp);

lntd_pid lntd_pid_get_pid(void);

lntd_error lntd_pid_from_str(char const *str, lntd_pid *pidp);

lntd_error lntd_pid_name(char const *name);

#endif /* LNTD_PID_H */
