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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_PID_H
#define LINTED_PID_H

#include "linted/error.h"

#include <sys/types.h>

#if defined HAVE_POSIX_API
#include <signal.h>
#include <sys/wait.h>
#endif

/**
 * @file
 *
 * System processes.
 *
 * @todo Get rid of conditional platform test
 */

#define LINTED_PID_COMM_MAX 16U

struct linted_asynch_pool;
struct linted_asynch_task;

struct linted_pid_task_waitid;

struct linted_pid_stat
{
	pid_t pid;
	char comm[LINTED_PID_COMM_MAX + 1U];
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

linted_error
linted_pid_task_waitid_create(struct linted_pid_task_waitid **taskp,
                              void *data);
void
linted_pid_task_waitid_destroy(struct linted_pid_task_waitid *task);

#if defined HAVE_POSIX_API
void linted_pid_task_waitid_prepare(struct linted_pid_task_waitid *task,
                                    unsigned task_action, idtype_t type,
                                    id_t id, int options);
void linted_pid_task_waitid_info(struct linted_pid_task_waitid *task,
                                 siginfo_t *info);
#endif
void *linted_pid_task_waitid_data(struct linted_pid_task_waitid *task);
struct linted_asynch_task *
linted_pid_task_waitid_to_asynch(struct linted_pid_task_waitid *task);
struct linted_pid_task_waitid *
linted_pid_task_waitid_from_asynch(struct linted_asynch_task *task);

void linted_pid_do_waitid(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);

linted_error linted_pid_kill(pid_t pid, int signo);
linted_error linted_pid_terminate(pid_t pid);
linted_error linted_pid_stat(pid_t pid, struct linted_pid_stat *buf);
linted_error linted_pid_children(pid_t pid, pid_t **childrenp,
                                 size_t *lenp);

#endif /* LINTED_PID_H */
