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
#define _POSIX_C_SOURCE 200809L

#include "linted/pid.h"

#include "linted/async.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/* 2^(bits - 1) - 1 */
/* Sadly, this assumes a twos complement implementation */
#define PID_MAX                                                        \
	((pid_t)((UINTMAX_C(1)                                         \
	          << (uintmax_t)(sizeof(pid_t) * CHAR_BIT - 1U)) -     \
	         1U))

struct linted_pid_task_waitid {
	struct linted_async_task *parent;
	void *data;
	siginfo_t info;
	idtype_t idtype;
	id_t id;
	int options;
};

linted_error
linted_pid_task_waitid_create(struct linted_pid_task_waitid **taskp,
                              void *data)
{
	linted_error err;
	struct linted_pid_task_waitid *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_async_task *parent;
	{
		struct linted_async_task *xx;
		err = linted_async_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_WAITID);
		if (err != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return err;
}

void linted_pid_task_waitid_destroy(struct linted_pid_task_waitid *task)
{
	linted_async_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_pid_task_waitid_data(struct linted_pid_task_waitid *task)
{
	return task->data;
}

struct linted_async_task *
linted_pid_task_waitid_to_async(struct linted_pid_task_waitid *task)
{
	return task->parent;
}

struct linted_pid_task_waitid *
linted_pid_task_waitid_from_async(struct linted_async_task *task)
{
	return linted_async_task_data(task);
}

void linted_pid_task_waitid_info(struct linted_pid_task_waitid *task,
                                 siginfo_t *info)
{
	*info = task->info;
}

void linted_pid_task_waitid_prepare(struct linted_pid_task_waitid *task,
                                    union linted_async_ck task_ck,
                                    idtype_t type, id_t id, int options)
{
	linted_async_task_prepare(task->parent, task_ck);
	task->idtype = type;
	task->id = id;
	task->options = options;
}

void linted_pid_do_waitid(struct linted_async_pool *pool,
                          struct linted_async_task *task)
{
	struct linted_pid_task_waitid *task_wait =
	    linted_async_task_data(task);

	linted_error err = 0;

	idtype_t idtype = task_wait->idtype;
	id_t id = task_wait->id;
	int options = task_wait->options;

	if (-1 == waitid(idtype, id, &task_wait->info, options)) {
		err = errno;
		LINTED_ASSUME(err != 0);
	}

	if (EINTR == err) {
		linted_async_pool_resubmit(pool, task);
		return;
	}

	linted_async_pool_complete(pool, task, err);
}

linted_error linted_pid_kill(linted_pid pid, int signo)
{
	if (pid < 1)
		return LINTED_ERROR_INVALID_PARAMETER;
	if (signo < 1)
		return LINTED_ERROR_INVALID_PARAMETER;

	if (-1 == kill(pid, signo)) {
		linted_error err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}

linted_error linted_pid_terminate(linted_pid pid)
{
	if (pid < 1)
		return LINTED_ERROR_INVALID_PARAMETER;

	if (-1 == kill(pid, SIGKILL)) {
		linted_error err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}

linted_error linted_pid_stat(linted_pid pid,
                             struct linted_pid_stat *buf)
{
	linted_error err = 0;

	linted_ko stat_ko;
	{
		char path[sizeof "/proc/" - 1U +
		          LINTED_NUMBER_TYPE_STRING_SIZE(linted_pid) +
		          sizeof "/stat" - 1U + 1U];
		if (-1 == sprintf(path, "/proc/%" PRIuMAX "/stat",
		                  (uintmax_t)pid)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			return err;
		}

		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, path,
		                     LINTED_KO_RDONLY);
		if (ENOENT == err)
			return ESRCH;
		if (err != 0)
			return err;
		stat_ko = xx;
	}

	FILE *file = fdopen(stat_ko, "r");
	if (0 == file) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(stat_ko);

		return err;
	}

	memset(buf, 0, sizeof *buf);

	char *line;
	ssize_t zz;
	{
		char *xx = 0;
		size_t yy = 0U;
		errno = 0;
		zz = getline(&xx, &yy, file);
		line = xx;
	}

	if (-1 == zz) {
		err = errno;
		/* err may be zero */
		goto free_line;
	}

	/* If some fields are missing just leave them to be zero */
	{
		linted_pid xx;
		if (EOF == sscanf(line, "%" PRIuMAX " (" /* pid */
		                  ,
		                  &xx)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto free_line;
		}
		buf->pid = xx;
	}

	/* Avoid troubles with processes that have names like ':-) 0 1
	 * 2 3 4 5'. procps-ng takes a different approach involving
	 * limits on the possible size of a name that I'm not actually
	 * sure works. */

	char *start = strchr(line, '(') + 1U;
	char *end = strrchr(line, ')');

	memcpy(buf->comm, start, end - start);

	{
		linted_pid ppid;
		linted_pid pgrp;
		linted_pid session;
		linted_pid tpgid;
		if (EOF ==
		    sscanf(end, ")\n"
		                "%c\n"           /* state */
		                "%" PRIuMAX "\n" /* ppid */
		                "%" PRIuMAX "\n" /* pgrp */
		                "%" PRIuMAX "\n" /* session */
		                "%d\n"           /* tty_nr */
		                "%" PRIuMAX "\n" /* tpgid */
		                "%u\n"           /* flags */
		                "%lu\n"          /* minflt */
		                "%lu\n"          /* cminflt */
		                "%lu\n"          /* majflt */
		                "%lu\n"          /* cmajflt */
		                "%lu\n"          /* utime */
		                "%lu\n"          /* stime */
		                "%ld\n"          /* cutime */
		                "%ld\n"          /* cstime */
		                "%ld\n"          /* priority */
		                "%ld\n"          /* nice */
		                "%ld\n"          /* num_threads */
		                "%ld\n"          /* itrealvalue */
		                "%llu\n"         /* starttime */
		                "%lu\n"          /* vsize */
		                "%ld\n"          /* rss */
		                "%lu\n"          /* rsslim */
		                "%lu\n"          /* startcode */
		                "%lu\n"          /* endcode */
		                "%lu\n"          /* startstack */
		                "%lu\n"          /* kstkesp */
		                "%lu\n"          /* kstkeip */
		                "%lu\n"          /* signal */
		                "%lu\n"          /* blocked */
		                "%lu\n"          /* sigignore */
		                "%lu\n"          /* sigcatch */
		                "%lu\n"          /* wchan */
		                "%lu\n"          /* nswap */
		                "%lu\n"          /* cnswap */
		                "%d\n"           /* exit_signal */
		                "%d\n"           /* processor */
		                "%u\n"           /* rt_priority */
		                "%u\n"           /* policy */
		                "%llu\n" /* delayacct_blkio_ticks */
		                "%lu\n"  /* guest_time */
		                "%ld\n"  /* cguest_time */
		           ,
		           &buf->state, &ppid, &pgrp, &session,
		           &buf->tty_nr, &tpgid, &buf->flags,
		           &buf->minflt, &buf->cminflt, &buf->majflt,
		           &buf->cmajflt, &buf->utime, &buf->stime,
		           &buf->cutime, &buf->cstime, &buf->priority,
		           &buf->nice, &buf->num_threads,
		           &buf->itrealvalue, &buf->starttime,
		           &buf->vsize, &buf->rss, &buf->rsslim,
		           &buf->startcode, &buf->endcode,
		           &buf->startstack, &buf->kstkesp,
		           &buf->kstkeip, &buf->signal, &buf->blocked,
		           &buf->sigignore, &buf->sigcatch, &buf->wchan,
		           &buf->nswap, &buf->cnswap, &buf->exit_signal,
		           &buf->processor, &buf->rt_priority,
		           &buf->policy, &buf->delayacct_blkio_ticks,
		           &buf->guest_time, &buf->cguest_time)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto free_line;
		}

		buf->ppid = ppid;
		buf->pgrp = pgrp;
		buf->session = session;
		buf->tpgid = tpgid;
	}

free_line:
	linted_mem_free(line);

	if (EOF == fclose(file)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return err;
}

linted_error linted_pid_children(linted_pid pid, linted_pid **childrenp,
                                 size_t *lenp)
{
	linted_error err;

	linted_ko children_ko;
	{
		char path[sizeof "/proc/" - 1U +
		          LINTED_NUMBER_TYPE_STRING_SIZE(linted_pid) +
		          sizeof "/task/" - 1U +
		          LINTED_NUMBER_TYPE_STRING_SIZE(linted_pid) +
		          sizeof "/children" - 1U + 1U];
		if (-1 == sprintf(path, "/proc/%" PRIuMAX
		                        "/task/%" PRIuMAX "/children",
		                  (uintmax_t)pid, (uintmax_t)pid)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			return err;
		}

		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, path,
		                     LINTED_KO_RDONLY);
		if (ENOENT == err)
			return ESRCH;
		if (err != 0)
			return err;
		children_ko = xx;
	}

	FILE *file = fdopen(children_ko, "r");
	if (0 == file) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(children_ko);

		return err;
	}

	/* Get the child all at once to avoid raciness. */
	char *buf = 0;
	bool eof = false;
	ssize_t zz;
	{
		char *xx = buf;
		size_t yy = 0U;

		errno = 0;
		zz = getline(&xx, &yy, file);
		buf = xx;
	}

	if (-1 == zz) {
		err = errno;
		/* May be zero */
		eof = true;
	}

	if (EOF == fclose(file)) {
		if (0 == err) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}
	}

	if (err != 0) {
		linted_mem_free(buf);
		return err;
	}

	if (eof) {
		linted_mem_free(buf);
		buf = 0;
	}

	size_t ii = 0U;
	char const *start = buf;
	linted_pid *children = 0;

	if (0 == buf)
		goto finish;

	for (;;) {
		errno = 0;
		linted_pid child = strtol(start, 0, 10);
		err = errno;
		if (err != 0)
			goto free_buf;

		{
			void *xx;
			err = linted_mem_realloc_array(
			    &xx, children, ii + 1U,
			    sizeof children[0U]);
			if (err != 0)
				goto free_buf;
			children = xx;
		}
		children[ii] = child;
		++ii;

		start = strchr(start, ' ');
		if (0 == start)
			break;
		if ('\n' == *start)
			break;
		if ('\0' == *start)
			break;
		++start;
		if ('\n' == *start)
			break;
		if ('\0' == *start)
			break;
	}

free_buf:
	linted_mem_free(buf);

	if (err != 0) {
		linted_mem_free(children);
		return err;
	}

finish:
	*lenp = ii;
	*childrenp = children;

	return err;
}

linted_pid linted_pid_get_pid(void)
{
	return getpid();
}

linted_error linted_pid_from_str(char const *str, linted_pid *pidp)
{
	size_t digits_count = strlen(str);

	linted_pid pid;

	if ('0' == str[0U]) {
		pid = 0;
		goto write_pid;
	}

	uintmax_t maybe_pid = 0U;
	uintmax_t digit_place = 1U;
	for (size_t ii = digits_count; ii != 0U;) {
		--ii;
		char digit = str[ii];

		if (digit < '0' || digit > '9')
			return LINTED_ERROR_INVALID_PARAMETER;

		unsigned long digit_val = digit - '0';

		maybe_pid += digit_val * digit_place;
		if (maybe_pid > PID_MAX)
			return ERANGE;
		digit_place *= 10U;
	}
	pid = maybe_pid;

write_pid:
	*pidp = pid;
	return 0;
}
