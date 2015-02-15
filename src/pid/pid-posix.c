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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 200809L

#include "linted/pid.h"

#include "linted/asynch.h"
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

struct linted_pid_task_waitid
{
	struct linted_asynch_task *parent;
	void *data;
	siginfo_t info;
	idtype_t idtype;
	id_t id;
	int options;
};

linted_error
linted_pid_task_waitid_create(struct linted_pid_task_waitid **taskp, void *data)
{
	linted_error errnum;
	struct linted_pid_task_waitid *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(&xx, task,
		                                   LINTED_ASYNCH_TASK_WAITID);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_pid_task_waitid_destroy(struct linted_pid_task_waitid *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_pid_task_waitid_data(struct linted_pid_task_waitid *task)
{
	return task->data;
}

struct linted_asynch_task *
linted_pid_task_waitid_to_asynch(struct linted_pid_task_waitid *task)
{
	return task->parent;
}

struct linted_pid_task_waitid *
linted_pid_task_waitid_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

void linted_pid_task_waitid_info(struct linted_pid_task_waitid *task,
                                 siginfo_t *info)
{
	*info = task->info;
}

void linted_pid_task_waitid_prepare(struct linted_pid_task_waitid *task,
                                    unsigned task_action, idtype_t type,
                                    id_t id, int options)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->idtype = type;
	task->id = id;
	task->options = options;
}

void linted_pid_do_waitid(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
	struct linted_pid_task_waitid *task_wait =
	    linted_asynch_task_data(task);

	linted_error errnum = 0;

	idtype_t idtype = task_wait->idtype;
	id_t id = task_wait->id;
	int options = task_wait->options;

	if (-1 == waitid(idtype, id, &task_wait->info, options)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum) {
		linted_asynch_pool_resubmit(pool, task);
		return;
	}

	linted_asynch_pool_complete(pool, task, errnum);
}

linted_error linted_pid_request_terminate(pid_t pid)
{
	if (pid < 1)
		return EINVAL;

	if (-1 == kill(pid, SIGTERM)) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

linted_error linted_pid_terminate(pid_t pid)
{
	if (pid < 1)
		return EINVAL;

	if (-1 == kill(pid, SIGKILL)) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

linted_error linted_pid_stat(pid_t pid, struct linted_pid_stat *buf)
{
	linted_error errnum = 0;

	char path[sizeof "/proc/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/stat" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/stat", (uintmax_t)pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	linted_ko stat_ko;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
		if (errnum != 0)
			return errnum;
		stat_ko = xx;
	}

	FILE *file = fdopen(stat_ko, "r");
	if (0 == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(stat_ko);

		return errnum;
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
		errnum = errno;
		/* errnum may be zero */
		goto free_line;
	}

	/* If some fields are missing just leave them to be zero */
	if (EOF == sscanf(line, "%d (" /* pid */
	                  ,
	                  &buf->pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_line;
	}

	/* Avoid troubles with processes that have names like ':-) 0 1
	 * 2 3 4 5'. procps-ng takes a different approach involving
	 * limits on the possible size of a name that I'm not actually
	 * sure works. */

	char *start = strchr(line, '(') + 1U;
	char *end = strrchr(line, ')');

	memcpy(buf->comm, start, end - start);

	if (EOF ==
	    sscanf(end, ")\n"
	                "%c\n"   /* state */
	                "%d\n"   /* ppid */
	                "%d\n"   /* pgrp */
	                "%d\n"   /* session */
	                "%d\n"   /* tty_nr */
	                "%d\n"   /* tpgid */
	                "%u\n"   /* flags */
	                "%lu\n"  /* minflt */
	                "%lu\n"  /* cminflt */
	                "%lu\n"  /* majflt */
	                "%lu\n"  /* cmajflt */
	                "%lu\n"  /* utime */
	                "%lu\n"  /* stime */
	                "%ld\n"  /* cutime */
	                "%ld\n"  /* cstime */
	                "%ld\n"  /* priority */
	                "%ld\n"  /* nice */
	                "%ld\n"  /* num_threads */
	                "%ld\n"  /* itrealvalue */
	                "%llu\n" /* starttime */
	                "%lu\n"  /* vsize */
	                "%ld\n"  /* rss */
	                "%lu\n"  /* rsslim */
	                "%lu\n"  /* startcode */
	                "%lu\n"  /* endcode */
	                "%lu\n"  /* startstack */
	                "%lu\n"  /* kstkesp */
	                "%lu\n"  /* kstkeip */
	                "%lu\n"  /* signal */
	                "%lu\n"  /* blocked */
	                "%lu\n"  /* sigignore */
	                "%lu\n"  /* sigcatch */
	                "%lu\n"  /* wchan */
	                "%lu\n"  /* nswap */
	                "%lu\n"  /* cnswap */
	                "%d\n"   /* exit_signal */
	                "%d\n"   /* processor */
	                "%u\n"   /* rt_priority */
	                "%u\n"   /* policy */
	                "%llu\n" /* delayacct_blkio_ticks */
	                "%lu\n"  /* guest_time */
	                "%ld\n"  /* cguest_time */
	           ,
	           &buf->state, &buf->ppid, &buf->pgrp, &buf->session,
	           &buf->tty_nr, &buf->tpgid, &buf->flags, &buf->minflt,
	           &buf->cminflt, &buf->majflt, &buf->cmajflt, &buf->utime,
	           &buf->stime, &buf->cutime, &buf->cstime, &buf->priority,
	           &buf->nice, &buf->num_threads, &buf->itrealvalue,
	           &buf->starttime, &buf->vsize, &buf->rss, &buf->rsslim,
	           &buf->startcode, &buf->endcode, &buf->startstack,
	           &buf->kstkesp, &buf->kstkeip, &buf->signal, &buf->blocked,
	           &buf->sigignore, &buf->sigcatch, &buf->wchan, &buf->nswap,
	           &buf->cnswap, &buf->exit_signal, &buf->processor,
	           &buf->rt_priority, &buf->policy, &buf->delayacct_blkio_ticks,
	           &buf->guest_time, &buf->cguest_time)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_line;
	}

free_line:
	linted_mem_free(line);

	if (EOF == fclose(file)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return errnum;
}

linted_error linted_pid_children(pid_t pid, pid_t **childrenp, size_t *lenp)
{
	linted_error errnum;

	char path[sizeof "/proc/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/task/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/children" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/task/%" PRIuMAX "/children",
	                  (uintmax_t)pid, (uintmax_t)pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	linted_ko children_ko;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
		if (errnum != 0)
			return errnum;
		children_ko = xx;
	}

	FILE *file = fdopen(children_ko, "r");
	if (0 == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(children_ko);

		return errnum;
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
		errnum = errno;
		/* May be zero */
		eof = true;
	}

	if (EOF == fclose(file)) {
		if (0 == errnum) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

	if (errnum != 0) {
		linted_mem_free(buf);
		return errnum;
	}

	if (eof) {
		linted_mem_free(buf);
		buf = 0;
	}

	size_t ii = 0U;
	char const *start = buf;
	pid_t *children = 0;

	if (0 == buf)
		goto finish;

	for (;;) {
		errno = 0;
		pid_t child = strtol(start, 0, 10);
		errnum = errno;
		if (errnum != 0)
			goto free_buf;

		{
			void *xx;
			errnum = linted_mem_realloc_array(
			    &xx, children, ii + 1U, sizeof children[0U]);
			if (errnum != 0)
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

	if (errnum != 0) {
		linted_mem_free(children);
		return errnum;
	}

finish:
	*lenp = ii;
	*childrenp = children;

	return errnum;
}
