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
#define _GNU_SOURCE 1

#include "config.h"

#include "linted/pid.h"

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#if !defined HAVE_PTHREAD_SETNAME_NP && defined HAVE_SYS_PRCTL_H
#include "linted/prctl.h"
#endif

#include <dirent.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* 2^(bits - 1) - 1 */
/* Sadly, this assumes a twos complement implementation */
#define PID_MAX                                                        \
	((pid_t)((UINTMAX_C(1)                                         \
	          << (uintmax_t)(sizeof(pid_t) * CHAR_BIT - 1U)) -     \
	         1U))

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
	return linted_pid_kill(pid, SIGKILL);
}

linted_error linted_pid_continue(linted_pid pid)
{
	return linted_pid_kill(pid, SIGCONT);
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

/**
 * @bug Only obtains children of the main thread.
 */
linted_error linted_pid_children(linted_pid pid, linted_pid **childrenp,
                                 size_t *lenp)
{
	linted_error err = 0;

	linted_ko task_ko;
	{
		char path[sizeof "/proc/" - 1U +
		          LINTED_NUMBER_TYPE_STRING_SIZE(linted_pid) +
		          sizeof "/task" - 1U + 1U];
		if (-1 == sprintf(path, "/proc/%" PRIuMAX "/task",
		                  (uintmax_t)pid)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			return err;
		}

		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, path,
		                     LINTED_KO_RDONLY);
		if (err != 0)
			return err;
		task_ko = xx;
	}

	DIR *task_dir = fdopendir(task_ko);
	if (0 == task_dir) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(task_ko);

		return err;
	}

	size_t num_tasks = 0U;
	FILE **tasks = 0;
	for (;;) {
		errno = 0;
		struct dirent *entry = readdir(task_dir);
		if (0 == entry) {
			err = errno;
			if (err != 0)
				goto close_tasks;
			break;
		}

		char const *name = entry->d_name;
		if (0 == strcmp(".", name))
			continue;
		if (0 == strcmp("..", name))
			continue;

		{
			void *xx;
			err = linted_mem_realloc_array(
			    &xx, tasks, num_tasks + 1U,
			    sizeof tasks[0U]);
			if (err != 0)
				goto close_tasks;
			tasks = xx;
		}

		char path[LINTED_NUMBER_TYPE_STRING_SIZE(linted_pid) +
		          sizeof "/children" - 1U + 1U];
		if (-1 == sprintf(path, "%s/children", name)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto close_tasks;
		}

		linted_ko this_task;
		{
			linted_ko xx;
			err = linted_ko_open(&xx, task_ko, path,
			                     LINTED_KO_RDONLY);
			if (ENOENT == err) {
				err = ESRCH;
				goto close_tasks;
			}
			if (err != 0)
				goto close_tasks;
			this_task = xx;
		}

		FILE *file = fdopen(this_task, "r");
		if (0 == file) {
			err = errno;
			LINTED_ASSUME(err != 0);

			linted_ko_close(task_ko);

			goto close_tasks;
		}

		tasks[num_tasks] = file;

		++num_tasks;
	}

	size_t num_children = 0U;
	linted_pid *children = 0;

	char *buf = 0;
	size_t buf_size = 0U;
	for (size_t ii = 0U; ii < num_tasks; ++ii) {
		FILE *task = tasks[ii];

		/* Get the child all at once to avoid raciness. */
		bool eof = false;
		ssize_t zz;
		{
			char *xx = buf;
			size_t yy = buf_size;

			errno = 0;
			zz = getline(&xx, &yy, task);
			buf = xx;
			buf_size = yy;
		}

		if (-1 == zz) {
			err = errno;
			/* May be zero */
			eof = true;
		}

		if (err != 0)
			break;

		if (eof)
			continue;

		char const *start = buf;

		for (;;) {
			errno = 0;
			linted_pid child = strtol(start, 0, 10);
			err = errno;
			if (err != 0)
				goto free_buf;

			{
				void *xx;
				err = linted_mem_realloc_array(
				    &xx, children, num_children + 1U,
				    sizeof children[0U]);
				if (err != 0)
					goto free_buf;
				children = xx;
			}
			children[num_children] = child;
			++num_children;

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
	}
free_buf:
	linted_mem_free(buf);

	if (0 == err) {
		*lenp = num_children;
		*childrenp = children;
	}

	if (err != 0) {
		linted_mem_free(children);
	}

close_tasks:
	for (size_t ii = 0U; ii < num_tasks; ++ii) {
		fclose(tasks[ii]);
	}
	linted_mem_free(tasks);

	closedir(task_dir);

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

#if defined HAVE_PTHREAD_SETNAME_NP
linted_error linted_pid_name(char const *name)
{
	return pthread_setname_np(pthread_self(), name);
}
#elif defined HAVE_SYS_PRCTL_H
linted_error linted_pid_name(char const *name)
{
	return linted_prctl_set_name(name);
}
#else
linted_error linted_pid_name(char const *name)
{
	return 0;
}
#endif
