/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

#include "config.h"

#include "linted/io.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/poll.h>
#include <unistd.h>

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(short revents);

linted_error linted_io_read_all(linted_ko ko, size_t *bytes_read_out, void *buf,
                                size_t size)
{
	size_t bytes_read = 0U;
	size_t bytes_left = size;

	linted_error errnum = 0;
	for (;;) {
		for (;;) {
			ssize_t result =
			    read(ko, (char *)buf + bytes_read, bytes_left);
			if (-1 == result) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);

				if (EINTR == errnum)
					continue;

				break;
			}

			size_t bytes_read_delta = result;
			if (0U == bytes_read_delta)
				break;

			bytes_read += bytes_read_delta;
			bytes_left -= bytes_read_delta;
			if (0U == bytes_left)
				break;
		}

		if (errnum != EAGAIN && errnum != EWOULDBLOCK)
			break;

		short revents = 0;
		do {
			short xx;
			errnum = poll_one(ko, POLLIN, &xx);
			if (0 == errnum)
				revents = xx;
		} while (EINTR == errnum);
		if (errnum != 0)
			break;

		errnum = check_for_poll_error(revents);
		if (errnum != 0)
			break;
	}

	if (bytes_read_out != NULL)
		*bytes_read_out = bytes_read;
	return errnum;
}

linted_error linted_io_write_all(linted_ko ko, size_t *bytes_wrote_out,
                                 void const *buf, size_t size)
{
	linted_error errnum = 0;
	size_t bytes_wrote = 0U;
	size_t bytes_left = size;

	sigset_t oldset;
	/* Get EPIPEs */
	/* SIGPIPE may not be blocked already */
	/* Reuse oldset to save on stack space */
	sigemptyset(&oldset);
	sigaddset(&oldset, SIGPIPE);

	errnum = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (errnum != 0)
		return errnum;

	for (;;) {
		for (;;) {
			ssize_t result =
			    write(ko, (char *)buf + bytes_wrote, bytes_left);
			if (-1 == result) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);

				if (EINTR == errnum)
					continue;

				break;
			}

			size_t bytes_wrote_delta = result;

			bytes_wrote += bytes_wrote_delta;
			bytes_left -= bytes_wrote_delta;
			if (0U == bytes_left)
				break;
		}

		if (errnum != EAGAIN && errnum != EWOULDBLOCK)
			break;

		short revents = 0;
		do {
			short xx;
			errnum = poll_one(ko, POLLOUT, &xx);
			if (0 == errnum)
				revents = xx;
		} while (EINTR == errnum);
		if (errnum != 0)
			break;

		errnum = check_for_poll_error(revents);
		if (errnum != 0)
			break;
	}

	/* Consume SIGPIPEs */
	{
		sigset_t sigpipeset;

		sigemptyset(&sigpipeset);
		sigaddset(&sigpipeset, SIGPIPE);

		linted_error wait_errnum;
		do {
			struct timespec timeout = { 0 };

			if (-1 == sigtimedwait(&sigpipeset, NULL, &timeout)) {
				wait_errnum = errno;
				LINTED_ASSUME(wait_errnum != 0);
			} else {
				wait_errnum = 0;
			}
		} while (EINTR == wait_errnum);
		if (wait_errnum != 0 && wait_errnum != EAGAIN) {
			if (0 == errnum)
				errnum = wait_errnum;
		}
	}

	{
		linted_error mask_errnum =
		    pthread_sigmask(SIG_SETMASK, &oldset, NULL);
		if (0 == errnum)
			errnum = mask_errnum;
	}

	if (bytes_wrote_out != NULL)
		*bytes_wrote_out = bytes_wrote;
	return errnum;
}

linted_error linted_io_write_str(linted_ko ko, size_t *bytes_wrote,
                                 struct linted_str str)
{
	return linted_io_write_all(ko, bytes_wrote, str.bytes, str.size);
}

linted_error linted_io_write_string(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s)
{
	return linted_io_write_all(ko, bytes_wrote_out, s, strlen(s));
}

linted_error linted_io_write_format(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *format_str, ...)
{
	linted_error errnum = 0;

	va_list ap;
	va_start(ap, format_str);

	int bytes = vdprintf(ko, format_str, ap);
	if (-1 == bytes) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_va_list;
	}

	if (bytes_wrote_out != NULL)
		*bytes_wrote_out = bytes;

free_va_list:
	va_end(ap);

	return errnum;
}

static linted_error poll_one(linted_ko ko, short events, short *reventsp)
{
	linted_error errnum;

	short revents;
	{
		struct pollfd pollfd = { .fd = ko, .events = events };
		int poll_status = poll(&pollfd, 1U, -1);
		if (-1 == poll_status)
			goto poll_failed;

		revents = pollfd.revents;
		goto poll_succeeded;
	}

poll_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);
	return errnum;

poll_succeeded:
	*reventsp = revents;
	return 0;
}

static linted_error check_for_poll_error(short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = EBADF;

	return errnum;
}
