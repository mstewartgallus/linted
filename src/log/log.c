/*
 * Copyright 2014 Steven Stewart-Gallus
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
#include "config.h"

#include "linted/log.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <mqueue.h>
#include <sys/poll.h>

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);

/**
 * @todo Make asynchronous
 */
linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len)
{
	linted_error errnum = 0;

	for (;;) {
		int send_status;
		do {
			send_status = mq_send(log, msg_ptr, msg_len, 0);
			if (0 == send_status) {
				errnum = 0;
			} else {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			}
		} while (EINTR == errnum);
		if (errnum != EAGAIN)
			break;

		short revents;
		for (;;) {
			short xx;
			errnum = poll_one(log, POLLOUT, &xx);
			switch (errnum) {
			case EINTR:
				continue;

			case 0:
				revents = xx;
				goto check_for_poll_error;

			default:
				return errnum;
			}
		}

	check_for_poll_error:
		errnum = check_for_poll_error(log, revents);
		if (errnum != 0)
			break;
	}
	return errnum;
}

void linted_log_receive(struct linted_log_task_receive *task, unsigned task_id,
                        linted_log log, char msg_ptr[static LINTED_LOG_MAX])
{
	linted_mq_task_receive(LINTED_UPCAST(task), task_id, log, msg_ptr,
	                       LINTED_LOG_MAX);
}

static linted_error check_for_poll_error(linted_ko ko, short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = EBADF;

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
