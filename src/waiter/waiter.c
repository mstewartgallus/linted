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

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

static void propagate_signal(int signo);
static linted_error set_name(char const *name);

int main(int argc, char *argv[])
{
	linted_error errnum;

	char const *service = getenv("LINTED_SERVICE");

	if (service != NULL) {
		errnum = set_name(service);
		assert(errnum != EINVAL);
	}

	/* Catch signals
	 *
	 * The only signals that can be sent to process ID 1, the init
	 * process, are those for which init has explicitly installed
	 * signal handlers.  This is done to assure the system is not
	 * brought down accidentally.
	 *
	 * - KILL(2) http://www.kernel.org/doc/man-pages/.
	 *
	 * This applies to waiters to if they use CLONE_NEWPID.
	 *
	 * We want to explicitly handle the signal so that it is
	 * propagated to children of init as well.
	 */
	static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };
	if (1 == getpid()) {
		/* Delegate the exit signal to children and then exit */
		for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals);
		     ++ii) {
			struct sigaction action = { 0 };
			action.sa_handler = propagate_signal;
			action.sa_flags = 0;
			sigfillset(&action.sa_mask);
			if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
				assert(errno != EINVAL);
				assert(0);
			}
		}
	}

	for (;;) {
		int wait_status;
		{
			siginfo_t info;
			wait_status = waitid(P_ALL, -1, &info, WEXITED);
		}
		if (-1 == wait_status) {
			errnum = errno;
			assert(errnum != 0);
			assert(errnum != EINVAL);
			if (errnum != EINTR)
				break;
		}
	}

	return errnum;
}

static void propagate_signal(int signo)
{
	kill(-1, signo);

	/* Sadly, it is impossible to kill oneself with the proper
	 * signal as init. */
	raise(SIGKILL);
}

static linted_error set_name(char const *name)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
}
