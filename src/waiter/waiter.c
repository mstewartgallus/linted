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

#include <assert.h>
#include <errno.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
	linted_error errnum;

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
