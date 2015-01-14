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
#define _GNU_SOURCE

#include "config.h"

#include "linted/start.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/auxv.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include <linux/random.h>

static void do_nothing(int signo);

static bool is_privileged(void);
static bool was_privileged(void);

static linted_error get_system_entropy(unsigned *entropyp);

int main(int argc, char *argv[])
{
	linted_error errnum = 0;

	for (;;) {
		linted_ko ko;
		{
			linted_ko xx;
			errnum =
			    linted_ko_open(&xx, LINTED_KO_CWD, "/dev/null", 0);
			if (errnum != 0)
				return EXIT_FAILURE;
			ko = xx;
		}

		if (ko > 3U) {
			linted_ko_close(ko);
			break;
		}
	}

	char const *process_name;
	if (argc < 1) {
		process_name = linted_start_config.canonical_process_name;
	} else {
		process_name = argv[0U];
	}

	linted_log_open(process_name);

	if (argc < 1) {
		linted_log(LINTED_LOG_ERR, "missing process name");
		return EXIT_FAILURE;
	}

	if (is_privileged()) {
		linted_log(LINTED_LOG_ERR,
		           "%s should not be run with high privileges",
		           PACKAGE_NAME);
		return EPERM;
	}

	{
		unsigned entropy;
		errnum = get_system_entropy(&entropy);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERR, "get_system_entropy: %s",
			           linted_error_string(errnum));
			return errnum;
		}
		linted_random_seed_generator(entropy);
	}

	{
		struct sigaction act = { 0 };
		sigemptyset(&act.sa_mask);
		act.sa_handler = do_nothing;
		act.sa_flags = 0;
		if (-1 == sigaction(LINTED_ASYNCH_SIGNO, &act, 0)) {
			linted_log(LINTED_LOG_ERR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	return linted_start(process_name, argc, (char const * const *)argv);
}

static void do_nothing(int signo)
{
	/* Do nothing */
}

static bool is_privileged(void)
{
	uid_t uid = getuid();
	if (0 == uid)
		return true;

	gid_t gid = getgid();
	if (0 == gid)
		return true;

	return was_privileged();
}

#ifdef __linux__
static bool was_privileged(void)
{
	return getauxval(AT_SECURE);
}
#else
#error "was privileged" check has not been implemented for this system yet
#endif

static linted_error get_system_entropy(unsigned *entropyp)
{
	linted_error errnum;
	linted_ko random;

	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, "/dev/urandom",
		                        LINTED_KO_RDONLY);
		if (errnum != 0)
			return errnum;
		random = xx;
	}

	/* Minor time of check to time of use bug here but this is
	 * only a minor helper for check for bad system
	 * configurations.  Indeed, arguable this code SHOULD NOT be
	 * here because it prevents administrators from putting a
	 * custom socket, pipe, or file on /dev/urandom and providing
	 * their own implementation of the interface.  For example,
	 * one could put a normal file on /dev/urandom that provides
	 * some pregenerated data for deterministic tests of programs
	 * that use it.
	 */
	int entropy;
	{
		int xx;
		if (-1 == ioctl(random, RNDGETENTCNT, &xx)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		entropy = xx;
	}

	unsigned data;
	if ((unsigned)entropy < sizeof entropy * CHAR_BIT)
		return EAGAIN;

	{
		unsigned xx;
		errnum = linted_io_read_all(random, 0, &xx, sizeof xx);
		if (errnum != 0)
			return errnum;
		data = xx;
	}

	errnum = linted_ko_close(random);
	if (errnum != 0)
		return errnum;

	*entropyp = data;
	return 0;
}
