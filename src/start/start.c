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

#include "linted/error.h"
#include "linted/io.h"
#include "linted/locale.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <link.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/auxv.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <unistd.h>

#include <linux/random.h>

static void do_nothing(int signo);

static bool is_open(linted_ko ko);

static bool is_privileged(void);
static bool was_privileged(void);

static linted_error find_open_kos(linted_ko **kosp, size_t *size);
static void sort_kos(linted_ko *ko, size_t size);
static linted_error sanitize_kos(size_t kos_size);

static linted_error get_system_entropy(unsigned *entropyp);
static linted_error open_fds_dir(linted_ko *kop);

/*
 * Linked in libraries can do a lot of nastiness on startup so startup
 * before main even happens.
 */
static int real_setup(void)
{
	linted_error errnum;

	/* Check whether basics are open */
	if (!is_open(STDERR_FILENO))
		/* Sadly, this is all we can do */
		return EINVAL;

	if (!is_open(STDOUT_FILENO))
		return EINVAL;

	if (!is_open(STDIN_FILENO))
		return EINVAL;

	if (is_privileged()) {
		linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\
Bad administrator!\n\
It is insecure to run a game with high privileges!\n"));
		return EPERM;
	}

	size_t kos_size = linted_start_config.kos_size;
	linted_ko *kos = linted_start_config.kos;

	linted_ko *open_kos;
	size_t open_kos_size;
	{
		linted_ko *xx;
		size_t yy;
		errnum = find_open_kos(&xx, &yy);
		if (errnum != 0) {
			return errnum;
		}
		open_kos = xx;
		open_kos_size = yy;
	}

	if (open_kos_size < kos_size + 3U)
		return EINVAL;

	/* Sort the fds from smallest to largest */
	sort_kos(open_kos, open_kos_size);

	/* Duplicate over leaked files with STDERR_FILENO in case
	 * someone tries to write to them later.
	 */
	for (size_t ii = 3U + kos_size; ii < open_kos_size; ++ii) {
		if (-1 == dup3(STDERR_FILENO, open_kos[ii], O_CLOEXEC)) {
			return errno;
		}
	}

	for (size_t ii = 0U; ii < kos_size + 3U; ++ii) {
		if (open_kos[ii] != (linted_ko)ii)
			return EINVAL;
	}

	linted_mem_free(open_kos);

	errnum = sanitize_kos(3U + kos_size);
	if (errnum != 0)
		return errnum;

	for (linted_ko ii = 0; ii < 3; ++ii) {
		int flags = fcntl(ii, F_GETFD);
		if (-1 == flags) {
			perror("fcntl");
			return EXIT_FAILURE;
		}

		if (-1 == fcntl(ii, F_SETFD, flags & ~FD_CLOEXEC)) {
			perror("fcntl");
			return EXIT_FAILURE;
		}
	}

	for (size_t ii = 0U; ii < kos_size; ++ii)
		kos[ii] = (linted_ko)(ii + 3U);

	return 0;
}

__attribute__((constructor)) static void setup(void)
{
	linted_error errnum = real_setup();
	if (errnum != 0)
		_Exit(errnum);
}

int main(int argc, char *argv[])
{
	linted_error errnum = 0;

	if (argc < 1) {
		linted_locale_missing_process_name(
		    STDERR_FILENO, linted_start_config.canonical_process_name);
		return EINVAL;
	}

	char const *const process_name = argv[0U];

	size_t kos_size = linted_start_config.kos_size;

	if (kos_size > 0U) {
		char *listen_pid_string = getenv("LISTEN_PID");
		if (NULL == listen_pid_string) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: need LISTEN_PID\n",
			                       process_name);
			return EINVAL;
		}

		pid_t pid = atoi(listen_pid_string);
		if (getpid() != pid) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: LISTEN_PID %i != getpid() %i\n",
			                       process_name, pid, getpid());
			return EINVAL;
		}

		char *listen_fds_string = getenv("LISTEN_FDS");
		if (NULL == listen_fds_string) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: need LISTEN_FDS\n",
			                       process_name);
			return EINVAL;
		}

		linted_ko fds_count;
		{
			linted_ko xx;
			errnum = linted_ko_from_cstring(listen_fds_string, &xx);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_from_cstring");
				return errnum;
			}
			fds_count = xx;
		}

		if ((uintmax_t)fds_count != kos_size) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: LISTEN_FDS %i != %lu\n",
			                       process_name, fds_count,
			                       kos_size);
			return EINVAL;
		}
	}

	{
		unsigned entropy;
		errnum = get_system_entropy(&entropy);
		if (errnum != 0) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not read a source of system entropy: %s\n",
			                       process_name,
			                       linted_error_string(errnum));
			return errnum;
		}
		linted_random_seed_generator(entropy);
	}

	{
		struct sigaction act = { 0 };
		sigemptyset(&act.sa_mask);
		act.sa_handler = do_nothing;
		if (-1 == sigaction(SIGUSR1, &act, NULL)) {
			perror("sigaction");
			return EXIT_FAILURE;
		}
	}

	return linted_start(process_name, argc, (char const * const *)argv);
}

static void do_nothing(int signo)
{
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

static bool is_open(linted_ko ko)
{
	return fcntl(ko, F_GETFD) != -1;
}

static void sort_kos(linted_ko *kos, size_t size)
{
	for (size_t ii = 0U; ii < size; ++ii) {
		for (size_t jj = ii + 1U; jj < size; ++jj) {
			linted_ko kos_ii = kos[ii];
			linted_ko kos_jj = kos[jj];

			if (kos_ii > kos_jj) {
				kos[ii] = kos_jj;
				kos[jj] = kos_ii;
			}
		}
	}
}

static linted_error find_open_kos(linted_ko **kosp, size_t *sizep)
{
	linted_error errnum = 0;
	size_t size = 0U;
	linted_ko *fds = NULL;

	/*
	 * Use readdir because this function isn't thread safe anyways
	 * and readdir_r has a very broken interface.
	 */

	linted_ko fds_dir_ko;
	{
		linted_ko xx;
		errnum = open_fds_dir(&xx);
		if (errnum != 0)
			return errnum;
		fds_dir_ko = xx;
	}

	DIR *const fds_dir = fdopendir(fds_dir_ko);
	if (NULL == fds_dir) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(fds_dir_ko);

		return errnum;
	}

	for (;;) {
		errno = 0;
		struct dirent *const result = readdir(fds_dir);
		{
			errnum = errno;
			if (errnum != 0)
				goto close_fds_dir;
		}
		if (NULL == result)
			break;

		char const *d_name;

		d_name = result->d_name;

		if (0 == strcmp(".", d_name))
			continue;

		if (0 == strcmp("..", d_name))
			continue;

		linted_ko const fd = atoi(d_name);
		if (fd == fds_dir_ko)
			continue;

		{
			void *xx;
			errnum = linted_mem_realloc_array(&xx, fds, size + 1U,
			                                  sizeof fds[0]);
			if (errnum != 0)
				goto free_fds;
			fds = xx;
		}

		fds[size] = fd;

		++size;
	}

free_fds:
	if (errnum != 0) {
		linted_mem_free(fds);
		fds = NULL;
	}

close_fds_dir:
	if (-1 == closedir(fds_dir)) {
		linted_error close_errnum = errno;
		LINTED_ASSUME(close_errnum != 0);
		assert(close_errnum != EBADF);

		if (0 == errnum)
			errnum = close_errnum;
	}

	*sizep = size;
	*kosp = fds;

	return errnum;
}

/**
 * @bug Reopening sockets doesn't work properly. See bug
 * https://bugzilla.kernel.org/show_bug.cgi?id=79771 for details.
 *
 * Opening a UNIX socket file via `open` or similar system calls fails
 * with an error. And so, reopening a UNIX socket file via
 * `/proc/self/fd` fails with an error. `bind` does not work with
 * symbolic links so one can't reopen a UNIX socket file. As well,
 * there is no way to currently bind to an already created UNIX socket
 * file anyways (curiously enough, `SO_REUSEPORT` does allow the same
 * thing but for TCP sockets only so it might be possible to submit
 * patches to the Linux kernel that extend the functionality to UNIX
 * sockets and therefore get this part working).
 *
 * @bug Reopening pseudo-terminals doesn't work properly.  See bug
 * https://bugzilla.kernel.org/show_bug.cgi?id=89111 for details.
 * Basically, the problem is that `posix_openpt` is implemented by
 * opening the file `/dev/ptmx`.  Each time one opens `/dev/ptmx` one
 * receives a new file description that points to the same `/dev/ptmx`
 * file each time.  Each file description points to the exact same
 * inode of `/dev/ptmx`.  This is extremely confusing and weird
 * behaviour because `posix_openpt` does not create a new inode each
 * time but only creates a new file description that still points to
 * the same file at `/dev/ptmx`.  One confusing result of this
 * implementation of `posix_openpt` is that reopening a master
 * pseudo-terminal opens up a new file description that points to
 * `/dev/ptmx` and does not give one the old master pseudo-terminal
 * but instead creates a new one.
 */
static linted_error sanitize_kos(size_t kos_size)
{
	linted_error errnum;

	linted_ko fds_dir_ko;
	{
		linted_ko xx;
		errnum = open_fds_dir(&xx);
		if (errnum != 0)
			return errnum;
		fds_dir_ko = xx;
	}

	for (size_t ii = 0U; ii < kos_size; ++ii) {
		linted_ko fd = (linted_ko)ii;

		int oflags = fcntl(fd, F_GETFL);
		if (-1 == oflags) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		mode_t mode;
		{
			struct stat buf;
			if (-1 == fstat(fd, &buf)) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			}
			mode = buf.st_mode;
		}

		/* Reopening sockets fails with ENXIO. So just skip
		 * them for now. */
		if (S_ISSOCK(mode))
			continue;

		linted_ko new_fd;
		{
			char pathname[10U];
			sprintf(pathname, "%i", fd);

			do {
				new_fd = openat(fds_dir_ko, pathname,
				                oflags | O_NONBLOCK | O_NOCTTY |
				                    O_CLOEXEC);
				if (-1 == new_fd) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
				} else {
					errnum = 0;
				}
			} while (EINTR == errnum);
		}

		if (errnum != 0)
			return errnum;

		if (-1 == dup3(new_fd, fd, O_CLOEXEC)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		errnum = linted_ko_close(new_fd);
		if (errnum != 0)
			return errnum;
	}

	return linted_ko_close(fds_dir_ko);
}

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
		errnum = linted_io_read_all(random, NULL, &xx, sizeof xx);
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

#if defined __linux__
static linted_error open_fds_dir(linted_ko *kop)
{
	return linted_ko_open(kop, LINTED_KO_CWD, "/proc/self/fd",
	                      LINTED_KO_DIRECTORY);
}
#else
#error no open files directory known for this platform
#endif
