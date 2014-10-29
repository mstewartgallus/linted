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
#include <elf.h>
#include <errno.h>
#include <fcntl.h>
#include <link.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <unistd.h>

#include <linux/seccomp.h>

static bool is_open(linted_ko ko);

static bool is_privileged(void);
static bool was_privileged(void);

static linted_error find_open_kos(linted_ko **kosp, size_t *size);
static void sort_kos(linted_ko *ko, size_t size);
static linted_error sanitize_kos(size_t kos_size);

static linted_error get_system_entropy(unsigned *entropyp);
static linted_error open_fds_dir(linted_ko *kop);

static linted_error set_no_new_privs(bool v);
static linted_error set_seccomp(struct sock_fprog const *program);

int main(int argc, char *argv[])
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

	if (argc < 1) {
		linted_locale_missing_process_name(
		    STDERR_FILENO, linted_start_config.canonical_process_name);
		return EINVAL;
	}

	char const *const process_name = argv[0U];

	size_t kos_size = linted_start_config.kos_size;
	linted_ko *kos = linted_start_config.kos;

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

	linted_ko *open_kos;
	size_t open_kos_size;
	{
		linted_ko *xx;
		size_t yy;
		errnum = find_open_kos(&xx, &yy);
		if (errnum != 0) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: couldn't find open files: %s\n",
			                       process_name,
			                       linted_error_string(errnum));
			return errnum;
		}
		open_kos = xx;
		open_kos_size = yy;
	}

	if (open_kos_size < kos_size + 3U) {
		linted_io_write_format(STDERR_FILENO, NULL, "\
%s: too little files passed in\n",
		                       process_name);
		return EINVAL;
	}
	/* Sort the fds from smallest to largest */
	sort_kos(open_kos, open_kos_size);

	/* Close leaked files over and don't check for errors as they
	 * could just be leaked /dev/full handles or similar. */
	for (size_t ii = 3U + kos_size; ii < open_kos_size; ++ii)
		linted_ko_close(open_kos[ii]);

	for (size_t ii = 0U; ii < kos_size + 3U; ++ii) {
		if (open_kos[ii] != (linted_ko)ii) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: files passed in aren't in strict order\n",
			                       process_name);
			return EINVAL;
		}
	}

	linted_mem_free(open_kos);

	errnum = sanitize_kos(3U + kos_size);
	if (errnum != 0) {
		linted_io_write_format(STDERR_FILENO, NULL, "\
%s: sanitize_kos: %s\n",
		                       process_name,
		                       linted_error_string(errnum));
		return errnum;
	}

	for (size_t ii = 0U; ii < kos_size; ++ii)
		kos[ii] = (linted_ko)(ii + 3U);

	errnum = set_no_new_privs(true);
	if (errnum != 0) {
		linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not drop ability to raise privileges through execve: %s\n",
		                       process_name,
		                       linted_error_string(errnum));
		return errnum;
	}

	if (linted_start_config.seccomp_bpf != NULL) {
		errnum = set_seccomp(linted_start_config.seccomp_bpf);
		if (errnum != 0) {
			errno = errnum;
			perror("prctl");
			return errnum;
		}
	}

	{
		unsigned entropy;
		if ((errnum = get_system_entropy(&entropy)) != 0) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not read a source of system entropy: %s\n",
			                       process_name,
			                       linted_error_string(errnum));
			return errnum;
		}
		linted_random_seed_generator(entropy);
	}

	return linted_start(process_name, argc, (char const * const *)argv);
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
	/* At startup, environ hasn't been changed yet and so we can
	 * still find the auxv from it.
	 */
	char **envp = environ;

	while (*envp != NULL)
		++envp;

	ElfW(auxv_t) *vector = (ElfW(auxv_t) *)(envp + 1U);

	for (; vector->a_type != AT_NULL; ++vector) {
		if (AT_SECURE == vector->a_type)
			goto got_vector;
	}
	abort();
got_vector:
	return vector->a_un.a_val;
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

		/**
		 * @bug Sockets don't get O_NONBLOCK set.
		 */
		if (errnum != ENXIO) {
			if (errnum != 0)
				return errnum;

			if (-1 == dup2(new_fd, fd)) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				return errnum;
			}

			errnum = linted_ko_close(new_fd);
			if (errnum != 0)
				return errnum;
		}

		int dflags = fcntl(fd, F_GETFD);
		if (-1 == dflags) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		if (-1 == fcntl(fd, F_SETFD, (long)dflags | FD_CLOEXEC)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
	}

	return linted_ko_close(fds_dir_ko);
}

static linted_error get_system_entropy(unsigned *entropyp)
{
	/*
	 * Use /dev/random so we block when asking for entropy after
	 * startup.
	 */
	linted_error errnum;
	linted_ko random;
	unsigned entropy;

	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, "/dev/random",
		                        LINTED_KO_RDONLY);
		if (errnum != 0)
			return errnum;
		random = xx;
	}

	{
		unsigned xx;
		errnum = linted_io_read_all(random, NULL, &xx, sizeof xx);
		if (errnum != 0)
			return errnum;
		entropy = xx;
	}

	if ((errnum = linted_ko_close(random)) != 0)
		return errnum;

	*entropyp = entropy;
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

static linted_error set_no_new_privs(bool v)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NO_NEW_PRIVS, (unsigned long)v, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
}

static linted_error set_seccomp(struct sock_fprog const *program)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_SECCOMP, (unsigned long)SECCOMP_MODE_FILTER,
	                program, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
}
