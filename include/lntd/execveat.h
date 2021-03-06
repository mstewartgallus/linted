/*
 * Copyright 2015 Steven Stewart-Gallus
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
#ifndef LNTD_EXECVEAT_H
#define LNTD_EXECVEAT_H

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/util.h"

#include <errno.h>
#include <stdint.h>
#include <unistd.h>

#if defined HAVE_POSIX_API
#include <sys/syscall.h>
#endif

static inline lntd_error lntd_execveat(lntd_ko fd, char const *binary,
                                       char **args, char **env,
                                       uint_least64_t opts);

#if defined HAVE_POSIX_API

#ifndef __NR_execveat
#if defined __amd64__
#define __NR_execveat 322
#elif defined __i386__
#define __NR_execveat 358
#elif defined __powerpc__
#define __NR_execveat 362
#elif defined __arm__
#define __NR_execveat (__NR_SYSCALL_BASE + 387)
#elif defined __aarch64__
/* This is the generic syscall number */
#define __NR_execveat 281
#endif
#endif

#ifdef __NR_execveat
static inline lntd_error lntd_execveat(lntd_ko fd, char const *binary,
                                       char **args, char **env,
                                       uint_least64_t opts)
{
	syscall(__NR_execveat, (int)fd, binary, args, env, opts);
	lntd_error err = errno;
	LNTD_ASSUME(err != 0);
	return err;
}
#endif

#endif

#endif /* LNTD_EXECVEAT_H */
