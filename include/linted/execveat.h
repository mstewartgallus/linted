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
#ifndef LINTED_EXECVEAT_H
#define LINTED_EXECVEAT_H

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/util.h"

#include <errno.h>
#include <stdint.h>
#include <sys/syscall.h>
#include <unistd.h>

static inline linted_error linted_execveat(linted_ko fd,
                                           char const *binary,
                                           char **args, char **env,
                                           uint_least64_t opts);

#ifndef __NR_execveat
#if defined __amd64__
#define __NR_execveat 322
#elif defined __i386__
#define __NR_execveat 358
#elif defined __powerpc__
#define __NR_execveat 362
#endif
#endif

#ifdef __NR_execveat
static inline linted_error linted_execveat(linted_ko fd,
                                           char const *binary,
                                           char **args, char **env,
                                           uint_least64_t opts)
{
	syscall(__NR_execveat, (int)fd, binary, args, env, opts);
	linted_error err = errno;
	LINTED_ASSUME(err != 0);
	return err;
}
#endif

#endif /* LINTED_EXECVEAT_H */