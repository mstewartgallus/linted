/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
#include "privilege.h"

#include "linted/error.h"

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#if defined HAVE_SYS_AUXV_H
#include <sys/auxv.h>
#endif

linted_error linted_linted_privilege_check(void)
{
	uid_t uid = getuid();
	if (0 == uid)
		return EPERM;

	gid_t gid = getgid();
	if (0 == gid)
		return EPERM;

#if defined HAVE_SYS_AUXV_H && defined HAVE_GETAUXVAL &&               \
    defined AT_SECURE
	if (getauxval(AT_SECURE))
		return EPERM;
#endif

	return 0;
}
