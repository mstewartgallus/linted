/*
 * Copyright 2013 Steven Stewart-Gallus
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

#include "linted/sandbox.h"

#include "linted/util.h"

#include <errno.h>
#include <string.h>

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif                          /* HAVE_SYS_RESOURCE_H */

/* Failing to sandbox a task is always a fatal error and should not
   rely upon being checked. */
void linted_sandbox(void)
{
    int error_status;

#ifdef HAVE_SYS_RESOURCE_H
    /* If the error is that we don't have the permissions to sandbox
       then we're already sandboxed enough. */

    error_status = setrlimit(RLIMIT_NOFILE, &(struct rlimit) {
                             .rlim_cur = 0,.rlim_max = 0});
    if (-1 == error_status && errno != EPERM) {
        LINTED_ERROR("Could not sandbox process because of error: %m", errno);
    }

    error_status = setrlimit(RLIMIT_NPROC, &(struct rlimit) {
                             .rlim_cur = 0,.rlim_max = 0});
    if (-1 == error_status && errno != EPERM) {
        LINTED_ERROR("Could not sandbox process because of error: %m", errno);
    }
#endif                          /* HAVE_SYS_RESOURCE_H */
}
