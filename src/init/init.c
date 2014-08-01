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

#include "init.h"
#include "monitor.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/wait.h>

uint_fast8_t linted_init_init(linted_ko cwd, char const *display,
                              char const *chrootdir_path,
                              char const *fstab_path,
                              char const *simulator_path, char const *gui_path)
{
    linted_error errnum;

    static char const process_name[] = "init";

    /* The init. In the future it should reap all processes and
     * monitor the process monitor to restart it if it dies
     */
    if (-1 == prctl(PR_SET_NAME, (unsigned long)process_name, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    if (-1 == prctl(PR_SET_PDEATHSIG, (unsigned long)SIGKILL, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    pid_t child;
    {
        child = fork();
        if (-1 == child) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: can't clone unprivileged process: %s\n",
                                   process_name,
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }

        if (0 == child) {
            return linted_init_monitor(cwd, display, chrootdir_path,
                                       fstab_path, simulator_path, gui_path);
        }
    }

    {
        siginfo_t info;
        do {
            errnum = -1 == waitid(P_PID, child, &info, WEXITED) ? errno : 0;
        } while (EINTR == errnum);
        if (errnum != 0) {
            assert(errnum != EINVAL);
            assert(errnum != ECHILD);
            assert(false);
        }
        return info.si_status;
    }
}
