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
#ifndef LINTED_INIT_MONITOR_H
#define LINTED_INIT_MONITOR_H

#include "linted/ko.h"

/**
 * @file
 *
 * Sandboxes and monitors the process hierarchy.
 *
 * The monitor process is privileged with full capabilities and
 * controls everything.
 *
 * Defended Against Attacks:
 *
 * - The kernel prevents sandboxed processes from ptracing,
 *   process_vm_writeing, etc.. monitor as they lack capabilities
 *   inside the sandbox and so would violate privilege boundaries by
 *   doing so. Also, those system calls are disabled through seccomp.
 *
 * - Currently every process has the nondumpable bit inherited and so
 *   processes within the process cannot ptrace, process_vm_write,
 *   etc.. against other processes and gain more privileges.  However,
 *   due to Linux kernel bug 82531 at
 *   https://bugzilla.kernel.org/show_bug.cgi?id=82531 processes
 *   cannot defend themselves against attackers from outside the
 *   sandbox (which we don't need yet but should still know about).
 *
 * @bug Attacking processes can possibly evade seccomp protections by
 *      switching the architecture type.
 *
 * @bug Attacking processes can access the home of the user with full
 *      read, write access but we cannot sandbox /home with overlayfs
 *      as overflayfs has no equivalent to rbind and so we can't work
 *      with encrypted user folders.
 *
 * @bug Attacking processes can send signals to monitor and kill it or
 *      possibly otherwise cause misbehaviour.
 *
 * @bug Attacking processes can send signals to other processes and
 *      kill them or possibly otherwise cause misbehaviour.
 *
 * @todo Connect the /var directories of the processes to real
 *       locations on disk.
 *
 * @todo Set the PATH to
 *  "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" and
 *  MANAGERPID to getpid().
 */

unsigned char linted_init_monitor(linted_ko cwd, char const *chrootdir,
                                  char const *unit_path);

#endif /* LINTED_INIT_INIT_H */
