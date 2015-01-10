/* Copyright (C) 2014 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 * @file
 *
 * Linted -- Process Management
 *
 * Process hierarchy
 *
 * - `init`
 *   - `simulator`
 *     - `simulator`
 *   - `drawer`
 *      - `drawer`
 *   - `gui`
 *     - `gui`
 *   - `window`
 *     - `window`
 *   - `monitor`
 *
 * `init` is the top level service that contains everything using
 * `PR_SET_CHILD_SUBREAPER`.  All it does is monitor and restart the
 * `monitor` process and delegate signals to it.
 *
 * Underneath `init` the `monitor` process monitors and restarts
 * service processes and delegates signals.
 *
 * Service processes underneath `init` using `PR_SET_CHILD_SUBREAPER`
 * or `CLONE_NEWPID` contain underneath them similarly named processes
 * that do the actual work of the service.  Service processes don't
 * actually monitor or restart their children.  That job is delegated
 * to `monitor` who ptraces and monitor these service processes (but
 * not the children of the service processes.)
 *
 * @bug Currently not all sandboxed processes are killed on `init`
 * death.  Currently `monitor` will receive a `SIGHUP` because `init`
 * will be the controlling process for the controlling terminal (if
 * there is a terminal) or from a `SIGKILL` from set parent death
 * signal.  Possible solutions:
 * - `monitor` could ptrace `init` and kill its children when `init`
 *   is about to die.
 * - Sandboxes could use `PR_SET_PDEATHSIG` once they have been
 *   reparented to `init`.  However, I am not sure how they would wait
 *   until they have been reparented to `init` before using
 *   `PR_SET_PDEATHSIG`.
 *
 * `monitor` manually kill any children of dying
 * `PR_SET_CHILD_SUBREAPER` type sandboxes and children of dying
 * `CLONE_NEWPID` type sandboxes are killed by the kernel
 * automatically.
 *
 * @section ipc IPC
 *
 * Files that may exist in user visible filesystems and that can be
 * used for IPC:
 *
 * - Regular files
 * - Directory files
 * - Symlink
 * - FIFOs
 * - Unix domain sockets
 * - POSIX Message Queues
 * - Character devices
 * - Block devices
 *
 * So far we have chosen to use FIFOs as they can be put on many
 * places.
 *
 * Requirements
 * - Presents a security boundary
 *
 * Similar past attempts:
 * - The Xen hypervisor's shared memory I/O.
 *
 * @section signals Signal handling
 *
 * Currently `init` forwards all nonurgent exit signals (`SIGHUP`,
 * `SIGINT1`, `SIGQUIT`, and `SIGTERM`) to the `monitor` to handle
 * them.
 */
