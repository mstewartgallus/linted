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
 * Process heirarchy
 *
 * - `init`
 *   - `simulator`
 *     - `simulator`
 *   - `drawer`
 *      - `drawer`
 *   - `gui`
 *     - `gui`
 *   - `logger`
 *     - `logger`
 *   - `window`
 *     - `window`
 *   - `monitor`
 *
 * `init` is the top level service that contains everything using
 * `PR_SET_CHILD_SUBREAPER`. All it does is monitor and restart the
 * `monitor` process.
 *
 * Underneath `init` the `monitor` process monitors and restarts
 * service processes.
 *
 * Service processes underneath `init` using `PR_SET_CHILD_SUBREAPER`
 * or `CLONE_NEWPID` contain underneath them similarly named processes
 * that do the actual work of the service . Service processes don't
 * actually monitor or restart their children. That job is delegated
 * to `monitor` who ptraces and monitor these service processes.
 *
 * @bug Currently if `init` dies all the child processes will die too
 * from `SIGHUP` because `init` will be the controlling process for
 * the controlling terminal (if there is a terminal.) This is not good
 * enough for proper containerization as it doesn't work when there is
 * not a controlling terminal or if children change their controlling
 * terminal or if children use `CLONE_NEWPID`.
 *
 * Currently if `CLONE_NEWPID` sandboxes die then their children will
 * automatically be killed by the kernel.
 *
 * @bug Currently if `PR_SET_CHILD_SUBREAPER` sandboxes die then their
 * children wont be killed by the kernel and will mess up the init
 * container.
 *
 * @section signals Signal handling
 *
 * Currently, `init` blocks all nonurgent exit signals (`SIGHUP`,
 * `SIGINT1, `SIGQUIT`, and `SIGTERM`) and lets the `monitor` handle
 * them.
 *
 * @todo `init` should forward the signals to `monitor` instead of
 * blocking them.
 *
 * Currently (due to coincidental semantics of `CLONE_NEWPID`) the
 * `CLONE_NEWPID` sandboxes do not receive controlling terminal
 * notifications such as `SIGINT` and let the monitor handle the
 * signals.
 *
 * @todo Extend ignoring controlling terminal notifications to
 * `PR_SET_CHILD_SUBREAPER` sandboxes and let the monitor handle the
 * signals.
 */
