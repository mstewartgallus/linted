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
 * terminal.
 *
 * Currently if `CLONE_NEWPID` sandboxes die then their children will
 * automatically be killed by the kernel.
 *
 * @bug Currently if `PR_SET_CHILD_SUBREAPER` sandboxes die then their
 * children wont be killed by the kernel and will mess up the init
 * container. Kill `PR_SET_CHILD_SUBREAPER` sandbox children upon
 * their death and when `monitor` kills them.
 *
 * @section signals Signal handling
 *
 * Currently `init` blocks all nonurgent exit signals (`SIGHUP`,
 * `SIGINT1, `SIGQUIT`, and `SIGTERM`) and lets the `monitor` handle
 * them.
 *
 * Currently as sandboxes are ptraced by the `monitor` when they get
 * terminal notifications such as `SIGINT` they are not acted upon as
 * `monitor` traps such notifications and doesn't process them while
 * it is shutting down. This may be a good way of preventing sandboxes
 * from getting terminal notifications or it may be a bad way of
 * handling this.
 *
 * @todo Currently `CLONE_NEWPID` service processes explicitly setup
 * signal handlers for exit signals and forward them to children. This
 * does not work for `SIGKILL` for `PR_SET_CHILD_SUBREAPER` style
 * service processes. Remove such handling from `CLONE_NEWPID` style
 * waiters and make the monitor forward the signals and handle both
 * the `CLONE_NEWPID` case and the `PR_SET_CHILD_SUBREAPER` case.
 *
 * @todo Extend ignoring controlling terminal notifications to
 * `PR_SET_CHILD_SUBREAPER` sandboxes and let the monitor handle the
 * signals.
 */
