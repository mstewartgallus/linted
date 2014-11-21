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
 * - init
 *   - simulator
 *     - simulator
 *   - drawer
 *      - drawer
 *   - gui
 *     - gui
 *   - logger
 *     - logger
 *   - window
 *     - window
 *   - monitor
 *
 * Everything is contained under init using
 * PR_SET_CHILD_SUBREAPER. monitor monitors containers with ptrace and
 * spawns containers. Containers contain similarly named processes
 * using PR_SET_CHILD_SUBREAPER or CLONE_NEWPID.
 *
 * @bug Currently if the init process dies all the child processes
 * will die too from SIGHUP because init will be the controlling
 * process for the controlling terminal (if there is a terminal.) This
 * is not good enough for proper containerization as it doesn't work
 * when there is not a controlling terminal or if children change
 * their controlling terminal.
 *
 * Currently if CLONE_NEWPID sandboxes die then their children will
 * automatically be killed by the kernel.
 *
 * @bug Currently if PR_SET_CHILD_SUBREAPER sandboxes die then their
 * children wont be killed by the kernel and will mess up the init
 * container.
 */
