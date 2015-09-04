/* Copyright (C) 2013, 2015 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- Guarding
 *
 * @section used Used Techniques
 * <ul>
 * <li> Processs Isolation, in particular: <ul>
 * <li> @see src/linted-monitor/monitor.c </li>
 * <li> @see src/linted-sandbox/sandbox.c </li>
 * <li> @see src/linted/linted.c (prevents file descriptor leaks) </li>
 * <li> we use sandbox specific chroots</li>
 * <li> we use new namespaces for sandboxes (`CLONE_NEWUSER`, `CLONE_NEWIPC`,
 * `CLONE_NEWNS`, `CLONE_NEWUTS`, `CLONE_NEWNET`, `CLONE_NEWPID`) </li>
 * <li> we sanitize environment variables </li>
 * <li> we set a generic low scheduling policy for sandboxes </li>
 * <li> we use a generic Seccomp policy for sandboxes </li>
 * <li> we use generic resource limits for sandboxes </li>
 * </ul></li>
 * <li> Toolchain hardening options:<ul>
 * <li> @see m4/linted_harden.m4 </li>
 * <li> position independent executables </li>
 * </ul></li>
 * <li> Program linters, see: <ul>
 * <li> @see scripts/check-cppcheck.in </li>
 * <li> @see scripts/check-clang-analysis.in </li>
 * <li> @see scripts/check-iwyu.in </li>
 * </ul></li>
 * <li> A few unit tests (we need more) </li>
 * <li> Unit tests are also run under Valgrind (alternate dynamic
 * checkers like electric fence or duma might have advantages) </li>
 * </li> </ul>
 *
 * @section potential Potential Techniques
 * - Use fine-grained and sandbox specific scheduling policies
 * - Use fine-grained and sandbox specific seccomp policies
 * - Use fine-grained and sandbox specific resource limits
 *
 * - Restrict Capabilities:
 *   - Ftrace
 *   - SELinux
 *   - AppArmor
 *
 * - Formal Analysis Tools:
 *   - Frama C
 *   - CBMC
 *
 * - Model Checking:
 *   - Our interprocess interactions might be a good area to formalize.
 *
 * - Testing:
 *   - Using much more unit tests
 *   - Using fuzz tests
 *
 * - Toolchain hardening:
 *   - stack smashing protection with -fstack-protector-all
 */
