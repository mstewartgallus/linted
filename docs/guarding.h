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
 * <li> Processs Isolation, see in particular: <ul>
 * <li> @see src/linted-monitor </li>
 * <li> @see src/linted-sandbox </li>
 * <li> @see src/linted/linted.c (prevents file descriptor leaks) </li>
 * </ul></li>
 * <li> Toolchain hardening options, see: @see m4/linted_harden.m4 </li>
 * <li> Program linters, see: <ul>
 * <li> @see scripts/check-cppcheck.in </li>
 * <li> @see scripts/check-clang-analysis.in </li>
 * <li> @see scripts/check-iwyu.in </li>
 * </ul></li>
 * </ul>
 *
 * @section potential Potential Techniques
 * - Use `setrlimit` to limit resources in individual processes.  I am
 *   not sure how it works with `CLONE_NEWUSER`.
 *
 * - Restrict Capabilities
 *   - Seccomp
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
 *   - Using unit tests
 *   - Using fuzz tests
 *   - Under tools like
 *     - Valgrind
 *     - electric fence or duma
 *
 * - Toolchain hardening:
 *   - stack smashing protection with -fstack-protector-all
 *   - position independent executable support with -pie and -fPIE
 */
