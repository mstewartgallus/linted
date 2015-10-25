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
 * <li> Toolchain hardening options:
 *      <ul>
 *      <li> @see m4/linted_harden.m4 </li>
 *      <li> position independent executables </li>
 * </ul></li>
 * <li> Program linters, see:
 * <ul>
 *      <li> @see scripts/check-cppcheck.in </li>
 *      <li> @see scripts/check-clang-analysis.in </li>
 *      <li> @see scripts/check-iwyu.in </li>
 * </ul></li>
 * <li> A few unit tests (we need more) </li>
 * <li> Unit tests are also run under Valgrind (alternate dynamic
 * checkers like electric fence or duma might have advantages) </li>
 * </ul>
 *
 * @subsection isolation Process Isolation
 * <ul>
 * <li> @see src/linted-monitor/monitor.c </li>
 * <li> @see src/linted-sandbox/sandbox.c </li>
 * <li> @see src/linted/linted.c (prevents file descriptor leaks) </li>
 * <li> we use sandbox specific chroots</li>
 * <li> we use new namespaces for sandboxes (`CLONE_NEWUSER`,
 *      `CLONE_NEWIPC`, `CLONE_NEWNS`, `CLONE_NEWUTS`, `CLONE_NEWNET`,
 *      `CLONE_NEWPID`) </li>
 * <li> we sanitize environment variables </li>
 * <li> we set a generic low scheduling policy for sandboxes </li>
 * <li> we use a generic Seccomp policy for sandboxes </li>
 * <li> we use generic resource limits for sandboxes </li>
 * </ul>
 *
 *
 * @subsection determinism Determinism
 *
 * To achieve more determinism we define `__FILE__` to be the null
 * pointer.
 *
 * We use the options: `-fno-working-directory`,
 *`-gno-record-gcc-switches`,
 * `-fdebug-prefix-map=${srcdir}=.`.
 *
 * We set the environment variable `PWD` to be `/proc/self/cwd`.
 *
 * We give `ranlib` the `-D` option.
 *
 * We give `ar` the `D` option.
 *
 * We set the `frandom-seed` option to be a hash of the source file.
 *
 * @section formal-methods Formal Methods
 *
 * The process of proving properties of a program using formal methods
 * generally consists of three parts:
 * - program specification
 * - desired property or theorem specification
 * - proof that the program specification satisfies the theorem.
 *
 * @subsection program-specification Program Specification
 *
 * Before one can prove properties of a program one must define it in a
 * formal and unambiguous way.
 *
 * There are three common approaches to creating a formal specification
 * of a program:
 * - Automatic extraction of the program from the program specification.
 * - Automatic extraction of the specification from the program.
 * - Manual specification creation and manual verification that the
 *   specification matches the program code.
 *
 * @subsection theorem-specification Theorem Specification
 *
 * Before one can prove properties of a program one must first define
 * those properties and the theorems one wants to prove in a formal and
 * unambiguous way.  Too many people gloss over and forget about this
 * step.  One should make sure that one is actually verifying the
 * properties that one thinks one is.
 *
 * @subsection theorem-proof Theorem Proof
 *
 * Finally, one must actually prove the theorems or theorem one wants to
 * prove.
 *
 * There are four common approaches to proving program theorems:
 *
 * - Automatic proof by way of abstract interpretation.
 * - Automatic proof by way of model checking.
 * - Manual proof generation and automatic checking.
 * - Manual proof generation and manual proof checking.
 *
 * @subsection analysis Analysis
 *
 * There are many, many, many tools for automatic theorem proving. There
 * do not seem to be any production ready tools for automatic program
 * specification.  Manual program specification is far too fragile.
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
 *
 */
