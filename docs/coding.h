/* Copyright (C) 2014 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- Coding Standards
 *
 * This is a list of standards for maintaining a correct and high
 * quality of a common style in Linted. The rules for code correctness
 * may never be disobeyed. The rules for code quality may be bent
 * occasionally. The rules for code style are very arbitrary and may
 * be broken occasionally.
 *
 * @section correctness Code Correctness
 *
 * <ul>
 *
 * <li> Honestly reassess one's own competence.
 *
 * Don't do security theater and don't do cargo cult security. Use
 * simple, understandable and secure methods of making one's code
 * correct. If you can't prove it's secure it's not.
 *
 * </li>
 *
 * <li> Do not leak private data to other actors.
 *
 * For example, when one copies data from structures using mq_send,
 * write or other functions as if they are byte arrays one can leak
 * data through data structure padding. See also CERT rule DCL39-C:
 * Avoid information leakage in structure padding (as of 2014-02-15
 * available at
 * https://www.securecoding.cert.org/confluence/display/seccode/DCL39-C.+Avoid+information+leakage+in+structure+padding
 * ).
 *
 * </li>
 *
 * <li> Do not leak capabilities or privileges to other actors.
 *
 * For example, when one spawns a new sandboxed process one should not
 * leave open extraneous file descriptors and so leak privileges to
 * the new process.
 *
 * </li>
 *
 * <li> Maintain compatibility with all supported targets.
 *
 * For example, names ending in _t are reserved by POSIX and should
 * not be defined in code that should be POSIX compatible (although
 * code reimplementing POSIX functionality should probable define _t
 * ending names to names of replacement types).
 *
 * </li>
 *
 * <li> Properly recover from and report function failure.
 *
 * A function may fail at accomplishing it's task. This failure is not
 * an error, incorrect code, a rare event or unexpected. In the event
 * of failure to accomplish a task, the task must be retried, an
 * alternative must be tried or the task must sanely exit. In the
 * event of a failure to accomplish a task no corruption of task state
 * is expected.
 *
 * </li>
 *
 * <li> Abort on finding errorneous task state.
 *
 * A task may have unexpected and erroneous state that violates
 * internal assertions. Such state implies that the task's code was
 * written incorrectly, a privileged process reached in and corrupted
 * the state or that the platform is broken in some way. In such a
 * case, a task with error state cannot be recovered from as the
 * recovery code itself may be incorrect or state relied upon to
 * recover the task may be corrupted. Moreover, if some task state is
 * erroneous, other state in the task may be corrupted and probably
 * should not be saved or at least not overwrite already saved data.
 *
 * </li>
 *
 * <li> Allow for possibly unknown types of function failure.
 *
 * POSIX allows a system call to fail for potentially any reason and
 * so one's code must handle possibly unknown types of function
 * failures. It is okay to assert that a specific kind of error should
 * never happen (for example EBADF) but always pass up other kinds of
 * errors.
 *
 * The relevant quotation from The Open Group Base Specifications
 * Issue 7 IEEE Std 1003.1, 2013 Edition.
 *
 * "Implementations may support additional errors not included in this
 * list, may generate errors included in this list under circumstances
 * other than those described here, or may contain extensions or
 * limitations that prevent some errors from occurring."
 *
 * Note that POSIX allows implementations to generate errors under
 * circumstances other than those described by the standard.  So, it
 * is technically unportable to check the value of errors at all. For
 * example, a conforming implementation could randomly generate EBADF
 * not when a non open bad file descriptor is passed in but when some
 * recoverable condition happens. This project accepts the small loss
 * of portability that occurs by checking for errors.
 *
 * </li>
 *
 * </ul>
 *
 * @section quality Code Quality
 *
 * <ul>
 *
 * <li> Do not needlessly waste resources.
 *
 * For example, do not needless block threads and waste CPU time
 * slices and memory.
 *
 * As well, do not needlessly allocate large amounts of memory.
 *
 * </li>
 *
 * <li> Use a clear and consistent coding style. </li>
 *
 * </ul>
 *
 * @section style Code style
 *
 * <ul>
 *
 * <li> Annotate strings with lengths.
 *
 * Alternative encodings of strings such as null terminated strings
 * are difficult to reason about, often have poor algorithmic
 * complexity and are never very generic.
 *
 * </li>
 *
 * <li> Return error values directly.
 *
 * For example, we often return values of type linted_error instead of
 * passing values through errno.
 *
 * </li>
 *
 * <li> Abort on errorneous code or corrupted program state using the
 * `assert` or `LINTED_IMPOSSIBILITY` macros. </li>
 *
 * <li> Abort on error cases the developer is to lazily handle at the
 * moment with the `LINTED_LAZY_DEV` macro.
 *
 * This macro should never appear in release versions of the
 * code.
 *
 * </li>
 *
 * <li> Make noninheritance of capabilities the default.
 *
 * Making noninheritance of capabilities the default makes it easy for
 * one to "not leak capabilities to new actors".
 *
 * An example application of this rule is that nearly all files should
 * be opened with the `O_CLOEXEC` flag.
 *
 * </li>
 *
 * <li> Make asynchronous or nonblocking behaviour the default.
 *
 * Making asynchronous or nonblocking behaviour the default makes
 * makes it easy to not "needlessly waste resources" by blocking
 * threads.
 *
 * An example application of this rule is that nearly all files should
 * be opened with the `O_NONBLOCK` flag.
 *
 * </li>
 *
 * <li> Obey Linted code formatting guidelines.
 *
 * Using one coding style makes it easy to "Use a clear and consistent
 * coding style."
 *
 * Use the Linux kernel coding style but with an indent level of 4 and
 * no tabs.
 *
 * Use %%i instead of %%d in format string specifiers.
 *
 * </ul>
 *
 * @section resources Other Resources
 *
 * <ul>
 *
 * <li> You may wish to read the CERT C coding standards (as of
 *  2014-02-15 available at
 *  https://www.securecoding.cert.org/confluence/display/seccode/CERT+C+Coding+Standard
 *  ).
 * </li>
 *
 * </ul>
 */
