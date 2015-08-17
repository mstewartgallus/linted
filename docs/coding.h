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
 * This is a list of standards for maintaining correct and high
 * quality code in Linted.  It is more important to understand the
 * coding standard than to follow it.
 *
 * The Linted project unconditionally enforces its rules for correct
 * code. No exceptions.
 *
 * The Linted project has rules that may help ensure high code
 * quality, that is to say: functional and defect-free code. However,
 * these code quality rules are only heuristics and should never get
 * in the way of writing correct code.
 *
 * The Linted project's rules for code style are nothing more than
 * guidelines.
 *
 * @section correctness Code Correctness
 *
 * <ul>
 *
 * <li> Honestly reassess one's own competence.
 *
 * Don't do security theater and don't do cargo cult security. Use
 * simple, understandable and secure methods of making one's code
 * correct. If you can't prove the code is correct it's not.
 *
 * </li>
 *
 * <li> Do not leak private data to other actors.
 *
 * For example, when one copies data from structures to external
 * sources using `mq_send`, write or other functions as if they are
 * byte arrays one can leak data through data structure padding. See
 * also CERT rule DCL39-C: Avoid information leakage in structure
 * padding (as of 2014-02-15 available at
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
 * For example, names ending in `_t` are reserved by POSIX and should
 * not be defined in code that should be POSIX compatible (although
 * code reimplementing POSIX functionality should probable define `_t`
 * ending names to names of replacement types).
 *
 * </li>
 *
 * <li> Properly recover from and report function failure.
 *
 * A function may fail at accomplishing it's task.  This failure is
 * not an error, incorrect code, a rare event or unexpected.  In the
 * event of failure to accomplish a task, the task must be retried, an
 * alternative must be tried or the task must sanely exit.  In the
 * event of a failure to accomplish a task no corruption of task state
 * is expected.
 *
 * </li>
 *
 * <li> Abort on finding errorneous task state.
 *
 * A task may have unexpected and erroneous state that violates
 * internal assertions.  Such state implies that the task's code was
 * written incorrectly, a privileged process reached in and corrupted
 * the state or that the platform is broken in some way.  In such a
 * case, a task with error state cannot be recovered from as the
 * recovery code itself may be incorrect or state relied upon to
 * recover the task may be corrupted.  Moreover, if some task state is
 * erroneous, other state in the task may be corrupted and probably
 * should not be saved or at least not overwrite already saved data.
 *
 * </li>
 *
 * <li> Handle any and all possible types of failure
 *
 * For example, POSIX allows a system call to fail for potentially any
 * reason and so one's code must report and return possibly unknown
 * types of function failures.
 *
 * The relevant quotation from The Open Group Base Specifications
 * Issue 7 IEEE Std 1003.1, 2013 Edition:
 *
 * "Implementations may support additional errors not included in this
 * list, may generate errors included in this list under circumstances
 * other than those described here, or may contain extensions or
 * limitations that prevent some errors from occurring."
 *
 * </li>
 *
 * <li> Do not use undefined behaviour
 *
 * For example, the code memset(&p, 0, sizeof p); where p is a pointer
 * is undefined behaviour as the null pointer may not be represented
 * as zeros in memory.
 *
 * </li>
 *
 * </ul>
 *
 * @section quality Code Quality
 *
 * <ul>
 *
 * <li> Do not communicate code to be executed between actors
 *
 * When code to be executed is communicated between different actors a
 * malicious actor can often submit program code that takes over the
 * actor that executes the malicious actor's code.
 *
 * For example, Python's pickled objects data format embeds code to be
 * executed inside of it and so an attacker that can submit arbitrary
 * pickled object data to your service to be decoded can subvert and
 * control it.
 *
 * </li>
 *
 * <li> Do not communicate credentials or privileges between actors
 *
 * When credentials or privileges are communicated between actors it
 * is often possible for an attacker to forge fake credentials that
 * impersonate someone else and that gives the attacker privileges
 * they should not have.
 *
 * For example, many insecure web electronic store fronts implement
 * user sessions by storing customer account information in web
 * browser cookies. However, a customer's session cookies are under
 * their full control and a malicious actor could rewrite their
 * session cookies to indicate another customer's user account unless
 * said cookies are securely communicated and private to one user's
 * account at a time, authenticated as approved by the web store
 * front, and expire at a data that is in a reasonably short time in
 * the future.
 *
 * </li>
 *
 * <li> Do not needlessly waste system resources.
 *
 * For example, do not needless block threads and waste CPU time
 * slices and memory.
 *
 * As well, do not needlessly allocate large amounts of memory.
 *
 * </li>
 *
 * <li> Use a clear and consistent coding style.
 *
 * For example, the Linted project has it's own internal coding style
 * which should always be used.
 *
 * </li>
 *
 * </ul>
 *
 * @section style Code style
 *
 * <ul>
 *
 * <li> Accept and produce strings annotated with lengths.
 *
 * Alternative encodings of strings such as null terminated strings
 * are difficult to reason about, often have poor algorithmic
 * complexity and are never very generic.
 *
 * </li>
 *
 * <li> Directly return error values.
 *
 * For example, we often return values of type `linted_error` instead
 * of passing values through the thread local `errno` variable.
 *
 * </li>
 *
 * <li> Abort on errorneous code or corrupted program state using the
 * `assert` or `LINTED_IMPOSSIBILITY` macros.
 *
 * This keeps the code consistent and clear in it's meaning.
 *
 * </li>
 *
 * <li> Abort on error cases the developer is to lazily handle at the
 * moment with the `LINTED_LAZY_DEV` macro.
 *
 * This macro should never appear in release versions of the
 * code.
 *
 * </li>
 *
 * <li> Lose capabilities by default.
 *
 * Making noninheritance of capabilities the default makes it easy for
 * one to "not leak capabilities to new actors".
 *
 * For example, nearly all files should be opened with the `O_CLOEXEC`
 * flag.
 *
 * Another example, is that for data flags that set the actions
 * allowable on an object the value of zero should give as little
 * privileges as possible. This is no arbitrary example. A memory
 * overwrite vulnerability that allowed one to zero arbitrary data was
 * usable as a way to bypass exploit mitigations by setting the
 * permissions of a COM object to allow one to execute unsafe
 * actions. Learn more at
 * http://www.contextis.com/blog/windows-mitigaton-bypass/.
 *
 * </li>
 *
 * <li> Run asynchronously or nonblockingly by default.
 *
 * Runningly asynchronous or nonblockingly by default makes makes it
 * easy to not "needlessly waste resources" by blocking threads.
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
 * Use the Linux kernel coding style but with tabs and spaces. If you
 * are unsure of how to format some code let the clang-format tool do
 * it for you.
 *
 * Use %%i instead of %%d in format string specifiers.
 *
 * </li>
 * </ul>
 *
 * @section resources Other Resources
 *
 * <ul>
 * <li> [The CERT C Coding Standards](https://www.securecoding.cert.org/confluence/display/seccode/CERT+C+Coding+Standard) </li>
 * <li> [EC-- Rule Set](http://www.leshatton.org/ISOC_subset1103.html) </li>
 * <li> [Defensive Coding](https://docs.fedoraproject.org/en-US/Fedora_Security_Team/1/html/Defensive_Coding) </li>
 * <li> [Cryptography Coding Standard](https://cryptocoding.net/index.php/Coding_rules)
 *
 * Note that this standard currently has dangerous and incorrect
 * advice on how to use the volatile qualifier to guarantee that
 * memory is zeroed. In general, the compiler and the hardware are
 * free at any time to make hidden copies of your data (for example,
 * other cores on the CPU are free to cache stale copies of the data
 * from before it was zeroed by your cunning memset_s implementation
 * and also the CPU is free to cache the zeroing writes of the data in
 * a store buffer before actually clearing the data in RAM or other
 * core caches).
 *
 * This standard also has dangerous and incorrect advice on how to
 * guarantee constant time behaviour for algorithms. In general, one
 * can not guarantee constant time behaviour without special compiler
 * and hardware support. Instead one should use encryption or hashing
 * to prevent any possible leakage of data through timing attacks.
 * </li>
 * <li> [OWASP Secure Coding Principles](https://www.owasp.org/index.php/Secure_Coding_Principles) </li>
 * <li> [JPL Institutional Coding Standard for the C Programming Language](http://lars-lab.jpl.nasa.gov/JPL_Coding_Standard_C.pdf) </li>
 * </ul>
 */
