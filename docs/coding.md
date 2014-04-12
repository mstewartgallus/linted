Copyright (C) 2014 Steven Stewart-Gallus

Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.

This is a list of rules for code quality and style in Linted. The
rules for code style are very arbitrary but must be followed for
overall code consistency although they can bent sometimes. The rules
for code quality are not arbitrary and must be always followed.

Code quality:

- Understand but do not slavishly follow the CERT C coding standards
  (as of 2014-02-15 available at
  https://www.securecoding.cert.org/confluence/display/seccode/CERT+C+Coding+Standard
  ).

- Do not leak data through structure padding.

  When one copies data from structures using mq_send, write or any
  other function as if they are byte arrays one can leak data. See
  CERT rule DCL39-C. Avoid information leakage in structure padding
  (as of 2014-02-15 available at
  https://www.securecoding.cert.org/confluence/display/seccode/DCL39-C.+Avoid+information+leakage+in+structure+padding
  ).

- Maintain compatibility with all supported targets

  For example, names ending in _t are reserved by POSIX and should not
  be defined in code that should be POSIX compatible (although code
  reimplementing POSIX functionality should probable define _t ending
  names to names of replacement types).

- Understand error and failure handling

  - Function failure is not an error.

    A function may fail at accomplishing it's task. This failure is
    not an error, incorrect code, a rare event or unexpected. In the
    event of failure to accomplish a task, the task must be retried,
    an alternative must be tried or the task must sanely exit. In the
    event of a failure no corruption of task state is expected.

  - A task can not recover from erroneous task state.

    A task may have unexpected and erroneous state that violates
    internal assertions. Such state implies that the task's code was
    written incorrectly, a privileged process reached in and corrupted
    the state or that the platform is broken in some way. In such a
    case, a task with error state cannot be recovered from as the
    recovery code itself may be incorrect or state relied upon to
    recover the task may be corrupted. Moreover, if some task state is
    erroneous, other state in the task may be corrupted and probably
    should not be saved or at least not overwrite already saved data.

  - POSIX loosely specificies failure of system calls.

    POSIX allows a system call to fail for potentially any reason. It is
    okay to assert that a specific kind of error should never happen
    (for example EBADF) but always pass up other kinds of errors.

    The relevant quotation from The Open Group Base Specifications Issue
    7 IEEE Std 1003.1, 2013 Edition.

    "Implementations may support additional errors not included in this
    list, may generate errors included in this list under circumstances
    other than those described here, or may contain extensions or
    limitations that prevent some errors from occurring."

    Note that POSIX allows implementations to generate errors under
    circumstances other than those described by the standard. So, it
    is technically unportable to check the value of errors at all. For
    example, a conforming implementation could randomly generate EIO
    not when an actual input output error occurs but when some
    recoverable condition happens. This project accepts the small loss
    of portability that occurs by checking for errors. Such a
    corrupted task can only abort.

Coding style:

- We do not use null terminated strings unless interfacing with other
  people's code. Instead, we pass a length along with a pointer to the
  string.

- On function failure we return an error value directly with a return
  type of errno_t. We do not use errno unless interfacing with other
  people's code.

- On erroneous task state such as caused by programmer logic errors
  and memory corruption we abort using the `assert` macro or the
  `LINTED_IMPOSSIBILITY` macro.

- We have `LINTED_LAZY_DEV` for error cases that the developer is too
  lazy to properly handle at the moment. These should never appear in
  release builds.

- Use the Linux kernel coding style but with an indent level of 4 and
  no tabs.

- Use %i instead of %d in format specifiers.
