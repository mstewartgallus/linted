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

- Understand error checking

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
  circumstances other than those described by the standard. So, it is
  technically unportable to check the value of errors at all. For
  example, a conforming implementation could randomly generate EIO not
  when an actual input output error occurs but when some recoverable
  condition happens. This project accepts the small loss of
  portability that occurs by checking for errors.

Coding style:

- We have 3 types of errors and one type of failure.

  - First we have function failure which is an error. Functions can
    fail for legitimate reasons. Functions that fail usually return -1
    and set a reason in errno.

  - We have `LINTED_FATAL_ERROR` for errors that must terminate the
    process. They exit and return an error code to the controller of
    the process.

  - We have `LINTED_IMPOSSIBLE_ERROR` for fundamental errors such as
    programmer logic errors and memory corruption. These are
    assertions that abort the process. This is preferred over assert
    because it gives more information (it also doesn't have the side
    effect problems of it).

  - We have `LINTED_LAZY_DEV_ERROR` for error cases that the developer
    is too lazy to properly handle at the moment. These should never
    appear in release builds.

- Use the Linux kernel coding style but with an indent level of 4 and
  no tabs.

- Use %i instead of %d in format specifiers.
