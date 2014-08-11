dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Enables debug flags
AC_DEFUN([LINTED_DEBUG],[
dnl
AC_ARG_ENABLE(
        [debug],
        AS_HELP_STRING(
                [--enable-debug],
                [enable debug mode (insecure!)]))
dnl
AS_IF([test "x${enable_debug}" = "xyes"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_DEBUG],[dnl
        [-ftrapv]dnl
        [-fsanitize=address]dnl
        [-fsanitize=undefined]dnl
])
dnl
AC_SUBST([linted_CFLAGS_DEBUG])
dnl
dnl Sanitizer flags must appear in both the linker options and
dnl compiler options to link against the appropriate runtime
dnl libraries.
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_DEBUG],[dnl
        [-fsanitize=address]dnl
        [-fsanitize=undefined]dnl
])
AC_SUBST([linted_LDFLAGS_HARDEN])
dnl
], [
LINTED_CHECK_CFLAGS([linted_CFLAGS_DEBUG],[dnl
        [-DNDEBUG]dnl
])
AC_SUBST([linted_CFLAGS_DEBUG])
])
])
