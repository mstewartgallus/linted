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
AC_DEFUN([LINTED_MINOR_DEBUG],[
dnl
AC_ARG_ENABLE(
        [debug],
        AS_HELP_STRING(
                [--enable-minor-debug],
                [enable minor debug mode features (still secure)]))
dnl
AS_IF([test "x${enable_minor_debug}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_MINOR_DEBUG],[dnl
        [-g]dnl
        [-fno-omit-frame-pointer]dnl
])
dnl
AC_SUBST([linted_CFLAGS_MINOR_DEBUG])
dnl
])
])
