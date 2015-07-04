dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Disables assertions
AC_DEFUN([LINTED_ASSERT],[
dnl
AC_ARG_ENABLE(
        [assert],
        AS_HELP_STRING(
                [--disable-assert],
                [disable assertions (still secure)]),
        [
         [enable_assert=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_assert=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_assert=no]])
         AS_IF([test "x${enable_assert}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-assert])])
        ], [[enable_assert='yes']])
dnl
AS_IF([test "x${enable_assert}" = "xno"], [],[
LINTED_CHECK_CFLAGS([linted_CFLAGS_DEBUG],[dnl
        [-DNDEBUG]dnl
])
AC_SUBST([linted_CFLAGS_DEBUG])
])
])
