dnl -*-Autoconf-*-
dnl Enables debug flags 
AC_DEFUN([LINTED_DEBUG],[
dnl
AC_ARG_ENABLE(
        [debug],
        AS_HELP_STRING(
                [--disable-debug],
                [disable debug mode]))
dnl
AS_IF([test "x${enable_debug}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_DEBUG],[[-g]])
dnl
AC_SUBST([linted_CFLAGS_DEBUG])
])
])
