dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_CLANG],[
dnl
AC_ARG_VAR([CLANG], [The clang compiler used for static analysis])
dnl
test -z "$CLANG" && AC_CHECK_TOOLS([CLANG], [clang])
])
