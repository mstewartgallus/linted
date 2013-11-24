dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_CLANG],[
dnl
AC_ARG_VAR([CLANG], [The clang compiler used for static analysis])
dnl
AC_REQUIRE([AC_EXEEXT])
test -z "$CLANG" && AC_CHECK_PROGS([CLANG], [clang$EXEEXT])
])
