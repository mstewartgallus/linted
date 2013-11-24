dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_FRAMA_C],[
dnl
AC_ARG_VAR([FRAMA_C], [The frama-c tool])
dnl
AC_REQUIRE([AC_EXEEXT])
test -z "$FRAMA_C" && AC_CHECK_PROGS([FRAMA_C], [frama-c$EXEEXT])
])
