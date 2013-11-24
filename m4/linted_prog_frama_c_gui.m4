dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_FRAMA_C_GUI],[
dnl
AC_ARG_VAR([FRAMA_C_GUI], [The frama-c-gui tool])
dnl
AC_REQUIRE([AC_EXEEXT])
test -z "$FRAMA_C_GUI" && AC_CHECK_PROGS([FRAMA_C_GUI], [frama-c-gui$EXEEXT])
])
