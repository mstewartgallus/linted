dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_FRAMA_C_GUI],[
dnl
AC_ARG_VAR([FRAMA_C_GUI], [The frama-c-gui tool])
dnl
test -z "$FRAMA_C_GUI" && AC_CHECK_TOOLS([FRAMA_C_GUI], [frama-c-gui])
])
