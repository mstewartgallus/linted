dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_FRAMA_C],[
dnl
AC_ARG_VAR([FRAMA_C], [The frama-c tool])
dnl
test -z "$FRAMA_C" && AC_CHECK_TOOLS([FRAMA_C], [frama-c])
])
