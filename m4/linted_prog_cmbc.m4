dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_CBMC],[
dnl
AC_ARG_VAR([CBMC], [The cmbc bounded model checker])
dnl
test -z "${CBMC}" && AC_CHECK_TOOLS([CBMC], [cbmc])
])
