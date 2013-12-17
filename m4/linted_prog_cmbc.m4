dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_CBMC],[
dnl
AC_ARG_VAR([CBMC], [The cmbc bounded model checker])
dnl
AC_REQUIRE([AC_EXEEXT])
test -z "${CBMC}" && AC_CHECK_PROGS([CBMC], ["cbmc${EXEEXT}"])
])
