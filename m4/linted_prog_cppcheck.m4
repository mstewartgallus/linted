dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_CPPCHECK],[
dnl
AC_ARG_VAR([CPPCHECK], [The cppcheck analyzer tool])
AC_ARG_VAR([CPPCHECKFLAGS], [cppcheck analyzer flags])
dnl
AC_REQUIRE([AC_EXEEXT])
test -z "$CPPCHECK" && AC_CHECK_PROGS([CPPCHECK], [cppcheck$EXEEXT])
])
