dnl -*-Autoconf-*-
AC_DEFUN([LINTED_PROG_INDENT],[
dnl
AC_ARG_VAR([INDENT], [The indent indentation tool])
dnl
test -z "${INDENT}" && AC_CHECK_TOOLS([INDENT], [indent])
])
