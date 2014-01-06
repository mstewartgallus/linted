dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
AC_DEFUN([LINTED_PROG_INDENT],[
dnl
AC_ARG_VAR([INDENT], [The indent indentation tool])
dnl
test -z "${INDENT}" && AC_CHECK_PROGS([INDENT], [indent])
])
