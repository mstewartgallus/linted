dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
AC_DEFUN([LINTED_PROG_FRAMA_C_GUI],[
dnl
AC_ARG_VAR([FRAMA_C_GUI], [The frama-c-gui tool])
dnl
test -z "${FRAMA_C_GUI}" && AC_CHECK_TOOLS([FRAMA_C_GUI], [frama-c-gui])
])
