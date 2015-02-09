dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Autodetects if a library is valid. This autoconf macro is
dnl invoked like LINTED_CHECK_LIB([myvar_lib], [m], [sin])
dnl
AC_DEFUN([LINTED_CHECK_LIB],[
dnl
[linted_old_LIBS="${LIBS}"]
dnl
AC_LANG_PUSH([C])
dnl
AC_MSG_CHECKING([for library containing ]$3)
[LIBS='-l]$2[']
AC_LINK_IFELSE([AC_LANG_SOURCE([[extern char ]$3[(void);][int main() { ]$3[(); return 0; }]])], [
         AC_MSG_RESULT([-l]$2)
         $1[='-l]$2[']
         $4
], [
         AC_MSG_RESULT([no])
         $5
])
dnl
AC_LANG_POP([C])
dnl
[LIBS="${linted_old_LIBS}"]
dnl
AC_SUBST($1)
])
