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
dnl invoked like LINTED_CHECK_LIB([myvar_lib], [-lm], [sin], [#include <math.h>])
dnl
AC_DEFUN([LINTED_CHECK_LIB],[
dnl
[linted_old_LIBS="${LIBS}"]
dnl
AC_LANG_PUSH([C])
dnl
AC_MSG_CHECKING([for library containing ]$3)
[LIBS="${LIBS} "']$2[']
AC_LINK_IFELSE([AC_LANG_SOURCE([dnl
$4
[int main() {]
[        static void (*volatile my_value_f7owgovm)(void);]
[        my_value_f7owgovm = (void(*)(void))]$3[;]
[        return 0;]
[}]dnl
])], [
         AC_MSG_RESULT($2)
         $1[=']$2[']
         $5
], [
         AC_MSG_RESULT([no])
         $6
])
dnl
AC_LANG_POP([C])
dnl
[LIBS="${linted_old_LIBS}"]
dnl
AC_SUBST($1)
])
