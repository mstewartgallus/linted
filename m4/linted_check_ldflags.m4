dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Autodetects if an ld flag is valid. This autoconf macro is
dnl invoked like LINTED_CHECK_LDFLAGS([myvar], [-fa -fb])
dnl
AC_DEFUN([LINTED_CHECK_LDFLAGS],[
dnl
[linted_old_LDFLAGS="${LDFLAGS}"]
dnl
AC_LANG_PUSH([C])
m4_foreach_w([linted_the_flag], $2, [
         AC_MSG_CHECKING([for linker flag linted_the_flag])
         [LDFLAGS=']linted_the_flag[']
         AC_LINK_IFELSE([AC_LANG_SOURCE([[int main() { return 0; }]])], [
                 AC_MSG_RESULT([yes])
                 $1[="$]$1[ $LDFLAGS"]
         ], [
                 AC_MSG_RESULT([no])
         ])
])
dnl
[LDFLAGS="${linted_old_LDFLAGS}"]
dnl
AC_LANG_POP([C])
])
