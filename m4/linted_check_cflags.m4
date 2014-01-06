dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Autodetects if a compiler flag is valid. This autoconf macro is
dnl invoked like LINTED_CHECK_CFLAGS([myvar], [-fa -fb])
dnl
AC_DEFUN([LINTED_CHECK_CFLAGS],[
dnl
[linted_old_CFLAGS="${CFLAGS}"]
dnl
AC_LANG_PUSH([C])
m4_foreach_w([linted_the_flag], $2, [
        AC_MSG_CHECKING([for C compiler flag linted_the_flag])
        [CFLAGS=']linted_the_flag[']
        AC_LANG_WERROR
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[]])], [
                 AC_MSG_RESULT([yes])
                 $1[="$]$1[ $CFLAGS"]
        ],
        [
                 AC_MSG_RESULT([no])
        ])
])
dnl
[CFLAGS="${linted_old_CFLAGS}"]
dnl
AC_LANG_POP([C])
])
