dnl -*-Autoconf-*-
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
        [CFLAGS=']linted_the_flag[']
        AC_LANG_WERROR
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[]])],
                 $1[="$]$1[ $CFLAGS"])
])
dnl
[CFLAGS="${linted_old_CFLAGS}"]
dnl
AC_LANG_POP([C])
])
