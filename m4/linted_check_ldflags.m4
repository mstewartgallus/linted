dnl -*-Autoconf-*-
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
         [LDFLAGS=']linted_the_flag[']
         AC_LINK_IFELSE([AC_LANG_SOURCE([[int main() { return 0; }]])],
                 $1[="$]$1[ $LDFLAGS"])
])
dnl
[LDFLAGS="${linted_old_LDFLAGS}"]
dnl
AC_LANG_POP([C])
])
