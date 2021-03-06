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
[linted_old_werror_flag="${ac_c_werror_flag}"]
dnl
[ac_c_werror_flag='yes']
dnl
AC_LANG_PUSH([C])
$1[='']
m4_foreach_w([linted_the_flag], $2, [
        [linted_the_flag_var=]linted_the_flag

        AC_MSG_CHECKING([for C compiler flag ${linted_the_flag_var}])
        [linted_old_checked_CFLAGS="${CFLAGS}"]
        [CFLAGS="${CFLAGS} ${linted_the_flag_var}"]
dnl
        [ac_c_werror_flag='yes']
dnl
        dnl ISO C requires a nonempty translation unit
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
            void nonempty_translation_unit_E5XlWC(void) { }
        ]])], [
                 AC_MSG_RESULT([yes])
                 $1[="$]$1[ ${linted_the_flag_var}"]
        ],
        [
                 AC_MSG_RESULT([no])
                 [CFLAGS="${linted_old_checked_CFLAGS}"]
        ])
])
dnl
[ac_c_werror_flag="${linted_old_werror_flag}"]
[CFLAGS="${linted_old_CFLAGS}"]
dnl
AC_LANG_POP([C])
])
