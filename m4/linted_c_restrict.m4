dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Detects if restrict is supported. This autoconf macro is
dnl invoked like LINTED_C_RESTRICT
dnl
dnl Needed because AC_C_RESTRICT checks in completely the wrong order
dnl for restrict qualifiers.
dnl
AC_DEFUN([LINTED_C_RESTRICT],[
dnl
AC_LANG_PUSH([C])
dnl
AC_MSG_CHECKING([for restrict qualifier])
dnl
[linted_restrict_qualifier=no]
for linted_qualifier in restrict  _Restrict __restrict __restrict__; do
        dnl ISO C requires a nonempty translation unit
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
                typedef char * $qualifier random_identifier_HG4ns;
                void nonempty_translation_unit_E5XlWC(void) { }
        ]])], [linted_restrict_qualifier="$linted_qualifier"])
dnl
        [test "$linted_restrict_qualifier" != no && break]
done
dnl
AS_IF([test "x${linted_restrict_qualifier}" = 'xno'], [
        AC_MSG_RESULT([no])
], [
        AC_MSG_RESULT([yes])
        AS_IF([test "x${linted_restrict_qualifier}" = 'xrestrict'], [
        ], [
                     AC_DEFINE_UNQUOTED([restrict], ["${linted_restrict_qualifier}"], [restrict qualifier])
        ])
])
dnl
AC_LANG_POP([C])
])
