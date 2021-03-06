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
[linted_checked_lib=]$2
[linted_old_LIBS="${LIBS}"]
dnl
AC_LANG_PUSH([C])
dnl
AC_MSG_CHECKING([for library containing ]$3)
[LIBS="${LIBS} ${linted_checked_lib}"]
AC_LINK_IFELSE([AC_LANG_SOURCE([dnl
$4
[#if defined _WIN32]
[#define WIN32_LEAN_AND_MEAN]
[#define _UNICODE]
[#include <windows.h>]
[#if defined UNICODE]
[int WINAPI wWinMain(HINSTANCE program_instance,]
[                    HINSTANCE prev_instance_unused,]
[                    wchar_t *command_line_unused, int show_command_arg)]
[{]
[#else]
[int WINAPI WinMain(HINSTANCE program_instance,]
[                   HINSTANCE prev_instance_unused,]
[                   char *command_line_unused, int show_command_arg)]
[{]
[#endif]
[#else]
[int main(void) {]
[#endif]
[        static void (*volatile my_value_f7owgovm)(void);]
[        my_value_f7owgovm = (void(*)(void))]$3[;]
[        return 0;]
[}]dnl
])], [
         AC_MSG_RESULT(${linted_checked_lib})
         $1[="${linted_checked_lib}"]
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
