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
AC_DEFUN([LINTED_CHECK_LDFLAG],[
dnl
[linted_old_LDFLAGS="${LDFLAGS}"]
dnl
AC_LANG_PUSH([C])
dnl
AC_MSG_CHECKING([for linker flag $2])
[LDFLAGS=]$2
dnl
AC_LINK_IFELSE([AC_LANG_SOURCE([
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
[        return 0;]
[}]
])], [
         AC_MSG_RESULT([yes])
         $1[="$LDFLAGS"]
], [
         $1[='']
         AC_MSG_RESULT([no])
])
dnl
[LDFLAGS="${linted_old_LDFLAGS}"]
dnl
AC_LANG_POP([C])
])
