dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Enables debug infromation
AC_DEFUN([LINTED_DEBUG_INFO],[
dnl
AC_ARG_ENABLE(
        [debug-info],
        AS_HELP_STRING(
                [--disable-debug-info],
                [disable debug info]),
        [
         [enable_debug_info=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_debug_info=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_debug_info=no]])
         AS_IF([test "x${enable_debug_info}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-debug-info])])
        ],
        [[enable_debug_info='yes']])
dnl
AS_IF([test "x${enable_debug_info}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_DEBUG_INFO],[dnl
        dnl Get as much debug info as possible
        [-fno-omit-frame-pointer]dnl
        [-g]dnl
        [-g3]dnl
        [-ggdb]dnl
        [-fvar-tracking-assignments]dnl
        dnl
        dnl Allow better backtraces
        [-rdynamic]dnl
])
dnl
AC_SUBST([linted_CFLAGS_DEBUG_INFO])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_DEBUG_INFO],[dnl
        [-Wl,--gdb-index]dnl
        [-Wl,-export-dynamic]dnl
        [-Wl,--export-all-symbols]dnl
])
dnl
AC_SUBST([linted_LDFLAGS_DEBUG_INFO])
])
])
