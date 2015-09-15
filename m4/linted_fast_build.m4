dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Speeds up the build

AC_DEFUN([LINTED_FAST_BUILD],[
dnl
AC_ARG_ENABLE(
        [fast-build],
        AS_HELP_STRING(
                [--disable-fast-build],
                [disable adjustments for speeding up the build]),
        [
         [enable_fast_build=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_fast_build=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_fast_build=no]])
         AS_IF([test "x${enable_fast_build}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-fast-build])])
        ],
        [[enable_fast_build='yes']])
dnl
AS_IF([test "x${enable_fast_build}" != "xno"], [
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_FAST_BUILD],[dnl
        [-fuse-ld=gold]
        [-fintegrated-as]       dnl Clang's integrated ASM is faster
        [-Wa,--compress-debug-sections]
])
dnl
])
])
