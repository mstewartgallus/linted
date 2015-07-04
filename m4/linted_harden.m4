dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Autodetects compiler warnings
AC_DEFUN([LINTED_HARDEN],[
dnl
AC_ARG_ENABLE(
        [harden],
        AS_HELP_STRING(
                [--disable-harden],
                [disable hardening flags (not advised)]),
        [
         [enable_harden_api=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_harden_api=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_harden_api=no]])
         AS_IF([test "x${enable_harden_api}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-harden-api])])
        ],
        [[enable_harden='yes']])
dnl
AS_IF([test "x${enable_harden}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_HARDEN],[dnl
dnl No way to test this so it's added always. It won't harm programs
dnl if it doesn't work.
        [-D_FORTIFY_SOURCE=2]dnl
        [-fstack-protector-all -Wstack-protector]dnl
        [-pie -fPIE]dnl
])
AC_SUBST([linted_CFLAGS_HARDEN])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_HARDEN],[dnl
        [-pie -fPIE]dnl
        [-Wl,-z,relro]dnl
        [-Wl,-z,now]dnl
])
AC_SUBST([linted_LDFLAGS_HARDEN])
dnl
])
])
