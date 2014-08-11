dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Selects optimizations
AC_DEFUN([LINTED_OPTIMIZE],[
dnl
AC_ARG_ENABLE(
        [optimize],
        AS_HELP_STRING(
                [--disable-optimize],
                [disable optimization flags (useful for debugging, portability)]))
dnl
AS_IF([test "x${enable_optimize}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_OPTIMIZE],[dnl
        [-O]dnl
        [-O2]dnl
        [-O3]dnl
dnl
        [-fno-common]dnl This gives better performance and stops
                     dnl sloppy code
dnl
        [-fvisibility=hidden]dnl This gets around a few redirections
                             dnl and prevents symbol conflicts
])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_OPTIMIZE],[dnl
        [-fuse-linker-plugin]dnl
        [-Wl,-O1]dnl
        [-Wl,--sort-common]dnl
        [-Wl,--hash-style=gnu]dnl
        [-Wl,--as-needed]dnl
        [-Wl,--gc-sections]dnl
        [-Wl,-z,combreloc]])
AC_SUBST([linted_LDFLAGS_OPTIMIZE])
dnl
])
])
