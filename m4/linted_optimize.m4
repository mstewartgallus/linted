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
                [disable optimization flags (useful for debugging, portability)]),
        [
         [enable_optimize=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_optimize=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_optimize=no]])
         AS_IF([test "x${enable_optimize}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-optimize])])
        ],
        [[enable_optimize='yes']])
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
        [-Bsymbolic]dnl Also gets around some more redirections
dnl
dnl     This is useful because combined with gc sections it can result in
dnl     omitting dependencies
        [-ffunction-sections]dnl
dnl
        [-flto]dnl
])
AC_SUBST([linted_CFLAGS_OPTIMIZE])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_OPTIMIZE],[dnl
        [-Wl,-O1]dnl
        [-Wl,-X] [-Wl,--discard-locals]dnl Remove compiler generated
                                       dnl symbols
        [-Wl,--sort-common]dnl
        [-Wl,--hash-style=gnu]dnl
        [-Wl,--as-needed]dnl
        [-Wl,--gc-sections]dnl
        [-Wl,-z,combreloc]dnl
dnl
        dnl Use large addresses on i386 Windows
        [-Wl,--large-address-aware]dnl
dnl
        [-flto]dnl
        [-fuse-linker-plugin]dnl
dnl
        [-Wl,--icf=safe]dnl
])
AC_SUBST([linted_LDFLAGS_OPTIMIZE])
dnl
])
])
