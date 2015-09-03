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
AC_REQUIRE([AC_CANONICAL_HOST])
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
dnl
dnl Stack protection seems to mess up Wine
dnl        [-Wl,-Bstatic -fstack-protector-all -Wl,-Bdynamic -Wstack-protector]dnl
])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_HARDEN],[dnl
        [-Wl,-z,relro]dnl
        [-Wl,-z,now]dnl
        [-Wl,--rosegment]dnl
dnl
dnl     DEP for Windows
        [-Wl,--nxcompat]dnl
dnl
dnl     Enforce code integrity checks on Windows
        [-Wl,--forceinteg]dnl
dnl
dnl     Use ASLR on Windows
        [-Wl,--dynamicbase]dnl
])
dnl
AS_IF([test "x${host_os}" != "xmingw32" &&
       test "x${host_os}" != "xmingw32msvc" ], [
dnl     Use ASLR on ELF
        LINTED_CHECK_CFLAGS([linted_CFLAGS_HARDEN_ASLR],[dnl
                  [-fpic -fPIC -fpie -fPIE]dnl
        ])
        [linted_CFLAGS_HARDEN="${linted_CFLAGS_HARDEN} ${linted_CFLAGS_HARDEN_ASLR}"]
dnl
        LINTED_CHECK_LDFLAGS([linted_LDFLAGS_HARDEN_ASLR],[dnl
                  [-pie -fpic -fPIC -fpie -fPIE]dnl
        ])
        [linted_LDFLAGS_HARDEN="${linted_LDFLAGS_HARDEN} ${linted_LDFLAGS_HARDEN_ASLR}"]
])
dnl
AC_SUBST([linted_CFLAGS_HARDEN])
AC_SUBST([linted_LDFLAGS_HARDEN])
dnl
])
])
