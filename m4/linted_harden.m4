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
                [disable hardening flags (not advised)]))
dnl
AS_IF([test "x${enable_harden}" != "xno"], [
dnl
dnl No way to test this so it's added always. It won't harm programs
dnl if it doesn't work.
[linted_CPPFLAGS_HARDEN=-D_FORTIFY_SOURCE=2]
AC_SUBST([linted_CPPFLAGS_HARDEN])
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_HARDEN],[dnl
        [-fstack-protector-all -Wstack-protector]dnl
        [-pie -fPIE]dnl
        [-ftrapv]dnl
        [-fmemsafety]dnl
        [-fsanitize=address-all]dnl
        [-Wa,--noexecstack]dnl
])
AC_SUBST([linted_CFLAGS_HARDEN])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_HARDEN],[dnl
        [-pie -fPIE]dnl
        [-Wl,-z,relro]dnl
        [-Wl,-z,now]dnl
        [-Wl,-z,noexecstack]dnl
        [-fsanitize=address-all]dnl This flag must appear in both the
                                dnl linker options and compiler
                                dnl options to link against the
                                dnl appropriate runtime libraries.
])
AC_SUBST([linted_LDFLAGS_HARDEN])
dnl
])
])
