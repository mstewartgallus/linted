dnl -*-Autoconf-*-
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
LINTED_CHECK_CFLAGS([linted_CFLAGS_HARDEN],[
        [-fstack-protector-all -Wstack-protector]dnl
        [-fsanitize=address]dnl
        [-pie -fPIE]dnl
        [-trapv]dnl
        [-fmemsafety]])
AC_SUBST([linted_CFLAGS_HARDEN])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_HARDEN],[
        [-Wl,-z,relro]dnl
        [-Wl,-z,now]dnl
        [-Wl,-z,noexecstack]])
AC_SUBST([linted_LDFLAGS_HARDEN])
dnl
])
])
