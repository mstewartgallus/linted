dnl -*-Autoconf-*-
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
LINTED_CHECK_CFLAGS([linted_CFLAGS_OPTIMIZE],[
        [-mtune=native]
        [-march=native]
        [-O]
        [-O2]
        [-O3]
])
dnl
dnl Link time optimization needs to be supported by the linker as well.
LINTED_CHECK_LDFLAGS([linted_CFLAGS_OPTIMIZE_LTO],[[-flto]])
[linted_CFLAGS_OPTIMIZE="${linted_CFLAGS_OPTIMIZE} ${linted_CFLAGS_OPTIMIZE_LTO}"]
dnl
AC_SUBST([linted_CFLAGS_OPTIMIZE])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_OPTIMIZE],[
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
