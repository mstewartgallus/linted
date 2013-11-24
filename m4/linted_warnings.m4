dnl -*-Autoconf-*-
dnl Autodetects compiler warnings
AC_DEFUN([LINTED_WARNINGS],[
LINTED_CHECK_CFLAGS([linted_CFLAGS_WARNINGS],[
        [-Wall]dnl
        [-Wextra]dnl
        [-Wformat=2]dnl
        [-Wunused] [-Wno-unused-parameter]dnl
        [-Wshadow]dnl
        [-Wwrite-strings]dnl
        [-Wstrict-prototypes]dnl
        [-Wold-style-definition]dnl
        [-Wredundant-decls]dnl
        [-Wnested-externs]
        [-Wjump-misses-init]dnl
        [-Wlogical-op]dnl
])
AC_SUBST([linted_CFLAGS_WARNINGS])
])
