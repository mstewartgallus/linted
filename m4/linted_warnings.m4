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
        [-Wlogical-op]dnl
])
AC_SUBST([linted_CFLAGS_WARNINGS])
])
