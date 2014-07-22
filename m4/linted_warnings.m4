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
dnl
        [-Wdate-time]dnl Reproducible builds are GOOD
dnl
        [-Woverflow]dnl
dnl
        [-Wzero-as-null-pointer-constant]dnl
        [-Wformat=2]dnl
        [-Wunused] [-Wno-unused-parameter]dnl
        [-Wshadow]dnl
        [-Wwrite-strings]dnl
        [-Wstrict-prototypes]dnl
        [-Wold-style-definition]dnl
        [-Wredundant-decls]dnl
        [-Wnested-externs]
        [-Wlogical-op]dnl
        [-Wstrict-aliasing=3]dnl
        [-Wstrict-overflow=5]dnl
        [-Wtrampolines]dnl We force a nonexecutable stack so for most
                       dnl targets would generate broken code if we
                       dnl used trampolines.
])
AC_SUBST([linted_CFLAGS_WARNINGS])
])
