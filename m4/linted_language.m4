dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Autodetects and sets language settings
AC_DEFUN([LINTED_LANGUAGE],[
dnl
dnl No way to test so always add. It should do no harm if it doesn't
dnl work.
dnl
dnl Use POSIX library functions
[linted_CPPFLAGS_LANGUAGE=-D_POSIX_C_SOURCE=200112L]
AC_SUBST([linted_CPPFLAGS_LANGUAGE])
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_LANGUAGE],[
        [-std=c99]dnl
        [-pedantic]dnl
])
AC_SUBST([linted_CFLAGS_LANGUAGE])
])
