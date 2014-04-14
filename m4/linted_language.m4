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
dnl -pthread appears twice because it sets preprocessor options and
dnl linker options
dnl
LINTED_CHECK_CFLAGS([linted_CPPFLAGS_LANGUAGE],[
        [-pthread]dnl
        [-D_POSIX_C_SOURCE=200809L]dnl Will always succeed but won't
                                   dnl be harmful.
])
AC_SUBST([linted_CPPFLAGS_LANGUAGE])
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_LANGUAGE],[
        [-std=c99]dnl
dnl        [-pedantic-errors]dnl
        [-pthread]dnl
])
AC_SUBST([linted_CFLAGS_LANGUAGE])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_LANGUAGE],[
        [-pthread]dnl
])
AC_SUBST([linted_LDFLAGS_LANGUAGE])
])
