dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Enables extra meta information
AC_DEFUN([LINTED_META],[
dnl
AC_ARG_ENABLE(
        [meta],
        AS_HELP_STRING(
                [--disable-meta],
                [disable meta info]),
        [
         [enable_meta_info=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_meta_info=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_meta_info=no]])
         AS_IF([test "x${enable_meta_info}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-meta-info])])
        ],
        [[enable_meta_info='yes']])
dnl
AS_IF([test "x${enable_meta_info}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_META],[dnl
        [-fstack-usage]dnl
        [-save-temps]dnl
        [-save-temps=obj]dnl
])
dnl
AC_SUBST([linted_CFLAGS_META])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_META],[dnl
])
dnl
AC_SUBST([linted_LDFLAGS_META])
])
])
