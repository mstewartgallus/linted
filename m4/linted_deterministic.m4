dnl -*-Autoconf-*-
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl Makes the build more deterministic. Still probably not accomplished yet.
AC_DEFUN([LINTED_DETERMINISTIC],[
dnl
AC_ARG_ENABLE(
        [deterministic],
        AS_HELP_STRING(
                [--disable-deterministic],
                [disable adjustments for making the build more deterministic]),
        [
         [enable_deterministic=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_deterministic=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_deterministic=no]])
         AS_IF([test "x${enable_deterministic}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-deterministic])])
        ],
        [[enable_deterministic='yes']])
dnl
AM_CONDITIONAL([LINTED_DETERMINISTIC], [test "x${enable_deterministic}" != "xno"])
dnl
AS_IF([test "x${enable_deterministic}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_DETERMINISTIC],[
dnl
	[-fno-working-directory]dnl
dnl
        [-gno-record-gcc-switches]dnl
	[-fdebug-prefix-map=${srcdir}=.]dnl
])
AC_SUBST([linted_CFLAGS_DETERMINISTIC])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_DETERMINISTIC],[
dnl     We might want to change this to be a hash or set it manually later
	[-Wl,--build-id=none]dnl
])
AC_SUBST([linted_LDFLAGS_DETERMINISTIC])
dnl
AC_MSG_CHECKING([for ar flag D])
touch lib.test
if "${AR}" crD lib.test.a lib.test
then
	[AR_FLAGS='crD']
	rm lib.test.a
        AC_MSG_RESULT([D])
else
	[AR_FLAGS='']
        AC_MSG_RESULT([no])
fi
touch lib.test
"${AR}" cr lib.test.a lib.test
AC_MSG_CHECKING([for ranlib flag -D])
if "${RANLIB}" -D lib.test.a
then
	[RANLIB="${RANLIB} -D"]
        AC_MSG_RESULT([-D])
else
        AC_MSG_RESULT([no])
fi
rm lib.test lib.test.a
AC_SUBST([AR_FLAGS])
dnl
])
])
