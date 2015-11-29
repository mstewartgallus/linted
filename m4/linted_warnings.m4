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
dnl
AC_ARG_ENABLE(
        [warnings],
        AS_HELP_STRING(
                [--disable-warnings],
                [disable warnings]),
        [
         [enable_warnings=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_warnings=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_warnings=no]])
         AS_IF([test "x${enable_warnings}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --disable-warnings])])
        ],
        [[enable_warnings='yes']])
dnl
AS_IF([test "x${enable_warnings}" != "xno"], [
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_WARNINGS],[
        [-Qunused-arguments]dnl
        [-Werror=unknown-warning-option]dnl
dnl
        [-Wall]dnl
        [-Wextra]dnl
dnl
        [-Wstack-usage=850]dnl
dnl
        [-Wdate-time]dnl Reproducible builds are GOOD
dnl
        [-Woverflow]dnl
        [-Wformat=2]dnl
        [-Wshadow]dnl
        [-Wstrict-prototypes]dnl
        [-Wmissing-prototypes]dnl
        [-Wmissing-declarations]dnl
        [-Wold-style-definition]dnl
        [-Wlogical-op]dnl
        [-Wstrict-aliasing=3]dnl
        [-Wstrict-overflow=5]dnl
        [-Wtrampolines]dnl We force a nonexecutable stack so for most
                       dnl targets would generate broken code if we
                       dnl used trampolines.
dnl
        [-Wunused]dnl
        [-Wno-unused-parameter]dnl
dnl
dnl     Make certain optimized implementations not cause warnings
        [-Wno-array-bounds]dnl
dnl
dnl     Make the = { 0 } idiom not cause warnings
        [-Wno-missing-field-initializers]dnl
        [-Wno-missing-braces]dnl
])
AC_SUBST([linted_CFLAGS_WARNINGS])
dnl
LINTED_CHECK_LDFLAGS([linted_LDFLAGS_WARNINGS],[
        [-Wl,--warn-common]dnl
        [-Wl,--warn-execstack]dnl
        [-Wl,--warn-search-mismatch]dnl
        [-Wl,--warn-shared-textrel]dnl
        [-Wl,--no-undefined]dnl
        [-Wl,--no-allow-shlib-undefined]dnl
])
AC_SUBST([linted_LDFLAGS_WARNINGS])
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_NO_WARN],[
        [-Wno-everything]dnl
        [-Wno-all]dnl
        [-Wno-extra]dnl
        [-Wno-unused]dnl
])
AC_SUBST([linted_CFLAGS_NO_WARN])
])
])
