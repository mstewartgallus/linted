dnl Process this file with autoconf to produce a configure script.
dnl
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without
dnl modifications, as long as this notice is preserved.
dnl
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
AC_INIT([Linted], [1.0], [sstewartgallus00@mylangara.bc.ca])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([subdir-objects -Wall std-options])
AC_CONFIG_HEADER([config.h])
dnl
AC_PREREQ([2.62])
AC_CONFIG_SRCDIR([src/bin/linted.c])
dnl
AC_ARG_VAR([CLANG], [The clang compiler used for static analysis])
AC_ARG_ENABLE(
        [save-temps],
        AS_HELP_STRING(
                [--enable-save-temps],
                [save temporary files (useful for debugging)]))
dnl
AS_IF([test "x${CFLAGS}" = "x"], [
  [CFLAGS='']
])
AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O
dnl
AC_CHECK_HEADERS([dnl
        fcntl.h dnl
        inttypes.h dnl
        stdint.h dnl
        stdlib.h dnl
        string.h dnl
        sys/time.h dnl
        unistd.h], [], [
  AC_MSG_ERROR([Unable to find header $ac_header])
], [])
dnl
AC_CHECK_HEADERS([sys/resource.h])
dnl
AC_HEADER_STDBOOL
AS_IF([test "x${ac_cv_header_stdbool_h}" = "xno"],[
 AC_MSG_ERROR([Unable to find header stdbool.h (conforming to C99)])
])
AC_TYPE_SIZE_T
AS_IF([test "x${ac_cv_type_size_t}" = "xno"],[
 AC_MSG_ERROR([Unable to find type size_t])
])
AC_TYPE_SSIZE_T
AS_IF([test "x${ac_cv_type_ssize_t}" = "xno"],[
 AC_MSG_ERROR([Unable to find type ssize_t])
])
AC_TYPE_PID_T
AS_IF([test "x${ac_cv_type_pid_t}" = "xno"],[
 AC_MSG_ERROR([Unable to find type pid_t])
])
AC_TYPE_UID_T
AS_IF([test "x${ac_cv_type_uid_t}" = "xno"],[
 AC_MSG_ERROR([Unable to find type uid_t])
])
AC_TYPE_UINT8_T
AS_IF([test "x${ac_cv_type_uint8_t}" = "xno"],[
 AC_MSG_ERROR([Unable to find type uint8_t])
])
dnl
AC_FUNC_MALLOC
AS_IF([test "x${ac_cv_func_malloc}" = "xno"],[
 AC_MSG_ERROR([Unable to find function malloc])
])
AC_FUNC_FORK
AS_IF([test "x${ac_cv_func_fork}" = "xno"],[
 AC_MSG_ERROR([Unable to find function fork])
])
AC_CHECK_FUNCS([strerror], [], [
 AC_MSG_ERROR([Unable to find function strerror])
])
dnl
LINTED_CHECK_CFLAGS([linted_CFLAGS_C99_STD],[
        [-std=c99]dnl
        [-pedantic]dnl
])
AC_SUBST([linted_CFLAGS_C99_STD])
dnl
LINTED_PROG_FRAMA_C_GUI
LINTED_PROG_FRAMA_C
LINTED_PROG_CBMC
LINTED_PROG_CLANG
LINTED_PROG_CPPCHECK
dnl
LINTED_OPTIMIZE
LINTED_HARDEN
LINTED_DEBUG
LINTED_WARNINGS
dnl
PKG_CHECK_MODULES([SDL], [sdl >= 1.2.14])
dnl
AC_SEARCH_LIBS([glClear], [GL], [], [
  AC_MSG_ERROR([Unable to find an OpenGL library])
])
dnl
AM_CONDITIONAL([SAVE_TEMPS], [test "x${enable_save_temps}" = "xyes"])
AM_CONDITIONAL([HAS_FRAMA_C], [test "x${FRAMA_C}" != "x"])
AM_CONDITIONAL([HAS_CBMC], [test "x${CBMC}" != "x"])
AM_CONDITIONAL([HAS_CLANG], [test "x${CLANG}" != "x"])
AM_CONDITIONAL([HAS_CPPCHECK], [test "x${CPPCHECK}" != "x"])
dnl
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile])
dnl
AC_OUTPUT
