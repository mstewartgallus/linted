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
AC_INIT([Linted], [0.0], [sstewartgallus00@mylangara.bc.ca],,
                  [https://www.gitorious.org/linted])
dnl
dnl Other software uses a shell command to get the date but that
dnl prevents deterministic builds.
AC_DEFINE([COPYRIGHT_YEAR], ["2014"], [year in copyright message])
dnl
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([subdir-objects -Wall std-options])
AC_CONFIG_HEADER([config.h])
dnl
AC_PREREQ([2.62])
AC_CONFIG_SRCDIR([src/init/init.c])
dnl
AC_ARG_VAR([CLANG], [The clang compiler used for static analysis])
dnl
AS_IF([test "x${CFLAGS}" = "x"], [
  [CFLAGS='']
])
AM_PROG_AR
LT_INIT
AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O
dnl
AC_CHECK_HEADERS([dnl
        limits.h dnl
        fcntl.h dnl
        float.h dnl
        stddef.h dnl
        stdint.h dnl
        stdlib.h dnl
        string.h dnl
        syslog.h dnl
        unistd.h], [], [
  AC_MSG_ERROR([Unable to find header ${ac_header}])
], [])
dnl
AC_CHECK_HEADERS([sys/resource.h])
dnl
AC_HEADER_STDBOOL
AS_IF([test "x${ac_cv_header_stdbool_h}" = "xno"],[
 AC_MSG_ERROR([Unable to find header stdbool.h (conforming to C99)])
])
dnl
AC_FUNC_MALLOC
AS_IF([test "x${ac_cv_func_malloc}" = "xno"],[
 AC_MSG_ERROR([Unable to find malloc function for which malloc(0) != NULL])
])
AC_FUNC_REALLOC
AS_IF([test "x${ac_cv_func_realloc}" = "xno"],[
 AC_MSG_ERROR([Unable to find realloc function for which realloc(NULL, 0) != NULL])
])
AC_FUNC_STRERROR_R
AS_IF([test "x${ac_cv_func_strerror_r}" = "xno"],[
 AC_MSG_ERROR([Unable to find strerror_r function])
])
AC_FUNC_FORK
AS_IF([test "x${ac_cv_func_fork}" = "xno"],[
 AC_MSG_ERROR([Unable to find good fork function])
])
AC_CHECK_FUNCS([memset select setenv strchr], [], [
 AC_MSG_ERROR([Unable to find function ${ac_func}])
])
dnl
AC_CHECK_FUNCS([environ], [], [
 AC_MSG_ERROR([Unable to find global variable ${ac_func}])
])
dnl
AC_TYPE_UINT64_T
dnl
AC_TYPE_SSIZE_T
AC_TYPE_SIZE_T
AC_TYPE_PID_T
AC_TYPE_UID_T
dnl
AC_CHECK_TYPE([errno_t], [], [
    AC_DEFINE([errno_t], [int], [error number type])
], [[#include <errno.h>]])
dnl
LINTED_PROG_BLENDER
LINTED_PROG_CBMC
LINTED_PROG_GOTO_CC
LINTED_PROG_CLANG
LINTED_PROG_CPPCHECK
LINTED_PROG_LUA
LINTED_PROG_FRAMA_C_GUI
LINTED_PROG_FRAMA_C
LINTED_PROG_INDENT
dnl
LINTED_LANGUAGE
LINTED_OPTIMIZE
LINTED_HARDEN
LINTED_DEBUG
LINTED_WARNINGS
dnl
AC_CHECK_LIB([GL], [glClear], [
  [GL_LIBS="-lGL"]
  AC_SUBST([GL_LIBS])
], [
  AC_MSG_ERROR([Unable to find an OpenGL library])
])
dnl
AM_CONDITIONAL([HAS_FRAMA_C], [test "x${FRAMA_C}" != "x"])
AM_CONDITIONAL([HAS_CBMC], [test "x${CBMC}" != "x"])
AM_CONDITIONAL([HAS_CLANG], [test "x${CLANG}" != "x"])
AM_CONDITIONAL([HAS_CPPCHECK], [test "x${CPPCHECK}" != "x"])
dnl
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile])
dnl
[CC="${srcdir}/scripts/ccwrapper '${CLANG}' '${CC}'"]
AC_CONFIG_SUBDIRS([libs/SDL])
AC_OUTPUT
