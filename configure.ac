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
AC_DEFINE([COPYRIGHT_YEAR], ["2015"], [year in copyright message])
dnl
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([subdir-objects -Wall])
AC_CONFIG_HEADER([config.h])
dnl
AC_PREREQ([2.62])
AC_CONFIG_SRCDIR([src/linted/linted.c])
dnl
AS_IF([test "x${CFLAGS}" = "x"], [
  [CFLAGS='']
])
AM_PROG_AR
LT_INIT
AC_PROG_CC
AM_PROG_CC_C_O
dnl
AC_CANONICAL_HOST
dnl
[linted_posix_api_default=no]
[linted_windows_api_default=no]
dnl
AS_IF([test "x${host_os}" = "xlinux-gnu"], [[linted_posix_api_default=yes]])
AS_IF([test "x${host_os}" = "xmingw32"], [[linted_windows_api_default=yes]])
AS_IF([test "x${host_os}" = "xmingw32msvc"], [[linted_windows_api_default=yes]])
dnl
AS_IF([test "x${CC}" = "xwinegcc"], [
        [linted_windows_api_default=yes]
        [linted_posix_api_default=no]
])
dnl
LINTED_FAST_BUILD
[CC="${CC} ${linted_CFLAGS_FAST_BUILD}"]
dnl
AC_SYS_LARGEFILE
AC_C_INLINE
dnl
AC_CHECK_HEADERS([[fcntl.h]dnl
                  [locale.h]dnl
                  [limits.h]dnl
                  [stddef.h]dnl
                  [stdint.h]dnl
                  [string.h]dnl
                  [unistd.h]], [], [
  AC_MSG_ERROR([Unable to find header ${ac_header}])
], [])
dnl
AC_HEADER_STDBOOL
AS_IF([test "x${ac_cv_header_stdbool_h}" = "xno"],[
 AC_MSG_ERROR([Unable to find header stdbool.h (conforming to C99)])
])
dnl
AC_CHECK_HEADERS([[inttypes.h]], [], [
  AC_MSG_ERROR([Unable to find header ${ac_header}])
], [])
dnl
LINTED_CHECK_LIB([MATH_LIBS], [m], [sin], [], [
   AC_MSG_ERROR([Unable to find a math library])
])
dnl
AC_CHECK_FUNCS([[dup2]dnl
                [memmove]dnl
                [memset]dnl
                [mkdir]dnl
                [strdup]dnl
                [strrchr]dnl
                [strstr]dnl
                [strtol]], [], [
 AC_MSG_ERROR([Unable to find function ${ac_func}])
])
dnl
PKG_CHECK_MODULES(XCB, xcb, [], [
 AC_MSG_ERROR([Unable to find an XCB library])
])
PKG_CHECK_MODULES(XCB_XKB, xcb-xkb, [], [
 AC_MSG_ERROR([Unable to find an xcb-xkb library])
])
dnl
PKG_CHECK_MODULES(XKB, xkbcommon, [], [
 AC_MSG_ERROR([Unable to find an xkbcommon library])
])
PKG_CHECK_MODULES(XKB_X11, xkbcommon-x11, [], [
 AC_MSG_ERROR([Unable to find an xkbcommon-x11 library])
])
dnl
PKG_CHECK_MODULES(EGL, egl, [], [
 AC_MSG_ERROR([Unable to find an EGL library])
])
PKG_CHECK_MODULES(GLES, glesv2, [], [
 AC_MSG_ERROR([Unable to find an OpenGLES version 2 library])
])
dnl
AC_TYPE_MODE_T
dnl
AC_TYPE_INT32_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
dnl
AC_TYPE_OFF_T
dnl
AC_TYPE_SSIZE_T
AC_TYPE_SIZE_T
AC_TYPE_PID_T
AC_TYPE_UID_T
dnl
AC_ARG_ENABLE(
        [posix-api],
        AS_HELP_STRING(
                [--enable-posix-api],
                [enable use of the POSIX API]),
        [[enable_posix_api='yes']],
        [[enable_posix_api="$linted_posix_api_default"]])
AC_ARG_ENABLE(
        [windows-api],
        AS_HELP_STRING(
                [--enable-windows-api],
                [enable use of the Microsoft Windows API]),
                [[enable_windows_api='yes']],
                [[enable_windows_api="$linted_windows_api_default"]])
dnl
AS_IF([test "x${enable_windows_api}" = "xyes"], [
dnl
AC_DEFINE([HAVE_WINDOWS_API], [1], [Define if the Microsoft Windows API is being used.])
dnl
LINTED_CHECK_LIB([WSOCK32_LIBS], [wsock32], [closesocket], [], [
   AC_MSG_ERROR([Unable to find a Winsock library])
])
LINTED_CHECK_LIB([MSWSOCK_LIBS], [mswsock], [AcceptEx], [], [
   AC_MSG_ERROR([Unable to find a Winsock library])
])
dnl
])
dnl
AS_IF([test "x${enable_posix_api}" = "xyes"], [
dnl
AC_DEFINE([HAVE_POSIX_API], [1], [Define if the POSIX API is being used.])
dnl
AC_CHECK_HEADERS([[arpa/inet.h]dnl
                  [mntent.h]dnl
                  [sys/mount.h]dnl
                  [sys/socket.h]dnl
                  [syslog.h]], [], [
  AC_MSG_ERROR([Unable to find header ${ac_header}])
], [])
dnl
LINTED_CHECK_LIB([REALTIME_LIBS], [rt], [clock_gettime], [], [
   AC_MSG_ERROR([Unable to find a realtime functions library])
])
dnl
LINTED_CHECK_LIB([CAP_LIBS], [cap], [cap_set_proc], [], [
   AC_MSG_ERROR([Unable to find a POSIX capabilities library])
])
dnl
AC_CHECK_FUNCS([[clock_gettime]], [], [
 AC_MSG_ERROR([Unable to find function ${ac_func}])
])
AC_FUNC_GETMNTENT
AS_IF([test "x${ac_cv_func_getmntent}" = "xno"],[
 AC_MSG_ERROR([Unable to find getmntent function])
])
AC_FUNC_STRERROR_R
AS_IF([test "x${ac_cv_func_strerror_r}" = "xno"],[
 AC_MSG_ERROR([Unable to find strerror_r function])
])
AC_FUNC_FORK
AS_IF([test "x${ac_cv_func_fork}" = "xno"],[
 AC_MSG_ERROR([Unable to find good fork function])
])
AC_FUNC_MMAP
AS_IF([test "x${ac_cv_func_mmap}" = "xno"],[
 AC_MSG_ERROR([Unable to find good mmap function])
])
dnl
AC_CHECK_FUNCS([[fchdir]dnl
                [gethostname]dnl
                [munmap]dnl
                [setenv]dnl
                [setlocale]dnl
                [socket]dnl
                [strndup]], [], [
 AC_MSG_ERROR([Unable to find function ${ac_func}])
])
dnl
])
dnl
LINTED_PROG_BLENDER
AS_IF([test -z "${BLENDER}"],[
 AC_MSG_ERROR([Unable to find a Blender executable])
])
dnl
LINTED_PROG_CLANG_FORMAT
dnl
LINTED_LANGUAGE
LINTED_OPTIMIZE
LINTED_HARDEN
LINTED_ASSERT
LINTED_DEBUG_INFO
LINTED_DEBUG
LINTED_WARNINGS
dnl
[CC="${CC} ${linted_CFLAGS_LANGUAGE}"
CC="${CC} ${linted_CFLAGS_OPTIMIZE}"
CC="${CC} ${linted_CFLAGS_HARDEN}"
CC="${CC} ${linted_CFLAGS_DEBUG}"
CC="${CC} ${linted_CFLAGS_DEBUG_INFO}"
CC="${CC} ${linted_CFLAGS_WARNINGS}"]
dnl
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([scripts/test
                 scripts/check-cbmc
                 scripts/check-clang-analysis
                 scripts/check-cppcheck
                 scripts/check-frama-c
                 scripts/check-iwyu
                 scripts/check-splint
                 scripts/check-sparse],[
                 chmod u+x "${ac_file}"
])
dnl
AC_OUTPUT
