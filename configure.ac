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
                  [https://gitlab.com/linted])
dnl
dnl Other software uses a shell command to get the date but that
dnl prevents deterministic builds.
AC_DEFINE([COPYRIGHT_YEAR], ["2015"], [year in copyright message])
AC_DEFINE([PACKAGE_NAME_SPACE], ["com.gitlab.sstewartgallus.linted"],
                               [package hierarchical name space])
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
dnl
AC_CANONICAL_HOST
AS_IF([test "x${cross_compiling}" = "xno"], [
        AC_MSG_WARN([dnl
[Native and cross builds do not result in the exact same binaries.]
[Explicitly set the --host and --build options and do not use native builds.]dnl
])
])
dnl
AC_PROG_CC
dnl
LINTED_FAST_BUILD
[CC="${CC} ${linted_CFLAGS_FAST_BUILD}"]
dnl
linted_target_host_flag="-target $host"
LINTED_CHECK_CFLAG([linted_CFLAGS_HOSTFLAGS], [$linted_target_host_flag])
AS_IF([test "x$linted_CFLAGS_HOSTFLAGS" != "x"], [
   [CC="${CC} ${linted_CFLAGS_HOSTFLAGS}"]
])
dnl
AS_IF([test "x$host_cpu" = xi386 || test "x$host_cpu" = xi686], [
      LINTED_CHECK_CFLAG([linted_CFLAGS_HOSTFLAGS], [-m32])
      AS_IF([test "x$linted_CFLAGS_HOSTFLAGS" != "x"], [
           [CC="${CC} ${linted_CFLAGS_HOSTFLAGS}"]
      ])
])
dnl
AS_IF([test "x$host_cpu" = 'xx86_64'], [
      LINTED_CHECK_CFLAG([linted_CFLAGS_HOSTFLAGS], [-m64])
      AS_IF([test "x$linted_CFLAGS_HOSTFLAGS" != "x"], [
           [CC="${CC} ${linted_CFLAGS_HOSTFLAGS}"]
      ])
])
dnl
AM_PROG_AR
dnl
dnl We only use static convenience libraries anyway
LT_INIT([disable-shared])
dnl
[linted_posix_api_default=no]
[linted_x11_api_default=no]
[linted_windows_api_default=no]
[linted_emscripten_api_default=no]
dnl
AS_IF([test "x${host_os}" = "xlinux-androideabi" ||
       test "x${host_os}" = "xlinux-gnu" ||
       test "x${host_os}" = "xlinux-gnux32" ||
       test "x${host_os}" = "xlinux-gnueabi" ||
       test "x${host_os}" = "xlinux-gnueabihf" ||
       test "x${host_os}" = "xlinux-musl"], [[linted_posix_api_default=yes]])
dnl
AS_IF([test "x${host_os}" = "xmingw32" ||
       test "x${host_os}" = "xmingw32msvc"], [[linted_windows_api_default=yes]])
dnl
AS_IF([test "x${CC}" = "xwinegcc"], [
        [linted_windows_api_default=yes]
        [linted_posix_api_default=no]
])
dnl
AS_IF([test "x${linted_posix_api_default}" = "xyes"], [[linted_x11_api_default=yes]])
dnl
AC_ARG_ENABLE(
        [posix-api],
        AS_HELP_STRING(
                [--enable-posix-api],
                [enable use of the POSIX API]),
        [
         [enable_posix_api=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_posix_api=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_posix_api=no]])
         AS_IF([test "x${enable_posix_api}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --enable-posix-api])])
        ],
        [[enable_posix_api="$linted_posix_api_default"]])
AC_ARG_ENABLE(
        [x11-api],
        AS_HELP_STRING(
                [--enable-x11-api],
                [enable use of the X11 API]),
        [
         [enable_x11_api=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_x11_api=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_x11_api=no]])
         AS_IF([test "x${enable_x11_api}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --enable-x11-api])])
        ],
        [[enable_x11_api="$linted_x11_api_default"]])
AC_ARG_ENABLE(
        [windows-api],
        AS_HELP_STRING(
                [--enable-windows-api],
                [enable use of the Microsoft Windows API]),
        [
         [enable_windows_api=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_windows_api=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_windows_api=no]])
         AS_IF([test "x${enable_windows_api}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --enable-windows-api])])
        ],
                [[enable_windows_api="$linted_windows_api_default"]])
AC_ARG_ENABLE(
        [emscripten-api],
        AS_HELP_STRING(
                [--enable-emscripten-api],
                [enable use of Emscripten API]),
        [
         [enable_emscripten_api=badval]
         AS_IF([test "x${enableval}" = "xyes"], [[enable_emscripten_api=yes]])
         AS_IF([test "x${enableval}" = "xno"], [[enable_emscripten_api=no]])
         AS_IF([test "x${enable_emscripten_api}" = "xbadval"], [
          AC_MSG_ERROR([bad value "${enableval}" for --enable-emscripten-api])])
        ],
                [[enable_emscripten_ap="$linted_emscripten_api_default"]])
dnl
AM_CONDITIONAL([WINDOWS_API], [test "x${enable_windows_api}" = xyes])
AS_IF([test "x${enable_windows_api}" = "xyes"], [
dnl
        AC_DEFINE([HAVE_WINDOWS_API], [1], [Define if the Microsoft Windows API is being used.])
dnl
        LINTED_TOOL_WINDRES
dnl
        dnl Note that we just use the subsystem option and not
        dnl -mwindows because some of are executables aren't graphical
        dnl and don't need to link in all the dialog stuff.
dnl
        LINTED_CHECK_CFLAGS([linted_CFLAGS_TARGET_TYPE], [
                [-mno-cygwin]dnl
        ])
        [CC="${CC} ${linted_CFLAGS_TARGET_TYPE}"]
        LINTED_CHECK_LDFLAGS([linted_LDFLAGS_TARGET_TYPE], [
                [-Wl,--tsaware]dnl
                [-Wl,--subsystem=windows:6.02]dnl
                [-Wl,--major-os-version=6]dnl
                [-Wl,--minor-os-version=0.2]dnl
        ])
        AC_SUBST([linted_LDFLAGS_TARGET_TYPE])

        LINTED_CHECK_CFLAGS([linted_CFLAGS_UNICODE], [
                [-municode]dnl
        ])
        AC_SUBST([linted_CFLAGS_UNICODE])
dnl
        LINTED_CHECK_LIB([CRT_LIBS], [-lmsvcr110], [_wdupenv_s], [#include <stdlib.h>], [], [
                AC_MSG_ERROR([Unable to find a CRT library])
        ])
        LINTED_CHECK_LIB([GDI_LIBS], [-lgdi32], [GetStockObject], [#include <windows.h>], [], [
                AC_MSG_WARN([Unable to find a GDI library])
        ])
        LINTED_CHECK_LIB([WS2_32_LIBS], [-lws2_32], [WSAStartup], [#include <winsock.h>], [], [
                AC_MSG_ERROR([Unable to find a Winsock library])
        ])
        LINTED_CHECK_LIB([D3D11_LIBS], [-ld3dx11], [D3D11CreateDevice], [#include <d3d11.h>], [], [
                AC_MSG_WARN([Unable to find a DirectX 11 library])
        ])
dnl
])
dnl
AM_CONDITIONAL([EMSCRIPTEN_API], [test "x${enable_emscripten_api}" = xyes])
AS_IF([test "x${enable_emscripten_api}" = "xyes"], [
dnl
        AC_DEFINE([HAVE_EMSCRIPTEN_API], [1], [Define if the Emscripten API is being used.])
])
dnl
AM_CONDITIONAL([POSIX_API], [test "x${enable_posix_api}" = xyes])
AS_IF([test "x${enable_posix_api}" = "xyes"], [
dnl
        AC_DEFINE([HAVE_POSIX_API], [1], [Define if the POSIX API is being used.])
dnl
        LINTED_CHECK_CFLAGS([linted_CFLAGS_TARGET_TYPE], [[-pthread]])
        [CC="${CC} ${linted_CFLAGS_TARGET_TYPE}"]
dnl
        AC_CHECK_HEADERS([[arpa/inet.h]dnl
                          [mntent.h]dnl
                          [sys/mount.h]dnl
                          [sys/socket.h]dnl
                          [syslog.h]], [], [
          AC_MSG_ERROR([Unable to find header ${ac_header}])
        ], [])
dnl
        AC_CHECK_HEADERS([[sys/capability.h]], [], [
          AC_MSG_WARN([Unable to find header ${ac_header}])
        ], [])
dnl
        LINTED_CHECK_LIB([MATH_LIBS], [-lm], [sin], [#include <math.h>], [], [
           AC_MSG_ERROR([Unable to find a math library])
        ])
dnl
dnl     First try to see if realtime functionality is included by default
        LINTED_CHECK_LIB([REALTIME_LIBS], [], [clock_gettime], [#define _POSIX_C_SOURCE 199309L
#include <time.h>], [], [
                LINTED_CHECK_LIB([REALTIME_LIBS], [-lrt], [clock_gettime], [#define _POSIX_C_SOURCE 199309L
#include <time.h>], [], [
                        AC_MSG_ERROR([Unable to find a realtime functions library])
                ])
        ])
dnl
        LINTED_CHECK_LIB([CAP_LIBS], [-lcap], [cap_set_proc], [#include <sys/capability.h>], [], [
                AC_MSG_WARN([Unable to find a POSIX capabilities library])
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
        AC_CHECK_HEADERS([[sys/auxv.h]])
        AC_CHECK_FUNCS([[getauxval]])
dnl
        AC_CHECK_FUNCS([[pthread_setname_np]])
dnl
        AC_CHECK_HEADERS([[sys/prctl.h]])
        AC_CHECK_FUNCS([[prctl]])
])
dnl
AM_CONDITIONAL([X11_API], [test "x${enable_x11_api}" = xyes])
AS_IF([test "x${enable_x11_api}" = "xyes"], [
dnl
        AC_DEFINE([HAVE_X11_API], [1], [Define if the X11 API is being used.])
dnl
        PKG_CHECK_MODULES(XCB, xcb, [], [
                AC_MSG_WARN([Unable to find an XCB library])
        ])
        PKG_CHECK_MODULES(XCB_XKB, xcb-xkb, [], [
                AC_MSG_WARN([Unable to find an xcb-xkb library])
        ])
dnl
        PKG_CHECK_MODULES(XKB, xkbcommon, [], [
                AC_MSG_WARN([Unable to find an xkbcommon library])
        ])
        PKG_CHECK_MODULES(XKB_X11, xkbcommon-x11, [], [
                AC_MSG_WARN([Unable to find an xkbcommon-x11 library])
        ])
dnl
        PKG_CHECK_MODULES(EGL, egl, [], [
                AC_MSG_WARN([Unable to find an EGL library])
        ])
        PKG_CHECK_MODULES(GLES, glesv2, [], [
                AC_MSG_WARN([Unable to find an OpenGLES version 2 library])
        ])
dnl
        PKG_CHECK_MODULES(PULSE, libpulse, [], [
                AC_MSG_WARN([Unable to find a PulseAudio library])
        ])
dnl
])
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
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([scripts/test
                 scripts/check-cbmc
                 scripts/check-cpa
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
