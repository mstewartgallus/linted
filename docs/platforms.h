/* Copyright (C) 2014, 2015 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- Platforms
 *
 * A list of platforms that final build products may or may not run
 * on. This is different from build types because some platforms can
 * run or emulate each others' build products.
 *
 * @section x86_64-linux-gnu x86_64 GNU/Linux
 *
 * This is the only supported platform.  Other platforms may work but
 * are only worked on for the purpose of making the code more modular
 * and better.
 *
 * @subsection runtime Runtime Dependencies
 *
 * - libcap2
 *   - Main Site - https://sites.google.com/site/fullycapable/
 *   - Binaries
 *     - /lib/x86_64-linux-gnu/libcap.so.2.24
 * - elfutils
 *   - Main Site - https://fedorahosted.org/elfutils/
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libelf-0.158.so
 * - Linux Kernel
 *   - Main Site - https://www.kernel.org/
 *   - Bug Database - https://bugzilla.kernel.org/
 * - Xorg X11 Server
 *   - Main Site - http://www.x.org/wiki/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=xorg
 * - Xorg X11 client libraries (Xlib, XCB, GLX)
 *   - Main Site - http://www.x.org/wiki/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=xorg
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libX11.so.6.3.0
 *     - /usr/lib/x86_64-linux-gnu/libX11-xcb.so.1.0.0
 *     - /usr/lib/x86_64-linux-gnu/libXau.so.6.0.0
 *     - /usr/lib/x86_64-linux-gnu/libxcb.so.1.1.0
 *     - /usr/lib/x86_64-linux-gnu/libxcb-dri2.so.0.0.0
 *     - /usr/lib/x86_64-linux-gnu/libxcb-xfixes.so.0.0.0
 *     - /usr/lib/x86_64-linux-gnu/libXdmcp.so.6.0.0
 *     - /usr/lib/x86_64-linux-gnu/libXext.so.6.4.0
 *     - /usr/lib/x86_64-linux-gnu/libXfixes.so.3.1.0
 *     - /usr/lib/x86_64-linux-gnu/libpciaccess.so.0.11.1
 * - Wayland
 *   - Main Site - http://wayland.freedesktop.org/
 *   - But Database - https://bugs.freedesktop.org/enter_bug.cgi?product=Wayland
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libwayland-client.so.0.2.0
 *     - /usr/lib/x86_64-linux-gnu/libwayland-server.so.0.1.0
 * - DRI
 *   - Main Site - http://dri.freedesktop.org/wiki/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=DRI
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/dri/i965_dri.so
 *     - /usr/lib/x86_64-linux-gnu/libgallium.so.0.0.0
 *     - /usr/lib/x86_64-linux-gnu/libdrm.so.2.4.0
 *     - /usr/lib/x86_64-linux-gnu/libdrm_intel.so.1.0.0
 *     - /usr/lib/x86_64-linux-gnu/libdrm_nouveau.so.2.0.0
 *     - /usr/lib/x86_64-linux-gnu/libdrm_radeon.so.1.0.1
 * - Mesa
 *   - Main Site - http://www.mesa3d.org/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=Mesa
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libgbm.so.1.0.0
 *     - /usr/lib/x86_64-linux-gnu/egl/egl_gallium.so
 *     - /usr/lib/x86_64-linux-gnu/mesa-egl/libEGL.so.1.0.0
 *     - /usr/lib/x86_64-linux-gnu/libglapi.so.0.0.0
 *     - /usr/lib/x86_64-linux-gnu/mesa-egl/libGLESv2.so.2.0.0
 *     - /usr/lib/x86_64-linux-gnu/mesa-egl/libOpenVG.so.1.0.0
 * - s2tc
 *   - Main Site - https://github.com/divVerent/s2tc
 *   - Bug Database - https://github.com/divVerent/s2tc/issues
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libtxc_dxtn_s2tc.so.0.0.0
 * - zlib
 *   - Main Site - http://zlib.net/
 *   - Binaries
 *     - /lib/x86_64-linux-gnu/libz.so.1.2.8
 * - libffi
 *   - Main Site - https://sourceware.org/libffi/
 *   - Bug Database - https://github.com/atgreen/libffi/issues
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libffi.so.6.0.1
 * - LLVM
 *   - Main Site - http://llvm.org/
 *   - Bug Database - http://llvm.org/bugs/
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libLLVM-3.4.so.1
 * - The Expat XML Parser
 *   - Main Site - http://expat.sourceforge.net/
 *   - Bug Database - http://sourceforge.net/p/expat/bugs/
 *   - Binaries
 *     - /lib/x86_64-linux-gnu/libexpat.so.1.6.0
 * - shared low-level terminfo library for terminal handling
 *   - Main Site - http://invisible-island.net/ncurses/
 *   - Binaries
 *     - /lib/x86_64-linux-gnu/libtinfo.so.5.9
 * - GCC Support Library
 *   - Main Site - https://gcc.gnu.org/libstdc++/
 *   - Bug Mailing List - http://gcc.gnu.org/ml/gcc-bugs/
 *   - Bug Database - http://gcc.gnu.org/bugzilla/
 *   - Binaries
 *     - /lib/x86_64-linux-gnu/libgcc_s.so.1
 * - The GNU Standard C++ Library
 *   - Main Site - https://gcc.gnu.org/libstdc++/
 *   - Bug Mailing List - http://gcc.gnu.org/ml/gcc-bugs/
 *   - Bug Database - http://gcc.gnu.org/bugzilla/
 *   - Binaries
 *     - /usr/lib/x86_64-linux-gnu/libstdc++.so.6.0.19
 * - The GNU C Library
 *   - Main Site - https://www.gnu.org/software/libc/
 *   - Bug Database - https://sourceware.org/bugzilla/describecomponents.cgi?product=glibc
 *   - Binaries
 *     - /lib/x86_64-linux-gnu/ld-2.19.so
 *     - /lib/x86_64-linux-gnu/libc-2.19.so
 *     - /lib/x86_64-linux-gnu/libdl-2.19.so
 *     - /lib/x86_64-linux-gnu/libm-2.19.so
 *     - /lib/x86_64-linux-gnu/libpthread-2.19.so
 *     - /lib/x86_64-linux-gnu/librt-2.19.so
 * - Unknown user machine that implements the amd64 hardware architecture
 *
 * @section x86_64-linux-gnu-wine x86_64 GNU/Linux Wine
 *
 * This is not a supported platform.  It may work but is only worked
 * on for a challenge and for making the code more modular and better.
 * In the future it may be supported.  This is more likely to be
 * supported in the future than x86_64 Microsoft Windows because I
 * have easier access to it.
 *
 * @section x86_64-windows x86_64 Microsoft Windows Vista
 *
 * This is not a supported platform.  It may work but is only worked
 * on for a challenge and for making the code more modular and better.
 * In the future it may be supported.
 */
