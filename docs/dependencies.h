/* Copyright (C) 2014 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- Dependencies
 *
 * @section runtime Runtime Dependencies
 *
 * - X client libraries (Xlib, XCB, GLX)
 *   - Main Site - http://www.x.org/wiki/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=xorg
 * - X11 Server
 *   - Main Site - http://www.x.org/wiki/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=xorg
 * - Mesa
 *   - Main Site - http://www.mesa3d.org/
 *   - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=Mesa
 *   - Dependencies
 *     - DRI
 *       - Main Site - http://dri.freedesktop.org/wiki/
 *       - Bug Database - https://bugs.freedesktop.org/describecomponents.cgi?product=DRI
 * - The GNU C Library
 *   - Main Site - https://www.gnu.org/software/libc/
 *   - Bug Database - https://sourceware.org/bugzilla/describecomponents.cgi?product=glibc
 * - Linux Kernel
 *   - Main Site - https://www.kernel.org/
 *   - Bug Database - https://bugzilla.kernel.org/
 * - Various User Hardware
 *
 * @section sharedobjects Shared Objects
 *
 * - libc6
 *   - /lib/x86_64-linux-gnu/ld-2.19.so
 *   - /lib/x86_64-linux-gnu/libc-2.19.so
 *   - /lib/x86_64-linux-gnu/libdl-2.19.so
 *   - /lib/x86_64-linux-gnu/libm-2.19.so
 *   - /lib/x86_64-linux-gnu/libpthread-2.19.so
 *   - /lib/x86_64-linux-gnu/librt-2.19.so
 * - libcap2
 *   - /lib/x86_64-linux-gnu/libcap.so.2.24
 * - libdrm2
 *   - /usr/lib/x86_64-linux-gnu/libdrm.so.2.4.0
 * - libdrm-intel1
 *   - /usr/lib/x86_64-linux-gnu/libdrm_intel.so.1.0.0
 * - libdrm-nouveau2
 *   - /usr/lib/x86_64-linux-gnu/libdrm_nouveau.so.2.0.0
 * - libdrm-radeon1
 *   - /usr/lib/x86_64-linux-gnu/libdrm_radeon.so.1.0.1
 * - libegl1-mesa-drivers
 *   - /usr/lib/x86_64-linux-gnu/egl/egl_gallium.so
 * - libegl1-mesa
 *   - /usr/lib/x86_64-linux-gnu/mesa-egl/libEGL.so.1.0.0
 * - libelf1
 *   - /usr/lib/x86_64-linux-gnu/libelf-0.158.so
 * - libexpat1
 *   - /lib/x86_64-linux-gnu/libexpat.so.1.6.0
 * - libffi6
 *   - /usr/lib/x86_64-linux-gnu/libffi.so.6.0.1
 * - libgbm1
 *   - /usr/lib/x86_64-linux-gnu/libgbm.so.1.0.0
 * - libgcc1
 *   - /lib/x86_64-linux-gnu/libgcc_s.so.1
 * - libgl1-mesa-dri
 *   - /usr/lib/x86_64-linux-gnu/dri/i965_dri.so
 *   - /usr/lib/x86_64-linux-gnu/libgallium.so.0.0.0
 * - libglapi-mesa
 *   - /usr/lib/x86_64-linux-gnu/libglapi.so.0.0.0
 * - libgles2-mesa
 *   - /usr/lib/x86_64-linux-gnu/mesa-egl/libGLESv2.so.2.0.0
 * - libllvm3.4
 *   - /usr/lib/x86_64-linux-gnu/libLLVM-3.4.so.1
 * - libopenvg1-mesa
 *   - /usr/lib/x86_64-linux-gnu/mesa-egl/libOpenVG.so.1.0.0
 * - libpciaccess0
 *   - /usr/lib/x86_64-linux-gnu/libpciaccess.so.0.11.1
 * - libstdc++6
 *   - /usr/lib/x86_64-linux-gnu/libstdc++.so.6.0.19
 * - libtinfo5
 *   - /lib/x86_64-linux-gnu/libtinfo.so.5.9
 * - libtxc-dxtn-s2tc0
 *   - /usr/lib/x86_64-linux-gnu/libtxc_dxtn_s2tc.so.0.0.0
 * - libwayland-client0
 *   - /usr/lib/x86_64-linux-gnu/libwayland-client.so.0.2.0
 * - libwayland-server0
 *   - /usr/lib/x86_64-linux-gnu/libwayland-server.so.0.1.0
 * - libx11-6
 *   - /usr/lib/x86_64-linux-gnu/libX11.so.6.3.0
 * - libx11-xcb1
 *   - /usr/lib/x86_64-linux-gnu/libX11-xcb.so.1.0.0
 * - libxau6
 *   - /usr/lib/x86_64-linux-gnu/libXau.so.6.0.0
 * - libxcb1
 *   - /usr/lib/x86_64-linux-gnu/libxcb.so.1.1.0
 * - libxcb-dri2-0
 *   - /usr/lib/x86_64-linux-gnu/libxcb-dri2.so.0.0.0
 * - libxcb-xfixes0
 *   - /usr/lib/x86_64-linux-gnu/libxcb-xfixes.so.0.0.0
 * - libxdmcp6
 *   - /usr/lib/x86_64-linux-gnu/libXdmcp.so.6.0.0
 * - libxext6
 *   - /usr/lib/x86_64-linux-gnu/libXext.so.6.4.0
 * - libxfixes3
 *   - /usr/lib/x86_64-linux-gnu/libXfixes.so.3.1.0
 * - zlib1g
 *   - /lib/x86_64-linux-gnu/libz.so.1.2.8
 *
 * @section buildtime Build Time Dependencies
 *
 * - The GNU Compiler Collection
 *   - Main Site - https://www.gnu.org/software/gcc/
 *   - Bug Mailing List - http://gcc.gnu.org/ml/gcc-bugs/
 *   - Bug Database - http://gcc.gnu.org/bugzilla/
 * - The GNU Binary Utilities
 *   - Main Site - https://www.gnu.org/software/binutils/
 *   - Bug Mailing List - <a href="mailto://bug-binutils@gnu.org">&lt;bug-binutils@gnu.org&gt;</a>
 * - Autoconf
 *   - Main Site - https://www.gnu.org/software/autoconf
 *   - Bug Mailing List - <a href="mailto://bug-autoconf@gnu.org">&lt;bug-autoconf@gnu.org&gt;</a>
 * - Automake
 *   - Main Site - https://www.gnu.org/software/automake
 *   - Bug Mailing List - <a href="mailto://bug-automake@gnu.org">&lt;bug-automake@gnu.org&gt;</a>
 * - GNU Make
 *   - Main Site - https://www.gnu.org/software/make
 *   - Bug Mailing List - <a href="mailto://bug-make@gnu.org">&lt;bug-make@gnu.org&gt;</a>
 */
