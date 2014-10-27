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
 * Linted -- Tools
 *
 * @section potential Potential tools
 * - [joern](https://github.com/fabsx00/joern)
 * - [SPLINT](http://www.splint.org/)
 * - [Cqual](http://www.cs.umd.edu/~jfoster/cqual/)
 * - [Smatch](http://smatch.sourceforge.net/)
 * - [MOPS](http://web.cs.ucdavis.edu/~hchen/mops/)
 * - [BLAST](http://mtc.epfl.ch/software-tools/blast/index-epfl.php)
 * - [CPAchecker](http://cpachecker.sosy-lab.org/)
 * - [BOON "Buffer Overrun detectiON"](http://www.cs.berkeley.edu/~daw/boon/)
 * - [Stanse](http://stanse.fi.muni.cz/)
 * - [Sparse](http://git.kernel.org/cgit/devel/sparse/sparse.git)
 * - [Oink](http://daniel-wilkerson.appspot.com/oink/index.html)
 * - [Yasca](http://sourceforge.net/projects/yasca/)
 * - [RTL-check](http://rtlcheck.sourceforge.net/)
 * - ggcc
 *
 * @section useful Useful tools
 * <ul>
 *
 * <li> [Valgrind](http://valgrind.org/)
 * - You have to comment out and disable the seccomp sandboxing.
 * - You have to make sure every running service has a /tmp directory.
 * - You have to use the latest version from SVN (version 11) to
 *   handle system calls like pivot_root.
 * - It's a bit buggy with sandboxing but works fine.
 * - memcheck works fine.
 * - helgrind can't cope with asynchronous cancellation well.
 * </li>
 *
 * <li> [Address Sanitizer](http://clang.llvm.org/docs/AddressSanitizer.html)
 * - It works great without any changes.
 * </li>
 *
 * <li> [Linux Perf Utils](https://git.kernel.org/cgit/linux/kernel/git/namhyung/linux-perf.git/)
 * - It works great without any changes and handles multiple processes
 *   well.
 * - It does not handle a few internal functions in some libraries,
 *   probably due to:
 *   https://bugzilla.kernel.org/show_bug.cgi?id=80671
 * - It cannot handle Mesa's JITted GPU code. Not sure how easy it
 *   would be for Mesa to output debug information.
 * - Make sure to download all debug symbols
 * - Related packages used by Linted on my OS
 *   - libc6
 *     - /lib/x86_64-linux-gnu/ld-2.19.so
 *     - /lib/x86_64-linux-gnu/libc-2.19.so
 *     - /lib/x86_64-linux-gnu/libdl-2.19.so
 *     - /lib/x86_64-linux-gnu/libm-2.19.so
 *     - /lib/x86_64-linux-gnu/libpthread-2.19.so
 *     - /lib/x86_64-linux-gnu/librt-2.19.so
 *   - libcap2
 *     - /lib/x86_64-linux-gnu/libcap.so.2.24
 *   - libdrm2
 *     - /usr/lib/x86_64-linux-gnu/libdrm.so.2.4.0
 *   - libdrm-intel1
 *     - /usr/lib/x86_64-linux-gnu/libdrm_intel.so.1.0.0
 *   - libdrm-nouveau2
 *     - /usr/lib/x86_64-linux-gnu/libdrm_nouveau.so.2.0.0
 *   - libdrm-radeon1
 *     - /usr/lib/x86_64-linux-gnu/libdrm_radeon.so.1.0.1
 *   - libegl1-mesa-drivers
 *     - /usr/lib/x86_64-linux-gnu/egl/egl_gallium.so
 *   - libegl1-mesa
 *     - /usr/lib/x86_64-linux-gnu/mesa-egl/libEGL.so.1.0.0
 *   - libelf1
 *     - /usr/lib/x86_64-linux-gnu/libelf-0.158.so
 *   - libexpat1
 *     - /lib/x86_64-linux-gnu/libexpat.so.1.6.0
 *   - libffi6
 *     - /usr/lib/x86_64-linux-gnu/libffi.so.6.0.1
 *   - libgbm1
 *     - /usr/lib/x86_64-linux-gnu/libgbm.so.1.0.0
 *   - libgcc1
 *     - /lib/x86_64-linux-gnu/libgcc_s.so.1
 *   - libgl1-mesa-dri
 *     - /usr/lib/x86_64-linux-gnu/dri/i965_dri.so
 *     - /usr/lib/x86_64-linux-gnu/libgallium.so.0.0.0
 *   - libglapi-mesa
 *     - /usr/lib/x86_64-linux-gnu/libglapi.so.0.0.0
 *   - libgles2-mesa
 *     - /usr/lib/x86_64-linux-gnu/mesa-egl/libGLESv2.so.2.0.0
 *   - libllvm3.4
 *     - /usr/lib/x86_64-linux-gnu/libLLVM-3.4.so.1
 *   - libopenvg1-mesa
 *     - /usr/lib/x86_64-linux-gnu/mesa-egl/libOpenVG.so.1.0.0
 *   - libpciaccess0
 *     - /usr/lib/x86_64-linux-gnu/libpciaccess.so.0.11.1
 *   - libstdc++6
 *     - /usr/lib/x86_64-linux-gnu/libstdc++.so.6.0.19
 *   - libtinfo5
 *     - /lib/x86_64-linux-gnu/libtinfo.so.5.9
 *   - libtxc-dxtn-s2tc0
 *     - /usr/lib/x86_64-linux-gnu/libtxc_dxtn_s2tc.so.0.0.0
 *   - libwayland-client0
 *     - /usr/lib/x86_64-linux-gnu/libwayland-client.so.0.2.0
 *   - libwayland-server0
 *     - /usr/lib/x86_64-linux-gnu/libwayland-server.so.0.1.0
 *   - libx11-6
 *     - /usr/lib/x86_64-linux-gnu/libX11.so.6.3.0
 *   - libx11-xcb1
 *     - /usr/lib/x86_64-linux-gnu/libX11-xcb.so.1.0.0
 *   - libxau6
 *     - /usr/lib/x86_64-linux-gnu/libXau.so.6.0.0
 *   - libxcb1
 *     - /usr/lib/x86_64-linux-gnu/libxcb.so.1.1.0
 *   - libxcb-dri2-0
 *     - /usr/lib/x86_64-linux-gnu/libxcb-dri2.so.0.0.0
 *   - libxcb-xfixes0
 *     - /usr/lib/x86_64-linux-gnu/libxcb-xfixes.so.0.0.0
 *   - libxdmcp6
 *     - /usr/lib/x86_64-linux-gnu/libXdmcp.so.6.0.0
 *   - libxext6
 *     - /usr/lib/x86_64-linux-gnu/libXext.so.6.4.0
 *   - libxfixes3
 *     - /usr/lib/x86_64-linux-gnu/libXfixes.so.3.1.0
 *   - zlib1g
 *     - /lib/x86_64-linux-gnu/libz.so.1.2.8
 * </li>
 *
 * @section rejected Rejected Tools
 *
 * <ul>
 *
 * <li> [Linux Dwarves tools](https://git.kernel.org/cgit/devel/pahole/pahole.git/)
 * - codiff       Diffs changes in binaries
 * - dtagnames    Lists tag names
 * - pahole       Finds holes in structures
 * - pdwtags      Dwarf information pretty printer
 * - pfunct       Displays information about functions
 * - pglobal      Displays information about global variables
 * - prefcnt      Tries to find unreferenced tags
 *
 * These tools would be extremely useful if they worked on a wider
 * range of types. So far, they all seem to break and not handle any
 * structure types that aren't extremely ordinary.
 *
 * </li>
 *
 * <li> [Clang's -Wthread-safety flag](http://clang.llvm.org/docs/ThreadSafetyAnalysis.html)
 *
 * This tool is okay but cannot handle thread cancellation which is
 * needed to kill worker threads that are taking too long.
 *
 * </li>
 *
 * <li> Flawfinder or RATS
 *
 * - Seems to be defunct right now.
 *
 * - Gives many false positives.
 *
 * - Did give a nice tip about how InitializeCriticalSection can throw
 * an exception however. Note that this only applies to Window XP and
 * lower.
 *
 * </li>
 *
 * </ul>
 */
