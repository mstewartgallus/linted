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
 * <ul>
 *
 * <li> iwyu </li>
 * <li> [joern](https://github.com/fabsx00/joern) </li>
 * <li> [Cqual](http://www.cs.umd.edu/~jfoster/cqual/) </li>
 * <li> [MOPS](http://web.cs.ucdavis.edu/~hchen/mops/) </li>
 * <li> [BLAST](http://mtc.epfl.ch/software-tools/blast/index-epfl.php) </li>
 * <li> [CPAchecker](http://cpachecker.sosy-lab.org/) </li>
 * <li> [BOON "Buffer Overrun detectiON"](http://www.cs.berkeley.edu/~daw/boon/) </li>
 * <li> [Stanse](http://stanse.fi.muni.cz/) </li>
 * <li> [Sparse](http://git.kernel.org/cgit/devel/sparse/sparse.git) </li>
 * <li> [Oink](http://daniel-wilkerson.appspot.com/oink/index.html) </li>
 * <li> [Yasca](http://sourceforge.net/projects/yasca/) </li>
 * <li> [RTL-check](http://rtlcheck.sourceforge.net/) </li>
 * <li> [AFL](http://lcamtuf.coredump.cx/afl/) </li>
 * <li> ggcc </li>
 *
 * </li>
 *
 * @section useful Useful tools
 * <ul>
 *
 * <li> [Valgrind](http://valgrind.org/)
 *
 * A runtime instrumentor that checks code for errors.
 *
 * - You have to comment out and disable the seccomp sandboxing.
 * - You have to make sure every running service has a /tmp directory.
 * - You have to set the pipe prefix appropriately
 * - You have to set the Valgrind to trace children
 * - You have to use the latest version from SVN (version 11) to
 *   handle system calls like pivot_root.
 * - It's a bit buggy with sandboxing but works fine.
 * - memcheck works fine.
 * - helgrind can't cope with asynchronous cancellation well.
 * - All together something like `valgrind --vgdb-prefix=/tmp/vgdb --trace-children=yes ./scripts/test` works.
 * </li>
 *
 * <li> [Address Sanitizer](http://clang.llvm.org/docs/AddressSanitizer.html)
 *
 * A compiler debug mode that checks code for errors.
 *
 * - It works great without any changes.
 * </li>
 *
 * <li> [Apitrace](https://github.com/apitrace/apitrace)
 *
 * Traces, error checks, replays and profiles OpenGL code.
 *
 * - It works great but before using one needs to set the drawer
 *   process to not use a custom mount space and to change into an
 *   owned directory.
 *
 * - Can't profile OpenGL ES code well.
 *
 * - When setting environment variables is better supported using them
 *   directly instead of going through the apitrace binary is probably
 *   better because then it can be used only on the drawer service.
 *   <code>
 *   LD_PRELOAD=/home/sstewartgallus/root/usr/local/stow/apitrace/bin/../lib/apitrace/wrappers/egltrace.so
 *   TRACE_FILE=mytrace.trace
 *   </code>
 *
 * </li>
 *
 * <li> [Linux Perf Utils](https://git.kernel.org/cgit/linux/kernel/git/namhyung/linux-perf.git/)
 *
 * Profiles code performance using CPU counters.
 *
 * - It works great without any changes and handles multiple processes
 *   well.
 * - It does not handle a few internal functions in some libraries,
 *   probably due to:
 *   https://bugzilla.kernel.org/show_bug.cgi?id=80671
 * - In order for perf to handle Mesa's JITted GPU code (from llvmpipe
 *   enabled with LIBGL_ALWAYS_SOFTWARE=true) perf needs Mesa to be
 *   built with PROFILE defined and Mesa needs to output a file
 *   "/tmp/perf-[pid]" mapping symbols to JITted code. This means that
 *   /tmp needs to be available inside the drawer process sandbox and
 *   the drawer can't be trapped in a new process hierarchy with
 *   CLONE_NEWPID. Also Mesa tests the PERF_BUILDID_DIR environment
 *   variable set by perf before writing the file.
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
 * <li> [Cppcheck](http://cppcheck.sourceforge.net/)
 * A code linter.
 * - It's okay but mostly just catches bad style.
 * </li>
 *
 * <li> [CBMC](http://www.cprover.org/cbmc/)
 * A code simulator that checks for errors.
 * - It's a bit fiddly to setup but works okay.
 * - Theoretically it should be able to handle nondeterminism and
 *   concurrency but only if I add a bunch of annotations manually.
 * </li>
 *
 * <li> [Frama-C](http://frama-c.com/)
 * - It's a bit fiddly to get working.
 * - It can't handle concurrency or function pointers in some cases.
 * </li>
 *
 * <li> [xtrace](http://xtrace.alioth.debian.org//)
 * - Works perfectly for what it does.
 * - Is kind of useless.
 * - Doesn't support a -D detach option like strace.
 * - Doesn't distinguish between multiple open connections well.
 * - It'd be awesome if it supported features like fault injection as
 *   well ("Maximum number of clients reached" for example.)
 * </li>
 *
 * <li> [FlameGraph](https://github.com/brendangregg/FlameGraph)
 * - Visualizes perf output nicely.
 * </li>
 *
 * <li> [Clang-Format](http://clang.llvm.org/)
 * - Formats code nicely
 * </li>
 *
 * <li> [Git](http://git-scm.com//)
 * - A version control system
 * </li>
 *
 * @section rejected Rejected Tools
 *
 * <ul>
 *
 * <li> [Build EAR](https://github.com/rizsotto/Bear)
 * - It works perfectly except each time it is rerun it overwrites the
 *   previous compile_commands.json and doesn't take into account the
 *   old commands.
 * </li>
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
 * <li> [Smatch](http://smatch.sourceforge.net/)
 * - Does not handle shared object linking (which makes sense as it
 *   was developed for the Linux kernel primarily.)
 * - Doesn't integrate with the hosts toolchain so it doesn't have
 *   important headers such as stddef.h.
 * - Lots of false positives, the tool seems to misunderstand a lot of
 *   standard C that the Linux kernel doesn't use.
 * </li>
 *
 * <li> [SPLINT](http://www.splint.org/)
 * - Does not know about standard defines such as `UINT32_MAX`.
 * - Can't seem to parse reasonable code like:
 * <code> uint_fast16_t low = ((uintmax_t) pos_bytes[0U]) | (((uintmax_t) pos_bytes[1U]) << 8U); </code>
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
 * @section also See Also
 * - https://www.gnu.org/software/hurd/open_issues/code_analysis.html
 * - NIST's Software Assurance Metrics and Tool Evaluation (SAMATE) project.
 * - The Open Source Quality Project at Berkeley.
 */
