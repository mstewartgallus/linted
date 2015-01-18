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
 * <li> [Why3](http://why3.lri.fr/) </li>
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
 * As of version 3.11:
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
 * Traces, error checks, replays and profiles OpenGL code.
 *
 * As of commit a3e4614c7fb0a6ffa8c748bf5d49b34612c9d6d4:
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
 * </li>
 *
 * <li> [Cppcheck](http://cppcheck.sourceforge.net/)
 * A code linter.
 * - It's okay but mostly just catches bad style.
 * </li>
 *
 * <li> [CBMC](http://www.cprover.org/cbmc/)
 * As of version 4.9:
 * A code simulator that checks for errors.
 * - It's a bit fiddly to setup but works okay.
 * - It is particularly annoying that it doesn't know that all indices
 *   into `argv` less than `argc` and greater than or equal to zero
 *   are valid C strings.
 * - Theoretically it should be able to handle nondeterminism and
 *   concurrency but only if I add a bunch of annotations manually.
 * </li>
 *
 * <li> [Frama-C](http://frama-c.com/
 * As of version Neon-20140301:
 * - It's a bit fiddly to get working.
 * - It doesn't work the standard library and supplies its own but it
 *   only has a very limited standard library.
 * - It can't handle concurrency or function pointers in some cases.
 * </li>
 *
 * <li> [xtrace](http://xtrace.alioth.debian.org//)
 * As of version 1.3.1:
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
 * As of version 3.5:
 * - Formats code nicely
 * </li>
 *
 * <li> [Git](http://git-scm.com//)
 * A version control system
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
 *
 * A set of simple binary analysis tools.
 *
 * - codiff       Diffs changes in binaries
 * - dtagnames    Lists tag names
 * - pahole       Finds holes in structures
 * - pdwtags      Dwarf information pretty printer
 * - pfunct       Displays information about functions
 * - pglobal      Displays information about global variables
 * - prefcnt      Tries to find unreferenced tags
 *
 * As of version 1.9:
 *
 * - These tools would be extremely useful if they worked on a wider
 *   range of types. So far, they all seem to break and not handle any
 *   structure types that aren't extremely ordinary.
 *
 * </li>
 *
 * <li> [Clang's -Wthread-safety flag](http://clang.llvm.org/docs/ThreadSafetyAnalysis.html)
 * As of version 3.5:
 * - This tool is okay but cannot handle thread cancellation which is
 *   needed to kill worker threads that are taking too long.
 * </li>
 *
 * <li> [Smatch](http://smatch.sourceforge.net/)
 * As of version 0.4.1:
 * - Does not handle shared object linking (which makes sense as it
 *   was developed for the Linux kernel primarily.)
 * - Doesn't integrate with the hosts toolchain so it doesn't have
 *   important headers such as stddef.h.
 * - Lots of false positives, the tool seems to misunderstand a lot of
 *   standard C that the Linux kernel doesn't use.
 * </li>
 *
 * <li> [SPLINT](http://www.splint.org/)
 * As of version 3.12:
 * - Does not know about standard defines such as `UINT32_MAX`.
 * - Can't seem to parse reasonable code like:
 *   <code> uint_fast16_t low = ((uintmax_t) pos_bytes[0U]) | (((uintmax_t) pos_bytes[1U]) << 8U); </code>
 * </li>
 *
 * <li> Flawfinder or RATS
 * - Seems to be defunct right now.
 * - Gives many false positives.
 * - Did give a nice tip about how `InitializeCriticalSection` can
 *   throw an exception however. Note that this only applies to Window
 *   XP and lower.
 *
 * </li>
 *
 * @section also See Also
 * - https://www.gnu.org/software/hurd/open_issues/code_analysis.html
 * - NIST's Software Assurance Metrics and Tool Evaluation (SAMATE) project.
 * - The Open Source Quality Project at Berkeley.
 */
