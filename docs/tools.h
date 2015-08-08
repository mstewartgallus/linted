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
 * Linted -- Tools
 *
 * @section potential Potential tools
 * <ul>
 *
 * <li> [Ada SPARK](http://libre.adacore.com/libre/tools/spark-gpl-edition/) </li>
 * <li> [AFL](http://lcamtuf.coredump.cx/afl/) </li>
 * <li> [BOON "Buffer Overrun detectiON"](http://www.cs.berkeley.edu/~daw/boon/) </li>
 * <li> [Cqual](http://www.cs.umd.edu/~jfoster/cqual/) </li>
 * <li> [Eclipse's CODAN](http://wiki.eclipse.org/CDT/designs/StaticAnalysis) </li>
 * <li> [ImProve](https://github.com/tomahawkins/improve/wiki/ImProve) </li>
 * <li> [Infer](http://fbinfer.com/) </li>
 * <li> [joern](https://github.com/fabsx00/joern) </li>
 * <li> [KLEE](https://klee.github.io/) </li>
 * <li> [MOPS](http://web.cs.ucdavis.edu/~hchen/mops/) </li>
 * <li> [Oink](http://daniel-wilkerson.appspot.com/oink/index.html) </li>
 * <li> [Overture Tool](http://overturetool.org/) </li>
 * <li> [RTL-check](http://rtlcheck.sourceforge.net/) </li>
 * <li> [Saturn](http://saturn.stanford.edu/) </li>
 * <li> [sixgill](http://sixgill.org/) </li>
 * <li> [S-Spider](https://code.google.com/p/s-spider/) </li>
 * <li> [Stanse](http://stanse.fi.muni.cz/) </li>
 * <li> [TLA+](http://research.microsoft.com/en-us/um/people/lamport/tla/tla.html) </li>
 * <li> [unifdef](http://dotat.at/prog/unifdef/) </li>
 * <li> [Why3](http://why3.lri.fr/) </li>
 * <li> [Vera++](https://bitbucket.org/verateam/vera/wiki/Home) </li>
 * <li> [Yasca](http://sourceforge.net/projects/yasca/) </li>
 *
 * </ul>
 *
 * @section useful Useful tools
 * <ul>
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
 *
 * Note that you can sign commits with the `user.signingkey` and
 * `commit.gpgsign` options.
 *
 * </li>
 *
 * </ul>
 *
 * @subsection instrumenters Instrumenters
 *
 * <ul>
 *
 * <li> GLibc Memory Allocator Debug Environment Variables
 * They are `MALLOC_CHECK_` and `MALLOC_PERTURB_`.
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
 * - Probably best used by setting the ExecStart in the drawer service
 *   like:
 *   <code>
 *   ExecStart=/usr/bin/env
 *   ExecStart=LD_PRELOAD=/home/sstewartgallus/root/usr/local/stow/apitrace/lib/x86_64-linux-gnu/apitrace/wrappers/egltrace.so
 *   ExecStart="${LINTED_DRAWER}" "window" "window-notifier-drawer" "updater"
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
 * </ul>
 *
 * @subsection analyzers Analyzers
 *
 * <ul>
 *
 * <li> [Cppcheck](http://cppcheck.sourceforge.net/)
 * A code linter.
 * - It's okay but mostly just catches bad style.
 * </li>
 *
 * <li> [IWYU](https://code.google.com/p/include-what-you-use/)
 * Is a bit buggy.
 * Use like: `make CC='iwyu -I/usr/lib/clang/3.6/include' -k | tee iwyu.log`
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
 * <li> [CPAchecker](http://cpachecker.sosy-lab.org/)
 * As of version 1.4:
 *
 * You can't pass flags into the builtin preprocessor such as include
 * flags.
 * </li>
 *
 * <li> [Sparse](http://git.kernel.org/cgit/devel/sparse/sparse.git)
 * Works moderately well but isn't particularly better than other
 * static analyzers without custom annotations.
 *
 * Doesn't allow specifying system base dirs other than GCC's and
 * doesn't properly recognize the `-isystem` flag.
 *
 * Segfaults on some code.
 * </li>
 *
 * <li> [Frama-C](http://frama-c.com/)
 * As of version Neon-20140301:
 * - It's a bit fiddly to get working.
 * - It doesn't work the standard library and supplies its own but it
 *   only has a very limited standard library.
 * - It can't handle concurrency or function pointers in some cases.
 * </li>
 *
 * </ul>
 *
 * @section rejected Rejected Tools
 *
 * <ul>
 *
 * <li> [VOGL](https://github.com/ValveSoftware/vogl)
 * - It doesn't support EGL.  See the issue at
 *   https://github.com/ValveSoftware/vogl/issues/193
 * </li>
 *
 * <li> [GLSL-Debugger](https://glsl-debugger.github.io/)
 * - I couldn't get it to build as it required Qt4.
 * </li>
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
 *   needed to kill worker threads that are taking too long.  Luckily,
 *   we don't use thread cancellation anymore.
 * - Also it doesn't work with C well. See the bug at
 *   http://llvm.org/bugs/show_bug.cgi?id=20403.
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
 * - Only seems to be able parse traditional style C code which
 *   declares all variables up front.
 * </li>
 *
 * <li> Flawfinder or RATS
 * - Seems to be defunct right now.
 * - Gives many false positives.
 * - Did give a nice tip about how `InitializeCriticalSection` can
 *   throw an exception however. Note that this only applies to
 *   Windows XP and lower.
 * </li>
 *
 * @section also See Also
 * - http://gulliver.eu.org/program_dev_check_environments
 * - https://www.gnu.org/software/hurd/open_issues/code_analysis.html
 * - NIST's Software Assurance Metrics and Tool Evaluation (SAMATE)
     project.
 * - The Open Source Quality Project at Berkeley.
 */
