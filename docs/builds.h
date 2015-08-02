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
 * Linted -- Builds
 *
 * A list of types of final build products.
 *
 * @section supported-builds Supported Builds
 *
 * @subsection x86_64-linux-gnu x86_64 Linux GNU with GCC and GNU ld (GNU Binutils)
 *
 * This is the main build.  Other builds may work but are only worked
 * on for the purpose of making the code more modular and better.
 *
 * @subsubsection buildtime Build Time Dependencies
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
 *
 * @section unsupported Unsupported Builds
 *
 * This is not a supported final build product.  This is only worked
 * on for a challenge and for making the code more modular and better.
 * In the future it may be supported.
 *
 * - `aarch64-linux-gnu`
 * - `arm-linux-androideabi`
 * - `arm-linux-gnueabi`
 * - `i686-linux-gnu`
 * - `i686-w64-mingw32`
 * - `x86_64-linux-dietlibc`
 * - `x86_64-linux-gnu`
 * - `x86_64-linux-musl`
 * - `x86_64-w64-mingw32`
 *
 * @subsection x86_64-w64-mingw32 x86_64 MinGW-w64
 *
 * This build is temporarily out of service until I obtain a newer
 * toolchain.
 *
 * @subsection asmjs-unknown-emscripten Emscripten
 *
 * This build is temporarily out of service until I figure out why
 * Emscripten generates broken code (it seems to generate null
 * pointers out of nowhere) or the Emscripten compiler improves.
 */
