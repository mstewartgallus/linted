/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "config.h"

/**
 * @file
 *
 * Asynchronous IO implementation.

 * @todo Use a portable abstraction over `pthread_setname_np`.
 *
 * @section prior Prior Work
 *
 * @subsection glibc GLibc AIO
 *
 * See https://www.gnu.org/software/libc/
 *
 * GLibc's AIO is implemented in the `sysdeps/pthread/aio_*.c` files
 * in the GLibc repository.
 *
 * GLibc's AIO implementation is implemented in userspace, allocates
 * memory dynamically and is probably no faster than ours.
 *
 * @subsection openjdk OpenJDK's AIO
 *
 * See http://openjdk.java.net
 *
 * OpenJDK's AIO implementation is implemented in the files in
 * `jdk/src/java.base/ * /classes/sun/nio/ch` in the OpenJDK
 * repository.
 *
 * @subsection mono Mono CLR's AIO
 *
 * See https://github.com/mono/mono.git
 *
 * Mono CLR's AIO is implemented `mcs/class/corlib/System.IO`,
 * `mono/io-layer` and `mono/metadata/file-io.c` in the Mono
 * repository.
 *
 * @subsection microsoft-clr Microsoft CLR's AIO
 *
 * See https://github.com/dotnet/coreclr.git
 *
 * Microsoft CLR's AIO is implemented `mscorlib/src/System/IO` in the
 * `dotnet/coreclr` repository.
 *
 * @subsection linux Linux Kernel's AIO
 *
 * See https://www.kernel.org/
 *
 * Linux's AIO is implemented in `fs/aio.c` in the Linux kernel's
 * repository.
 *
 * You may think we could use libaio and `io_setup` can statically
 * allocate a work queue that is `max_tasks` large for us but as of
 * yet there would be no point as we cannot take advantage of the
 * syscalls that libaio provides asynchronous implementations
 * of. libaio provides asynchronous implementations of `pread`,
 * `pwrite`, `fsync`, `fdatasync`, `preadv` and `pwritev`. libaio
 * defines a constant for running `poll` asynchronously that we could
 * use but the command was never implemented.
 *
 * @subsection openbsd OpenBSD AIO
 * @subsection freebsd FreeBSD AIO
 * @subsection netbsd NetBSD AIO
 * @subsection dragonflybsd DragonFly BSD AIO
 */

#if defined HAVE_WINDOWS_API
#include "asynch-windows.c"
#elif defined HAVE_EMSCRIPTEN_API
#include "asynch-emscripten.c"
#elif defined HAVE_POSIX_API
#include "asynch-posix.c"
#else
#error no asynchronous IO implementation for this platform
#endif
