/* Copyright (C) 2014 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 * @file
 *
 * Linted -- Process Management
 *
 * Process hierarchy
 *
 * - `init`
 *   - `simulator`
 *     - `simulator`
 *   - `drawer`
 *      - `drawer`
 *   - `gui`
 *     - `gui`
 *   - `window`
 *     - `window`
 *   - `monitor`
 *
 * `init` is the top level service that contains everything using
 * `PR_SET_CHILD_SUBREAPER`.  All it does is monitor and restart the
 * `monitor` process and delegate signals to it.
 *
 * Underneath `init` the `monitor` process monitors and restarts
 * service processes and delegates signals.
 *
 * Service processes underneath `init` using `PR_SET_CHILD_SUBREAPER`
 * or `CLONE_NEWPID` contain underneath them similarly named processes
 * that do the actual work of the service.  Service processes don't
 * actually monitor or restart their children.  That job is delegated
 * to `monitor` who ptraces and monitor these service processes (but
 * not the children of the service processes.)
 *
 * @bug Currently not all sandboxed processes are killed on `init`
 * death.  Currently `monitor` will receive a `SIGHUP` because `init`
 * will be the controlling process for the controlling terminal (if
 * there is a terminal) or from a `SIGKILL` from set parent death
 * signal.  Possible solutions:
 * - `monitor` could ptrace `init` and kill its children when `init`
 *   is about to die.
 * - Sandboxes could use `PR_SET_PDEATHSIG` once they have been
 *   reparented to `init`.  However, I am not sure how they would wait
 *   until they have been reparented to `init` before using
 *   `PR_SET_PDEATHSIG`.
 *
 * `monitor` manually kills any children of dying
 * `PR_SET_CHILD_SUBREAPER` type sandboxes and children of dying
 * `CLONE_NEWPID` type sandboxes are killed by the kernel
 * automatically.
 *
 * @section ipc IPC
 *
 * So far we have chosen to use file FIFOs as they can be easily bound
 * in and outside of sandboxes.
 *
 * Note that when there are no more open writers nonblocking fifos
 * read zero bytes but don't return from poll with POLLIN so one has
 * to poll first for reading and then read to avoid falsely returning
 * a read with zero bytes.
 *
 * As well, fifos cannot be opened with write only permission
 * nonblockingly (they give ENXIO errors) when there are not open
 * readers.
 *
 * @subsection requirements Requirements
 * - Presents a security boundary.
 * - Allows access control between sandboxes.
 * - Does not allow a reader to interfere with the writer.
 * - Does not allow a writer to interfere with the reader.
 *
 * @subsection previous Previous Work
 * - The Xen hypervisor's shared memory I/O.
 *
 * @subsection rejected Rejected Approaches
 * - We cannot use shared memory because file sealing only works on
 *   `shmfs` (files created using `memfd_create`) and those files
 *   cannot be mounted.
 * - UNIX domain sockets are really messy.
 *
 * @subsection overview Overview
 *
 * @subsubsection files Files
 * File types:
 * - Eventfds
 * - Sockets
 *    - UNIX Domain Sockets
 *    - Netlink Sockets
 *    - Network Sockets
 * - Regular files
 * - Directory files
 * - Symlink
 * - FIFOs
 * - Unix domain sockets
 * - POSIX Message Queues
 * - Character devices
 *   - Pseudo-terminal handles
 * - Block devices
 * - Custom FUSE files
 *
 * Filesystem visible files:
 * - Regular files
 * - Directory files
 * - Symlink
 * - FIFOs
 * - Unix domain sockets
 * - POSIX Message Queues
 * - Character devices
 *   - Pseudo-terminal handles
 * - Block devices
 * - Custom FUSE files
 *
 * Inotify mechanisms
 *
 * @subsubsection dbus DBus
 * @subsubsection signals Signals
 * - Regular signals
 * - Realtime signals
 * @subsubsection sharedmemory Shared Memory
 * Futex locks + `mmap`.
 * @subsubsection sysvipc System V IPC
 * System V IPC types:
 * - message queues
 * - semaphore sets
 * - shared  memory  segments
 *
 * @section sandboxing Sandboxing
 *
 * I cannot use AppArmor as it requires root privileges in order to
 * load a profile into the kernel in the first place.
 *
 * Currently the following are used:
 * - Seccomp
 * - Process namespaces
 *
 * We could use:
 * - setxattr or setfacl to set values under the "security" and
 *   "security.posix_acl_access" namespace.  It's only attributes
 *   other than security ones that don't work.  However, this seems
 *   like it requires multi-UID user accounts to work.
 *
 * Approaches that won't work:
 * - We cannot use FS_IOC_SETFLAGS to set flags such as the append
 *   only and immutable flags because `tmpfs` does not support such
 *   flags.  Also even in sandbox land we'll probably lack the
 *   appropriate permissions.
 *
 * See also:
 * - http://www.chromium.org/developers/design-documents/sandbox
 * - http://www.cl.cam.ac.uk/research/security/capsicum/
 *
 * @section signals Signal handling
 *
 * Currently `init` forwards all nonurgent exit signals (`SIGHUP`,
 * `SIGINT1`, `SIGQUIT`, and `SIGTERM`) to the `monitor` to handle
 * them.
 */
