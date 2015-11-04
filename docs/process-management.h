/* Copyright (C) 2014, 2015 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.
 */

/**
@file

Linted -- Process Management

@section lifecycle Life-Cycle

- `linted` -> `init`
- `monitor`
- `startup`
- services

First, the `linted` process starts up.  It sets a few environment
variables or possibly displays some sort of help.

Next, the `linted` binary executes and becomes the `init` process.
Using some of the set environment variables it forks off and spawns the
`monitor` process.

The `monitor` process then sets up a little bit of state such as
important IPC channels and then spawns the `startup` process.  After
spawning the `startup` process the `monitor` process then idly waits for
commands.

Meanwhile, the `startup` process reads through the unit files in the
directories listed in `LINTED_UNIT_PATH` and registers those units with
the `monitor`.  Once the `monitor` receives units to regiister it then
spawns associated processes or creates associated files.  In the future,
I would like to have the `monitor` automatically deduce unit file
dependencies and only spawn processes once their dependencies are met.

@section hierarchy Process hierarchy

- `init`
  - `audio`
    - `audio`
  - `simulator`
    - `simulator`
  - `drawer`
    - `drawer`
  - `gui`
    - `gui`
  - `window`
    - `window`
  - `monitor`

`init` is the top level service that contains everything using
`PR_SET_CHILD_SUBREAPER`.  All it does is monitor and restart the
`monitor` process and delegate signals to it.

Underneath `init` the `monitor` process monitors and restarts service
processes and delegates signals.

Service processes underneath `init` using `PR_SET_CHILD_SUBREAPER` or
`CLONE_NEWPID` contain underneath them similarly named processes that do
the actual work of the service.  Service processes don't actually
monitor or restart their children.  That job is delegated to `monitor`
who ptraces and monitor these service processes (but not the children of
the service processes.)  `monitor` manually kills any children of dying
`PR_SET_CHILD_SUBREAPER` type sandboxes and children of dying
`CLONE_NEWPID` type sandboxes are killed by the kernel automatically.

When `init` dies `monitor` will receive a `SIGHUP` because `init` will
be the controlling process for the controlling terminal (if there is a
terminal) or from a `SIGKILL` from a set parent death signal.

@bug Currently not all sandboxed processes are killed on `init`
     death.   Possible solutions:
     - `monitor` could ptrace `init` and kill its children when `init`
is about to die.
     - Sandboxes could use `PR_SET_PDEATHSIG` once they have been
reparented to `init`.  However, I am not sure how they would wait until
they have been reparented to `init` before using `PR_SET_PDEATHSIG`.

@section ipc IPC

So far we have chosen to use file FIFOs as they can be easily bound in
and outside of sandboxes.

@subsection requirements Requirements
- Presents a security boundary.
- Allows access control between sandboxes.
- Does not allow a reader to interfere with the writer.
- Does not allow a writer to interfere with the reader.

@subsection previous Previous Work
- The Xen hypervisor's shared memory I/O.

@subsection rejected Rejected Approaches
- UNIX domain sockets are really messy.

@subsection ipcsolutions Full Solutions
These are full blown solutions that handle both encoding data and
transporting data.
- D-Bus
- gRPC
- SOAP
- CORBA

@subsection ipcencodings Encodings
Currently we just use very arbitrary hacks for encoding our data.
- XML
- JSON
- Google Protocol Buffers
- Capn Proto
- ASN.1 Compiler

@subsection ipcprimitives Primitives
@subsubsection files Files
File types:
- Eventfds
- Sockets
  - UNIX Domain Sockets
  - Netlink Sockets
  - Network Sockets
- Regular files
- Directory files
- Symlink
- FIFOs

- Unix domain sockets
- POSIX Message Queues
- Character devices
  - Pseudo-terminal handles
  - Block devices
  - Custom FUSE files

Filesystem visible files:
- Regular files
- Directory files
- Symlink
- FIFOs
- Unix domain sockets
- POSIX Message Queues
- Character devices
  - Pseudo-terminal handles
  - Block devices
  - Custom FUSE files

@subsubsection fifos FIFOs

FIFOs give `POLLHUP` to poll and read zero bytes once a process has
written to the pipe once and there are no more open writers. As well,
fifos give `EPIPE` to writers when a process writes to the pipe and
there are no more open readers. Moreover, fifos cannot be opened with
write only permission nonblockingly (they give `ENXIO` errors) when
there are not open readers. For these reasons most of our fifo users
should open these fifos in read and write mode and not write only or
read only mode.

@subsubsection signals Signals
- Regular signals
- Realtime signals
@subsubsection sharedmemory Shared Memory
System calls that allocate memory:
- `mmap`
- `brk`
- Use the `DRM_IOCTL_I915_GEM_MMAP` `ioctl`
- Use the `DRM_IOCTL_RADEON_GEM_MMAP` `ioctl`
- `shmat`
- `prctl` (using `PR_SET_MM_BRK`)

Shared memory is unsuitable because:

- `mmap`able files can be `ftruncate`d by attackers
- We can only share memory mappings without files using a weird trick
  using `fork`.
- File sealing only works on `shmfs` (files created using
  `memfd_create`) and those files cannot be mounted.
- System V IPC sort of works but is awkward.  Sandboxed processes
  would have to share the same IPC namespace (and so would have
  access every shared memory segment), `shmget` does sets the initial
  size of the memory segment which cannot be changed later and so
  attackers can't create `SIGSEGV`s in users.
- Hilariously, it is possible to use `mmap` with netlink sockets
  (which should only be used for kernel space to user-space
  communication) and so achieve safe `mmap` between processes.

@subsubsection sysvipc System V IPC
System V IPC types:
- message queues
- semaphore sets
- shared memory segments

@section sandboxing Sandboxing

I cannot use AppArmor as it requires root privileges in order to
load a profile into the kernel in the first place.

Currently the following are used:
- Seccomp
- Process namespaces

We could use:
- `setxattr` or `setfacl` to set values under the "security" and
  "security.posix_acl_access" namespace.  It's only attributes other
  than security ones that don't work.  However, this seems like it
  requires multi-UID user accounts to work.

Approaches that won't work:
- We cannot use `FS_IOC_SETFLAGS` to set flags such as the append only
  and immutable flags because `tmpfs` does not support such flags.  Also
  even in sandbox land we'll probably lack the appropriate permissions.

See also:
- http://www.chromium.org/developers/design-documents/sandbox
- http://www.cl.cam.ac.uk/research/security/capsicum/

@section signals Signal handling

Currently `init` forwards all nonurgent exit signals (`SIGHUP`,
`SIGINT1`, `SIGQUIT`, and `SIGTERM`) to the `monitor` to handle them.
*/
