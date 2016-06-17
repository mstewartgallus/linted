/* Copyright (C) 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- TODO
 *
 * @todo Add platform specific information and defines to static
 *       analysis tooling.
 *
 * @todo Eventually reduce `-Wstack-usage` to 500.
 *
 * @todo Port to --host=x86_64-w64-mingw32
 *
 * @todo Make the process monitor distinguish between transient faults
 *       and persistent faults.
 *
 * @todo Use length annotated strings instead of C strings.
 *
 * @todo Create a notification method.
 *       Currently, outside administrators can create requests to the
 *       monitor system but there needs to be a way for the monitor to
 *       generate notifications. User Mode Linux has the same problem
 *       and we should learn from how they solve it.
 *
 * @todo Harden bind mounts with extra flags:
 *       Possible mount flags:
 *       - `MS_RDONLY`
 *       - `MS_NOSUID`
 *       - `MS_NODEV`
 *       - `MS_NOEXEC`
 *       - `MS_SYNCHRONOUS`
 *       - `MS_REMOUNT`
 *       - `MS_MANDLOCK`
 *       - `MS_DIRSYNC`
 *       - `MS_NOATIME`
 *       - `MS_NODIRATIME`
 *       - `MS_BIND`
 *       - `MS_MOVE`
 *       - `MS_REC`
 *       - `MS_SILENT`
 *       - `MS_POSIXACL`
 *       - `MS_UNBINDABLE`
 *       - `MS_PRIVATE`
 *       - `MS_SLAVE`
 *       - `MS_SHARED`
 *       - `MS_RELATIME`
 *       - `MS_KERNMOUNT`
 *       - `MS_I_VERSION`
 *       - `MS_STRICTATIME`
 *       - `MS_ACTIVE`
 *       - `MS_NOUSER`
 *       Mount flags that work on remounts (binds only allow you to
 *       remount flags):
 *       - `MS_RDONLY`
 *       - `MS_SYNCHRONOUS`
 *       - `MS_MANDLOCK`
 *       - `MS_I_VERSION`
 *
 * @todo Check child processes for hangups with a watchdog timer
 *
 * @todo Create a feed of possible vulnerabilities in all dependencies
 *
 * @todo Achieve deterministic builds.
 *       The Dwarf Directory Table can be sort of solved with the option
 *       `-fdebug-prefix-map` but that still leaves the problem of
 *       system include files.
 */
