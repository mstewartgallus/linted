/* Copyright (C) 2013, 2014 Steven Stewart-Gallus
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
 * @bug If window is moved partially offscreen and resized it fails to
 *      draw to a part of the window opposite from the part offscreen
 *      (not my bug works with glxgears too.)
 *
 * @bug Intermittently restarting a process fails and a new process is
 *      not spawned successfully and ptrace(PTRACE_SEIZE); fails with
 *      EPERM.
 *
 * @todo Add platform specific information and defines to static
 *       analysis tooling.
 *
 * @todo Eventually reduce -Wstack-usage to 500.
 *
 * @todo Port to --host=i685-linux-gnu
 * - This works well except that I can't test the drawer because EGL
 *   and GLES don't work well with multilib.
 *
 * @todo Port to --host=x86_64-w64-mingw32
 *
 * @todo Use length annotated strings instead of C strings.
 *
 * @todo Create a notification method.
 *       Currently, outside administrator can create requests to the
 *       monitor system but there needs to be a way for the monitor to
 *       generate notifications. User Mode Linux has the same problem
 *       and we should learn from how they solve it.
 *
 * @todo Stop graphical banding problems
 *
 * @todo Preserve socket files between restarts
 *
 * @todo Check child processes for hangups
 *
 * @todo Use the monitor to report crashes such as from seccomp rule
 *       violations.
 *
 * @todo Create a feed of possible vulnerabilities in all dependencies
 */
