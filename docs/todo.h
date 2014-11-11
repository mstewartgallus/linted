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
 * @todo Add platform specific information and defines to static
 *       analysis tooling.
 *
 * @todo Eventually reduce -Wstack-usage to 500.
 *
 * @todo Investigate security projects
 * - NIST's Software Assurance Metrics and Tool Evaluation (SAMATE) project
 * - The Open Source Quality Project at Berkeley is investigating
 *   tools and techniques for assuring software quality (not just
 *   security) of OSS/FS programs.
 *
 * @todo Port to --host=i685-linux-gnu
 * - This works well except that I can't test the drawer because EGL
 *   and GLES don't work well with multilib.
 *
 * @todo Port to --host=x86_64-w64-mingw32
 *
 * @todo Use length annotated strings instead of C strings.
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
 *
 * @todo For units check the configuration directory (eg /etc) before
 *       checking the default configuration directory (eg
 *       /usr/local/share/factory/linted/etc).
 */
