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
 * @todo Review and criticize several static analysers.
 * - If they don't work out write detailed reasons why.
 * - The tools:
 *   - SPLINT
 *   - Cqual
 *   - Smatch
 *   - MOPS
 *   - BLAST
 *   - BOON "Buffer Overrun detectiON"
 *   - ggcc
 *   - Stanse
 *   - Sparse
 *   - Oink
 *   - Yasca
 *   - RTL-check
 *
 * @todo Investigate security projects
 * - NIST's Software Assurance Metrics and Tool Evaluation (SAMATE) project
 * - The Open Source Quality Project at Berkeley is investigating
 *   tools and techniques for assuring software quality (not just
 *   security) of OSS/FS programs.
 *
 * @todo Port to --host=x86_64-w64-mingw32
 * - As part of this process environment sanitization code will need
 *   to be reworked. Windows maintains two copies of environment
 *   variables: one copy in process space and the other copy in kernel
 *   space.
 *
 * @todo Use length annotated strings instead of C strings.
 *
 * @todo Stop graphical banding problems
 *
 * @todo Check child processes for hangups
 * - Periodically poll message channels to see if they are writeable
 *   and not full because the reader has hungup. If the reader has
 *   hungup then kill and restart the reader.
 *
 * @todo Rewrite utility shell scripts in Python.
 *
 * @todo Use the monitor to report crashes such as from seccomp rule
 *       violations.
 *
 * @todo Create a feed of possible vulnerabilities in all dependencies
 */
