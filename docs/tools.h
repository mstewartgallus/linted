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
 * @section
 * <ul>
 *
 * <li> Valgrind
 *
 * - You have to comment out and disable the seccomp sandboxing.
 * - You have to make sure every unit has a /tmp directory.
 * </li>
 *
 * @section rejected Rejected Tools
 *
 * <ul>
 *
 * <li> Linux Dwarves tools
 *
 * - dwarves
 *   - codiff       Diffs changes in binaries
 *   - dtagnames    Lists tag names
 *   - pahole       Finds holes in structures
 *   - pdwtags      Dwarf information pretty printer
 *   - pfunct       Displays information about functions
 *   - pglobal      Displays information about global variables
 *   - prefcnt      Tries to find unreferenced tags
 *
 * These tools would be extremely useful if they worked on a wider
 * range of types. So far, they all seem to break and not handle any
 * structure types that aren't extremely ordinary.
 *
 * </li>
 *
 * <li> Clang's -Wthread-safety flags
 *
 * This tool is okay but cannot handle thread cancellation which is
 * needed to kill worker threads that are taking too long.
 *
 * </li>
 *
 * <li> Flawfinder
 *
 * - Seems to be defunct right now.
 *
 * - Gives many false positives.
 *
 * - Did give a nice tip about how InitializeCriticalSection can throw
 *   an exception however.
 *
 * </li>
 *
 * <li> RATS
 *
 * - Seems to be defunct right now.
 *
 * - Gives many false positives.
 *
 * - Did give a nice tip about how InitializeCriticalSection can throw
 *   an exception however.
 *
 * </li>
 *
 * </ul>
 */
