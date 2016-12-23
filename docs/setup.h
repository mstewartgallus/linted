/* Copyright (C) 2016 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- Process Setup
 *
 * First the process should open standard handles.
 *
 * The logger should be initialized as soon as possible.
 *
 * Second the process should figure out its name whether by
 * commandline argument or environment variable.
 *
 * Signals need to be setup.
 *
 * The locale needs to be initialized.
 *
 * Privilege should be checked.  The process shouldn't run at high
 * privilege.
 *
 * Next the process should fork off the main thread and then exit it
 * or send it into a loop.
 *
 * Next the process should sanitize file descriptors.
 */
