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
 * @todo Port to --host=i686-w64-mingw32 and CC=winegcc CFLAGS='-mno-cygwin -maccumulate-outgoing-args -std=gnu99'
 * - As part of this process environment sanitization code will need
 *   to be reworked. Windows maintains two copies of environment
 *   variables: one copy in process space and the other copy in kernel
 *   space.
 *
 * @todo Move away from C legacy practises
 * - move away from null terminated strings
 *
 * @todo Solve banding problems that are occuring in the shader
 *
 * @todo Make hang check timers in init
 * - These timers will periodically make sure tasks are reaching their
 *   deadlines by sending a message to every message queue and seeing
 *   if the owner of the queue responds within appropriate
 *   deadlines. It will then kill and restart the process if it is not
 *   responding.
 *
 * @todo Move shell scripts to Python.
 *
 * @todo Monitor vulnerabilities in dependencies and supported platforms
 *
 * @todo Make a minor and a major debug mode
 *
 * @todo Think about whether to use -Wa,--noexecstack and -Wl,-z,noexecstack
 * - I think I read somewhere that these force a nonexecutable stack
 *   but that if things are done correctly that the shouldn't be
 *   happening anyways. I believe what I really want is a
 *   -Wl,--warn-execstack option.
 */
