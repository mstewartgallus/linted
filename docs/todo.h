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
 * @todo Fix collision handling. I want to prevent interpenetration
 *       and properly handle discontinuities in velocity. I need
 *       proper collision response and not the hacky spring like thing
 *       I have currently.
 *       Resources:
 *       - http://www.gdcvault.com/play/1018239/Physics-for-Game-Programmers-Continuous
 *       - http://gafferongames.com/virtualgo/collision-response-and-coulomb-friction/
 *       - http://www.codezealot.org/archives/55
 *       - http://www.codezealot.org/archives/88
 *       - http://mollyrocket.com/849
 *       - http://chrishecker.com/images/d/df/Gdmphys1.pdf
 *       - http://chrishecker.com/images/c/c2/Gdmphys2.pdf
 *       - http://chrishecker.com/images/e/e7/Gdmphys3.pdf
 *       - http://chrishecker.com/images/b/bb/Gdmphys4.pdf
 *       - http://www.pixar.com/companyinfo/research/pbm2001/pdf/notesg.pdf
 *       - http://www.wildbunny.co.uk/blog/2011/04/06/physics-engines-for-dummies/
 *       - http://www.bulletphysics.com/ftp/pub/test/physics/papers/IterativeDynamics.pdf
 *
 * @todo Add platform specific information and defines to static
 *       analysis tooling.
 *
 * @todo Eventually reduce `-Wstack-usage` to 500.
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
 * @todo Harden bind mounts with extra flags
 *
 * @todo Check child processes for hangups
 *
 * @todo Use the monitor to report crashes such as from seccomp rule
 *       violations.
 *
 * @todo Create a feed of possible vulnerabilities in all dependencies
 *
 * @todo Sandbox leaked file descriptors before spawning new processes
 *       in sandbox.c
 */
