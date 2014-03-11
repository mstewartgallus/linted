Copyright (C) 2014 Steven Stewart-Gallus

Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.

Potentially Vulnerable Runtime Dependencies
===========================================

*   glLoadGenerator - https://bitbucket.org/alfonse/glloadgen/

    glLoadGenerator is both a build and a runtime dependency as it
    generates code to be used.

*   Linux - https://www.kernel.org/

*   The GNU C Library - https://www.gnu.org/software/libc/

    There are a few race conditions and bugs in gLibc so we need to be
    careful.

*   The GNU Compiler Collection - https://www.gnu.org/software/gcc/
*   The GNU Binary Utilities - https://www.gnu.org/software/binutils/
