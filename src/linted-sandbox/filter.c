#if defined __amd64__
#include "sandbox-amd64.c"
#elif defined __i386__
#include "sandbox-i386.c"
#elif defined __arm__
#include "sandbox-arm.c"
#else
#error No default seccomp filter has been defined for this architecture
#endif
