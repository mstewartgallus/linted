/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define _GNU_SOURCE

#include "config.h"

#include "sandbox.h"

#include "linted/execveat.h"
#include "linted/util.h"

#include <stddef.h>
#include <syscall.h>
#include <unistd.h>

#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>

#define ALLOW(XX)                                                      \
	BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_##XX, 0U, 1U),        \
	    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW)

static struct sock_filter const real_filter[] = {
    /**/ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                  offsetof(struct seccomp_data, arch)),
    /**/ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, EM_X86_64, 0U, 1U),
    /**/ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),
    /**/ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                  offsetof(struct seccomp_data, nr)),

    /* Common */
    /**/ ALLOW(execve),
    /**/ ALLOW(brk),
    /**/ ALLOW(access),
    /**/ ALLOW(mmap),
    /**/ ALLOW(open),
    /**/ ALLOW(stat),
    /**/ ALLOW(fstat),
    /**/ ALLOW(close),
    /**/ ALLOW(read),
    /**/ ALLOW(mprotect),
    /**/ ALLOW(arch_prctl),
    /**/ ALLOW(munmap),
    /**/ ALLOW(set_tid_address),
    /**/ ALLOW(set_robust_list),
    /**/ ALLOW(futex),
    /**/ ALLOW(rt_sigaction),
    /**/ ALLOW(rt_sigprocmask),
    /**/ ALLOW(getrlimit),
    /**/ ALLOW(clone),
    /**/ ALLOW(openat),
    /**/ ALLOW(exit),
    /**/ ALLOW(exit_group),
    /**/ ALLOW(restart_syscall),

    /* Not common */
    /**/ ALLOW(socket),
    /**/ ALLOW(connect),
    /**/ ALLOW(pipe2),
    /**/ ALLOW(pause),
    /**/ ALLOW(lseek),
    /**/ ALLOW(sendto),
    /**/ ALLOW(prctl),
    /**/ ALLOW(eventfd2),
    /**/ ALLOW(fcntl),
    /**/ ALLOW(epoll_create1),
    /**/ ALLOW(epoll_ctl),
    /**/ ALLOW(epoll_wait),
    /**/ ALLOW(readlink),
    /**/ ALLOW(getpeername),
    /**/ ALLOW(clock_nanosleep),
    /**/ ALLOW(write),
    /**/ ALLOW(umask),
    /**/ ALLOW(uname),
    /**/ ALLOW(mkdir),
    /**/ ALLOW(poll),
    /**/ ALLOW(ftruncate),
    /**/ ALLOW(writev),
    /**/ ALLOW(getdents),
    /**/ ALLOW(recvfrom),
    /**/ ALLOW(statfs),
    /**/ ALLOW(recvmsg),
    /**/ ALLOW(pwrite64),
    /**/ ALLOW(geteuid),
    /**/ ALLOW(getuid),
    /**/ ALLOW(kill),
    /**/ ALLOW(rt_sigtimedwait),
    /**/ ALLOW(unlink),
    /**/ ALLOW(getgid),
    /**/ ALLOW(getegid),
    /**/ ALLOW(shutdown),
    /**/ ALLOW(fchown),
    /**/ ALLOW(madvise),
    /**/ ALLOW(ioctl),
    /**/ ALLOW(pread64),
    /**/ ALLOW(sched_yield),
    /**/ ALLOW(fchmod),
    /**/ ALLOW(lstat),
    /**/ ALLOW(setsockopt),
    /**/ ALLOW(tgkill),
    /**/ ALLOW(rt_sigreturn),
    /**/ ALLOW(getsockopt),
    /**/ ALLOW(getsockname),
    /**/ ALLOW(ppoll),
    /**/ ALLOW(sendmsg),

#if 0
    /**/ ALLOW(clock_gettime),
    /**/ ALLOW(dup2),
    /**/ ALLOW(dup3),
    /**/ ALLOW(execveat),
    /**/ ALLOW(mincore),
    /**/ ALLOW(wait4),

    /* Debugging related system calls */
    /**/ ALLOW(gettid),
    /**/ ALLOW(getpid),
    /**/ ALLOW(nanosleep),
    /**/ ALLOW(sched_getaffinity),
    /**/ ALLOW(setrlimit),
    /**/ ALLOW(sigaltstack),

    /* Apitrace related system calls */
    /**/ ALLOW(dup),

    /* Valgrind related system calls */
    /**/ ALLOW(getcwd),
    /**/ ALLOW(getppid),
    /**/ ALLOW(gettimeofday),
    /**/ ALLOW(getxattr),
    /**/ ALLOW(mknod),
    /**/ ALLOW(pipe),
    /**/ ALLOW(pread64),
    /**/ ALLOW(time),
    /**/ ALLOW(tkill),

#endif

    /**/ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)};

static struct sock_fprog const default_filter = {
    .len = LINTED_ARRAY_SIZE(real_filter),
    .filter = (struct sock_filter *)real_filter};

struct sock_fprog const *const linted_sandbox_filter = &default_filter;
