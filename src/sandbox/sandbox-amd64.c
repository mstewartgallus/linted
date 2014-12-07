/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define ALLOW(XX)                                                              \
	BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_##XX, 0U, 1U),                \
	    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW)

static struct sock_filter const real_filter[] = {
	/**/ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	              offsetof(struct seccomp_data, arch)),
	/**/ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, EM_X86_64, 0U, 1U),
	/**/ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),
	/**/ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	              offsetof(struct seccomp_data, nr)),
	/**/ ALLOW(access),
	/**/ ALLOW(arch_prctl),
	/**/ ALLOW(brk),
	/**/ ALLOW(clock_gettime),
	/**/ ALLOW(clock_nanosleep),
	/**/ ALLOW(clone),
	/**/ ALLOW(close),
	/**/ ALLOW(connect),
	/**/ ALLOW(dup3),
	/**/ ALLOW(execve),
	/**/ ALLOW(exit),
	/**/ ALLOW(exit_group),
	/**/ ALLOW(fcntl),
	/**/ ALLOW(fstat),
	/**/ ALLOW(futex),
	/**/ ALLOW(getdents),
	/**/ ALLOW(getegid),
	/**/ ALLOW(geteuid),
	/**/ ALLOW(getgid),
	/**/ ALLOW(getpeername),
	/**/ ALLOW(getrlimit),
	/**/ ALLOW(getuid),
	/**/ ALLOW(ioctl),
	/**/ ALLOW(kill),
	/**/ ALLOW(madvise),
	/**/ ALLOW(mincore),
	/**/ ALLOW(mmap),
	/**/ ALLOW(mprotect),
	/**/ ALLOW(mq_timedreceive),
	/**/ ALLOW(mq_timedsend),
	/**/ ALLOW(munmap),
	/**/ ALLOW(open),
	/**/ ALLOW(openat),
	/**/ ALLOW(pause),
	/**/ ALLOW(poll),
	/**/ ALLOW(read),
	/**/ ALLOW(recvfrom),
	/**/ ALLOW(recvmsg),
	/**/ ALLOW(rt_sigaction),
	/**/ ALLOW(rt_sigprocmask),
	/**/ ALLOW(rt_sigtimedwait),
	/**/ ALLOW(set_robust_list),
	/**/ ALLOW(set_tid_address),
	/**/ ALLOW(shutdown),
	/**/ ALLOW(socket),
	/**/ ALLOW(stat),
	/**/ ALLOW(tgkill),
	/**/ ALLOW(uname),
	/**/ ALLOW(write),
	/**/ ALLOW(writev),

	/* Debugging related system calls */
	/**/ ALLOW(gettid),
	/**/ ALLOW(getpid),
	/**/ ALLOW(sched_getaffinity),
	/**/ ALLOW(setrlimit),

	/* Valgrind related system calls */
	/**/ ALLOW(getcwd),
	/**/ ALLOW(getppid),
	/**/ ALLOW(gettimeofday),
	/**/ ALLOW(getxattr),
	/**/ ALLOW(mknod),
	/**/ ALLOW(pipe),
	/**/ ALLOW(pread64),
	/**/ ALLOW(prctl),
	/**/ ALLOW(readlink),
	/**/ ALLOW(rt_sigreturn),
	/**/ ALLOW(sched_yield),
	/**/ ALLOW(time),
	/**/ ALLOW(tkill),
	/**/ ALLOW(unlink),

	/**/ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
};

static struct sock_fprog const default_filter = {
	.len = LINTED_ARRAY_SIZE(real_filter),
	.filter = (struct sock_filter *)real_filter
};
