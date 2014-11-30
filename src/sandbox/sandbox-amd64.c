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
	/*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	                offsetof(struct seccomp_data, arch)),
	/*  */ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, EM_X86_64, 0U, 1U),
	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),
	/*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	                offsetof(struct seccomp_data, nr)),
	/*  */ ALLOW(access),
	/*  */ ALLOW(arch_prctl),
	/*  */ ALLOW(brk),
	/*  */ ALLOW(clock_gettime),
	/*  */ ALLOW(clock_nanosleep),
	/*  */ ALLOW(clone),
	/*  */ ALLOW(close),
	/*  */ ALLOW(connect),
	/*  */ ALLOW(dup),
	/*  */ ALLOW(dup2),
	/*  */ ALLOW(dup3),
	/*  */ ALLOW(execve),
	/*  */ ALLOW(exit),
	/*  */ ALLOW(exit_group),
	/*  */ ALLOW(fcntl),
	/*  */ ALLOW(fork),
	/*  */ ALLOW(fstat),
	/*  */ ALLOW(futex),
	/*  */ ALLOW(getcwd),
	/*  */ ALLOW(getdents),
	/*  */ ALLOW(getegid),
	/*  */ ALLOW(geteuid),
	/*  */ ALLOW(getgid),
	/*  */ ALLOW(getpeername),
	/*  */ ALLOW(getpid),
	/*  */ ALLOW(getppid),
	/*  */ ALLOW(getpriority),
	/*  */ ALLOW(getrlimit),
	/*  */ ALLOW(gettid),
	/*  */ ALLOW(gettimeofday),
	/*  */ ALLOW(getuid),
	/*  */ ALLOW(getxattr),
	/*  */ ALLOW(ioctl),
	/*  */ ALLOW(kill),
	/*  */ ALLOW(lseek),
	/*  */ ALLOW(madvise),
	/*  */ ALLOW(mincore),
	/*  */ ALLOW(mknod),
	/*  */ ALLOW(mmap),
	/*  */ ALLOW(mprotect),
	/*  */ ALLOW(mq_timedreceive),
	/*  */ ALLOW(mq_timedsend),
	/*  */ ALLOW(munmap),
	/*  */ ALLOW(open),
	/*  */ ALLOW(openat),
	/*  */ ALLOW(pause),
	/*  */ ALLOW(pipe),
	/*  */ ALLOW(pipe2),
	/*  */ ALLOW(poll),
	/*  */ ALLOW(ppoll),
	/*  */ ALLOW(prctl),
	/*  */ ALLOW(pread64),
	/*  */ ALLOW(read),
	/*  */ ALLOW(readlink),
	/*  */ ALLOW(recvfrom),
	/*  */ ALLOW(recvmsg),
	/*  */ ALLOW(restart_syscall),
	/*  */ ALLOW(rt_sigaction),
	/*  */ ALLOW(rt_sigprocmask),
	/*  */ ALLOW(rt_sigreturn),
	/*  */ ALLOW(rt_sigtimedwait),
	/*  */ ALLOW(sched_getaffinity),
	/*  */ ALLOW(sched_yield),
	/*  */ ALLOW(setgroups),
	/*  */ ALLOW(setpriority),
	/*  */ ALLOW(setrlimit),
	/*  */ ALLOW(set_robust_list),
	/*  */ ALLOW(setsid),
	/*  */ ALLOW(set_tid_address),
	/*  */ ALLOW(shutdown),
	/*  */ ALLOW(socket),
	/*  */ ALLOW(stat),
	/*  */ ALLOW(tgkill),
	/*  */ ALLOW(time),
	/*  */ ALLOW(tkill),
	/*  */ ALLOW(uname),
	/*  */ ALLOW(unlink),
	/*  */ ALLOW(vfork),
	/*  */ ALLOW(wait4),
	/*  */ ALLOW(waitid),
	/*  */ ALLOW(write),
	/*  */ ALLOW(writev),

	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
};

static struct sock_fprog const default_filter = {
	.len = LINTED_ARRAY_SIZE(real_filter),
	.filter = (struct sock_filter *)real_filter
};
