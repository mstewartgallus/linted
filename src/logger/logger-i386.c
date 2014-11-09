/*
 * Copyright 2014 Steven Stewart-Gallus
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
	                offsetof(struct seccomp_data, nr)),

	/*  */ ALLOW(access),
	/*  */ ALLOW(brk),
	/*  */ ALLOW(clone),
	/*  */ ALLOW(close),
	/*  */ ALLOW(dup2),
	/*  */ ALLOW(execve),
	/*  */ ALLOW(fcntl64),
	/*  */ ALLOW(fstat64),
	/*  */ ALLOW(futex),
	/*  */ ALLOW(getdents64),
	/*  */ ALLOW(getgid32),
	/*  */ ALLOW(getrlimit),
	/*  */ ALLOW(getuid32),
	/*  */ ALLOW(mmap2),
	/*  */ ALLOW(mprotect),
	/*  */ ALLOW(munmap),
	/*  */ ALLOW(open),
	/*  */ ALLOW(openat),
	/*  */ ALLOW(futex),
	/*  */ ALLOW(rt_sigprocmask),
	/*  */ ALLOW(rt_sigtimedwait),
	/*  */ ALLOW(write),
	/*  */ ALLOW(futex),
	/*  */ ALLOW(mq_timedreceive),
	/*  */ ALLOW(poll),
	/*  */ ALLOW(set_robust_list),
	/*  */ ALLOW(prctl),
	/*  */ ALLOW(read),
	/*  */ ALLOW(rt_sigaction),
	/*  */ ALLOW(rt_sigprocmask),
	/*  */ ALLOW(set_robust_list),
	/*  */ ALLOW(set_thread_area),
	/*  */ ALLOW(set_tid_address),
	/*  */ ALLOW(uname),

	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
};

static struct sock_fprog const seccomp_filter = {
	.len = LINTED_ARRAY_SIZE(real_filter),
	.filter = (struct sock_filter *)real_filter
};
