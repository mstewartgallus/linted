/*
 * Copyright 2015 Steven Stewart-Gallus
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
    /**/ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, EM_ARM, 0U, 1U),
    /**/ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),
    /**/ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                  offsetof(struct seccomp_data, nr)),

    /**/ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)};

static struct sock_fprog const default_filter = {
    .len = LINTED_ARRAY_SIZE(real_filter),
    .filter = (struct sock_filter *)real_filter};

struct sock_fprog const *const linted_sandbox_filter = &default_filter;
