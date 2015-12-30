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
#ifndef LNTD_ASYNC_H
#define LNTD_ASYNC_H

#include "bool.h"
#include "lntd/error.h"
#include "lntd/ko.h"

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

enum { LNTD_ASYNC_CMD_TYPE_IDLE,
       LNTD_ASYNC_CMD_TYPE_POLL,
       LNTD_ASYNC_CMD_TYPE_TIMER,
       LNTD_ASYNC_CMD_TYPE_WRITE,
       LNTD_ASYNC_CMD_TYPE_READ };
typedef unsigned char lntd_async_cmd_type;

typedef uint_least8_t lntd_async_command_id;
typedef uint_least8_t lntd_async_poller_id;
typedef uint_least8_t lntd_task_id;

struct lntd_async_cmd_write {
	char const *bytes;
	size_t size;
	size_t bytes_left;
	lntd_ko ko;
	lntd_async_poller_id poller;
};

struct lntd_async_cmd_read {
	char *bytes;
	size_t size;
	size_t bytes_left;
	lntd_ko ko;
	lntd_async_poller_id poller;
};

struct lntd_async_cmd_poll {
	uint_least64_t events;
	uint_least64_t revents;
	lntd_ko ko;
	lntd_async_poller_id poller;
};

struct lntd_async_cmd_timer {
	struct timespec request;
	lntd_ko ko;
	lntd_async_poller_id poller;
};

#define LNTD_ASYNC_COMMAND                                             \
	"LntdAsyncCommand-6d5734b0-1474-4a28-945e-8ca970fc2a58"

#define LNTD_ASYNC_POLLER                                              \
	"LntdAsyncPoller-6d5734b0-1474-4a28-945e-8ca970fc2a58"

enum { LNTD_ASYNC_POLLER_IN = 1, LNTD_ASYNC_POLLER_OUT = 1 << 1 };

#endif /* LNTD_ASYNC_H */
