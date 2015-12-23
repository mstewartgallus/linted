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
#include "error.h"
#include "ko.h"

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

enum { LNTD_ASYNC_CMD_TYPE_IDLE,
       LNTD_ASYNC_CMD_TYPE_POLL,
       LNTD_ASYNC_CMD_TYPE_TIMER,
       LNTD_ASYNC_CMD_TYPE_WRITE,
       LNTD_ASYNC_CMD_TYPE_READ };
typedef unsigned char lntd_async_cmd_type;

struct lntd_async_cmd_write {
	char const *bytes;
	size_t size;
	lntd_ko ko;
	size_t bytes_left;
};

struct lntd_async_cmd_read {
	char *bytes;
	size_t size;
	lntd_ko ko;
	size_t bytes_left;
};

struct lntd_async_cmd_poll {
	uint_least64_t events;
	uint_least64_t revents;
	lntd_ko ko;
};

struct lntd_async_cmd_timer {
	struct timespec request;
	lntd_ko ko;
};

#endif /* LNTD_ASYNC_H */
