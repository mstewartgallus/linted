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
#include "config.h"

#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <unistd.h>

/**
 * @todo Make asynchronous
 */
linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len)
{
	if (-1 == write(log, msg_ptr, msg_len)) {
		return errno;
	}
	return 0;
}
