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
#ifndef LINTED_LOG_H
#define LINTED_LOG_H

#include "linted/error.h"
#include "linted/ko.h"

#include <stddef.h>

/**
 * @file
 *
 * Exposes a protocol for logging events.
 */

#define LINTED_LOG_MAX 1024U

/**
 * A handle to access the log. Is safe to share between processes.
 */
typedef linted_ko linted_log;

linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len);

#endif /* LINTED_LOG_H */
