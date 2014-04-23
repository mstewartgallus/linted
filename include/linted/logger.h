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
#ifndef LINTED_LOGGER_H
#define LINTED_LOGGER_H

#include <errno.h>
#include <mqueue.h>
#include <stddef.h>

#define LINTED_LOGGER_LOG_MAX 1024

/**
 * A handle to access the logger. Is safe to share between processes.
 */
typedef mqd_t linted_logger;

errno_t linted_logger_pair(linted_logger logger[2]);

errno_t linted_logger_close(linted_logger logger);

errno_t linted_logger_log(linted_logger logger, char const *msg_ptr,
                          size_t msg_len);

errno_t linted_logger_recv_log(linted_logger logger,
                               char msg_ptr[static LINTED_LOGGER_LOG_MAX],
                               size_t * msg_len);

#endif                          /* LINTED_LOGGER_H */