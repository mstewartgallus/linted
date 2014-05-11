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
#ifndef LINTED_QUEUE_H
#define LINTED_QUEUE_H

#include "linted/error.h"

#include <stddef.h>

/**
 * @file
 *
 * Safely mediates between different threads.
 */

struct linted_queue;

linted_error linted_queue_create(struct linted_queue** queuep, size_t msgsize);
void linted_queue_destroy(struct linted_queue* queue);

/**
 * deferred cancellation safe
 */
linted_error linted_queue_send(struct linted_queue* queue, void const* message);

/**
 * deferred cancellation safe
 *
 * @error EBUSY The queue is being used.
 * @error EAGAIN The queue is full.
 */
linted_error linted_queue_try_send(struct linted_queue* queue,
                                   void const* message);

/**
 * deferred cancellation safe
 *
 * @error EBUSY The queue is being used.
 * @error EAGAIN The queue is empty.
 */
linted_error linted_queue_try_recv(struct linted_queue* queue, void* message);

/**
 * deferred cancellation safe
 */
linted_error linted_queue_recv(struct linted_queue* queue, void* message);

#endif /* LINTED_QUEUE_H */
