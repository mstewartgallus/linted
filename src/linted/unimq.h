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
#ifndef LINTED_UNIMQ_H
#define LINTED_UNIMQ_H

#include <stddef.h>

struct linted__unimq;
typedef struct linted__unimq *linted_unimq;

struct linted_unimq_attr {
    size_t max_message_count;
    size_t message_size;
};

int linted_unimq_init(linted_unimq * mq, struct linted_unimq_attr *attr);

int linted_unimq_send(linted_unimq mq, void const *msg_ptr);

int linted_unimq_receive(linted_unimq mq, void *msg_ptr);

int linted_unimq_destroy(linted_unimq mq);

#endif                          /* LINTED_UNIMQ_H */
