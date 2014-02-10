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
#ifndef LINTED_MOVE_H
#define LINTED_MOVE_H

#include <mqueue.h>
#include <stdbool.h>

typedef mqd_t linted_move_queue_t;

enum linted_move_direction {
    LINTED_MOVE_UP,
    LINTED_MOVE_DOWN,
    LINTED_MOVE_LEFT,
    LINTED_MOVE_RIGHT
};

int linted_move_queue_pair(linted_move_queue_t queues[2]);

int linted_move_queue_send(linted_move_queue_t queue,
                           enum linted_move_direction direction,
                           bool moving);

int linted_move_queue_notify(linted_move_queue_t queue,
                             struct sigevent const * sevp);

int linted_move_queue_receive(linted_move_queue_t queue,
                              enum linted_move_direction * direction,
                              bool * moving);

int linted_move_queue_close(linted_move_queue_t move);

#endif                          /* LINTED_MOVE_H */
