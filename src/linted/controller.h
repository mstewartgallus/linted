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
#ifndef LINTED_CONTROLLER_H
#define LINTED_CONTROLLER_H

#include <mqueue.h>
#include <stdbool.h>

/**
 * A handle to access the controller. Is safe to share between processes.
 */
typedef mqd_t linted_controller;

struct linted_controller_message {
    unsigned char left: 1;
    unsigned char right: 1;
    unsigned char up: 1;
    unsigned char down: 1;
};

int linted_controller_pair(linted_controller controller[2],
                           int readflags, int writeflags);
int linted_controller_close(linted_controller controller);

int linted_controller_send(linted_controller controller,
                           struct linted_controller_message const *message);

int linted_controller_notify(linted_controller controller,
                             struct sigevent const *sevp);

int linted_controller_receive(linted_controller controller,
                              struct linted_controller_message *message);

#endif                          /* LINTED_CONTROLLER_H */
