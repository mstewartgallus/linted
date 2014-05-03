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
#ifndef LINTED_UPDATER_H
#define LINTED_UPDATER_H

#include <errno.h>
#include <mqueue.h>
#include <stdint.h>

/**
 * A handle to access the updater. Is safe to share between processes.
 */
typedef mqd_t linted_updater;

struct linted_updater_update {
    int_fast32_t x_position;
    int_fast32_t y_position;
    int_fast32_t z_position;

    uint_fast32_t x_rotation;
    uint_fast32_t y_rotation;
};

errno_t linted_updater_pair(linted_updater updater[2], int rflags, int wflags);

errno_t linted_updater_send_update(linted_updater updater,
                                   struct linted_updater_update const* update);

errno_t linted_updater_receive_update(linted_updater updater,
                                      struct linted_updater_update* update);

errno_t linted_updater_close(linted_updater updater);

#endif /* LINTED_UPDATER_H */
