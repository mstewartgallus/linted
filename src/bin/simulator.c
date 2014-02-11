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
#include "config.h"

#include "linted/simulator.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define SIGN(x) ((x) < 0 ? 1 : (0 == x) ? 0 : -1)

int linted_simulator_run(linted_controller_t const inbox, linted_gui_t const gui)
{
    int32_t x_left = 0;
    int32_t x_right = 0;

    int32_t y_up = 0;
    int32_t y_down = 0;

    int32_t x_position = 0;
    int32_t y_position = 0;

    int32_t x_velocity = 0;
    int32_t y_velocity = 0;

    for (;;) {
        struct linted_controller_message message_data;

        int read_status;
        do {
            read_status = linted_controller_receive(inbox, &message_data);
        } while (-1 == read_status && EINTR == errno);
        if (-1 == read_status) {
            return -1;
        }

        switch (message_data.type) {
        case LINTED_CONTROLLER_SHUTDOWN:
            goto exit_main_loop;

        case LINTED_CONTROLLER_MOVEMENT:
            switch (message_data.direction) {
            case LINTED_CONTROLLER_LEFT:
                x_left = message_data.moving;
                break;

            case LINTED_CONTROLLER_RIGHT:
                x_right = message_data.moving;
                break;

            case LINTED_CONTROLLER_UP:
                y_up = message_data.moving;
                break;

            case LINTED_CONTROLLER_DOWN:
                y_down = message_data.moving;
                break;
            }
            break;

        case LINTED_CONTROLLER_TICK:{
                int32_t x_thrust = 2 * (x_right - x_left);
                int32_t y_thrust = 2 * (y_up - y_down);

                int32_t x_future_velocity = x_thrust + x_velocity;
                int32_t y_future_velocity = y_thrust + y_velocity;

                int32_t x_friction = MIN(imaxabs(x_future_velocity), 1) * SIGN(x_future_velocity);
                int32_t y_friction = MIN(imaxabs(y_future_velocity), 1) * SIGN(y_future_velocity);

                x_velocity += x_thrust + x_friction;
                y_velocity += y_thrust + y_friction;

                x_position += x_velocity;
                y_position += y_velocity;

                int update_status;
                do {
                    update_status = linted_gui_send_update(gui, x_position, y_position);
                } while (-1 == update_status && EINTR == errno);
                if (-1 == update_status) {
                    return -1;
                }
                break;
            }

        default:
            LINTED_ERROR("Received unexpected message type: %i",
                         message_data.type);
        }
    }

 exit_main_loop:
    return 0;
}
