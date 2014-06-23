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

#include "linted/updater.h"
#include "linted/util.h"

/**
 * LINTED_UPDATER_UINT_MAX / 4u
 * / \
 *  |
 *  |
 *  |
 *  |
 *  |------------> 0u
 *
 *
 * |   -|-   |    |    |
 * | /  |  \ |    |    |
 * |/   |   \|    |    |
 * |----|----|----|-----
 * |    |    |\   |   /|
 * |    |    | \  |  / |
 * |    |    |   -|-   |
 */

int main(void)
{
    linted_updater_angle zero = LINTED_UPDATER_ANGLE(0u, 1u);
    linted_updater_int sin_zero = linted_updater_sin(zero);
    if (sin_zero != 0) {
        LINTED_IMPOSSIBILITY("linted_updater_sin(zero) == %" LINTED_UPDATER_Id,
                             sin_zero);
    }

    /* TODO: This is wrong! */
    linted_updater_angle half = LINTED_UPDATER_ANGLE(1u, 2u);
    linted_updater_int sin_half = linted_updater_sin(half);
    if (sin_half != LINTED_UPDATER_INT_MIN) {
        LINTED_IMPOSSIBILITY("linted_updater_sin(half) == %" LINTED_UPDATER_Id,
                             sin_half);
    }

    return EXIT_SUCCESS;
}
