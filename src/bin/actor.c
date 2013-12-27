/*
 * Copyright 2013 Steven Stewart-Gallus
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

#include "linted/actor.h"
#include "linted/util.h"

#include "linted/base/stdio.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>

void linted_actor_send_byte(linted_actor_chan const actor,
                            linted_actor_byte_fast const byte) {
    if (fwrite(&byte.x, 1, 1, actor.x) != 1) {
        LINTED_ERROR("Could not write bytes to file.\n");
    }
}

void linted_actor_flush(const linted_actor_chan actor) {
    int error_status;
    do {
        error_status = fflush(actor.x);
    } while (EOF == error_status && errno != EINTR);
    if (EOF == error_status) {
        LINTED_ERROR("Could not write to standard output because of error: %s\n",
                     strerror(errno));
    }
}

linted_actor_byte_fast linted_actor_recv_byte(linted_actor_port const actor) {
    int const input = fgetc(actor.x);
    if (EOF == input) {
        LINTED_ERROR("Could not receive byte for actor.\n");
    }
    return (linted_actor_byte_fast) { .x = input };
}
