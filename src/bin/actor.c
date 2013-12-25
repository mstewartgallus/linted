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

void linted_actor_send_byte(const linted_actor_chan actor,
                            const linted_actor_byte_fast byte) {
    linted_fwrite(&byte, sizeof byte, 1, actor.x);
}

void linted_actor_flush(const linted_actor_chan actor) {
    linted_fflush(actor.x);
}

linted_actor_byte_fast linted_actor_recv_byte(const linted_actor_port actor) {
    linted_actor_byte_fast_t byte = 0;
    linted_fread(&byte, sizeof byte, 1, actor.x);
    return (linted_actor_byte_fast) { .x = byte };
}
