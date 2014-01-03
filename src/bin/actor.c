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
#include "linted/base/stdio.h"
#include "linted/util.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>

void linted_actor_send(linted_actor_chan const actor,
                       void const * const message, size_t const size) {
    ssize_t bytes_written;
    do {
        bytes_written = write(actor.x, message, size);
    } while (bytes_written != size && (errno == EINTR));
    if (bytes_written != size) {
        LINTED_ERROR("Could not write bytes to file descriptor %d: %s.\n",
                     actor.x, strerror(errno));
    }
}

void linted_actor_recv(linted_actor_port const actor,
                       void * const message, size_t const size) {
    ssize_t bytes_read;
    do {
        bytes_read = read(actor.x, message, size);
    } while (bytes_read != size && (errno == EINTR));
    if (bytes_read != size) {
        LINTED_ERROR("Could not read bytes from file descriptor %d: %s.\n",
                     actor.x, strerror(errno));
    }
}
