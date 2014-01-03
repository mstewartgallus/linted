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
#ifndef LINTED_ACTOR_H
#define LINTED_ACTOR_H

#include <inttypes.h>
#include <stdio.h>

// An actor that can be written to.
typedef struct { int x; } linted_actor_chan;

// An actor that can be received from.
typedef struct { int x; } linted_actor_port;

void linted_actor_send(linted_actor_chan actor, void const * bytes, size_t size);
uint8_t linted_actor_recv_byte(linted_actor_port actor);

#endif /* LINTED_ACTOR_H */
