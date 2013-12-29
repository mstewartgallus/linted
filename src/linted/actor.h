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


/*
   First, we do not define a communication protocol, only an encoding
   of data. Any communication protocol is at a higher level. We call
   this encoding the linted network data encoding.
*/

/*
  First we define the smallest possible chunk of data in our code, a
  collection of 8 bits encoding a number in unsigned binary format.
*/
typedef uint_fast8_t linted_actor_byte_fast_t;
typedef uint_least8_t linted_actor_byte_least_t;

typedef struct { linted_actor_byte_fast_t x; } linted_actor_byte_fast;
typedef struct { linted_actor_byte_least_t x; } linted_actor_byte_least;


/*
   Now, we define an API for exchanging data in the linted network
   data encoding between actors.
*/


// An actor that can be written to.
typedef struct { int x; } linted_actor_chan;

// An actor that can be received from.
typedef struct { int x; } linted_actor_port;

void linted_actor_send_byte(linted_actor_chan actor,
                            linted_actor_byte_fast byte);

linted_actor_byte_fast linted_actor_recv_byte(linted_actor_port actor);

#endif /* LINTED_ACTOR_H */
