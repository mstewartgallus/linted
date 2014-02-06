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

#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <limits.h>
#include <mqueue.h>
#include <stdlib.h>
#include <string.h>

#define TEMPLATE_PREFIX "/anonymous-mq-"
#define TEMPLATE_NAME (TEMPLATE_PREFIX "XXXXXXXXXX")

mqd_t linted_mq_anonymous(struct mq_attr * attr, int oflag)
{
    char random_mq_name[sizeof TEMPLATE_NAME];
    memcpy(random_mq_name, TEMPLATE_NAME, sizeof TEMPLATE_NAME);

    mqd_t new_mq;
    do {
        /* TODO: Replace, rand is terrible */
        /* Seed with rand. */
        /* Normally using the modulus would give a bad distribution
           but CHAR_MAX + 1 is a power of two. */
        unsigned char state = rand() % (CHAR_MAX + 1);

        for (size_t ii = sizeof TEMPLATE_PREFIX - 1; ii < sizeof TEMPLATE_NAME - 1; ++ii) {
            for (;;) {
                /* Use a fast linear congruential generator */
                state = 5 + 3 * state;
                unsigned const possible_value = state;

                /* Again, throw out results that don't fit for an even
                 * distribution of values.
                 */
                if ((possible_value >= 'a' && possible_value <= 'z')
                    || (possible_value >= 'A' && possible_value <= 'Z')
                    || (possible_value >= '0' && possible_value <= '9')) {
                    random_mq_name[ii] = possible_value;
                    break;
                }
            }
        }

        new_mq = mq_open(random_mq_name, oflag | O_RDWR | O_CREAT | O_EXCL, 0, attr);
    } while (-1 == new_mq && EEXIST == errno);

    if (new_mq != -1) {
        /* This could only happen via programmer error */
        if (-1 == mq_unlink(random_mq_name)) {
            LINTED_ERROR
                ("Could not remove message queue with name %s because of error: %m",
                 random_mq_name, errno);
        }
    }

    return new_mq;
}
