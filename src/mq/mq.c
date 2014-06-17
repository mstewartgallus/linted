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

#include "linted/error.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <mqueue.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define TEMPLATE_PREFIX "/anonymous-mq-"
#define TEMPLATE_NAME TEMPLATE_PREFIX "XXXXXXXXXX"

/**
 * Implemented using POSIX message queues.
 */

linted_error linted_mq_create(linted_mq *mqp, struct linted_mq_attr *attr,
                              int flags)
{
    linted_error errnum;
    struct mq_attr mq_attr;
    char random_mq_name[sizeof TEMPLATE_NAME];
    linted_mq ko;

    mq_attr.mq_flags = 0;
    mq_attr.mq_curmsgs = 0;
    mq_attr.mq_maxmsg = attr->maxmsg;
    mq_attr.mq_msgsize = attr->msgsize;

    memcpy(random_mq_name, TEMPLATE_NAME, sizeof TEMPLATE_NAME);

    /* Seed the generator with rand */
    /**
     * @todo Replace the use of the terrible rand function.
     */
    /* Use unsigned so possibly overflowing later on is defined */
    unsigned long generator_state = rand();

    do {
        for (size_t ii = sizeof TEMPLATE_PREFIX - 1u;
             ii < sizeof TEMPLATE_NAME - 1u; ++ii) {
            for (;;) {
                /* Use a fast linear congruential generator */
                generator_state = 5u + 3u * generator_state;

                /* Normally using the modulus would give a bad
         * distribution but CHAR_MAX + 1u is a power of two
         */
                unsigned char const possible_value =
                    generator_state % (CHAR_MAX + 1u);

                /* Throw out results and retry for an even
         * distribution
         */
                if ((possible_value >= 'a' && possible_value <= 'z') ||
                    (possible_value >= 'A' && possible_value <= 'Z') ||
                    (possible_value >= '0' && possible_value <= '9')) {
                    random_mq_name[ii] = possible_value;
                    break;
                }
            }
        }

        ko = mq_open(random_mq_name, O_RDWR | O_CREAT | O_EXCL | O_NONBLOCK,
                     S_IRUSR | S_IWUSR, &mq_attr);
        if (-1 == ko) {
            errnum = errno;
            assert(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EEXIST == errnum);
    if (errnum != 0) {
        goto exit_with_error;
    }

    if (-1 == mq_unlink(random_mq_name)) {
        errnum = errno;
        assert(errnum != 0);
        goto exit_with_error_and_close;
    }

    *mqp = ko;

    return 0;

exit_with_error_and_close:
    mq_close(ko);

exit_with_error:
    return errnum;
}
