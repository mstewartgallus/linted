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
#include <sys/stat.h>

#define TEMPLATE_PREFIX "/anonymous-mq-"
#define TEMPLATE_NAME (TEMPLATE_PREFIX "XXXXXXXXXX")

int linted_mq_pair(mqd_t mqs[2], struct mq_attr * attr, int oflag)
{
    char random_mq_name[sizeof TEMPLATE_NAME];
    mqd_t write_end;
    mqd_t read_end;

    memcpy(random_mq_name, TEMPLATE_NAME, sizeof TEMPLATE_NAME);

    /* Seed the generator with rand */
    /* TODO: Replace rand which is terrible */
    /* Use unsigned so possibly overflowing later on is defined */
    unsigned long generator_state = rand();

    do {
        for (size_t ii = sizeof TEMPLATE_PREFIX - 1; ii < sizeof TEMPLATE_NAME - 1; ++ii) {
            for (;;) {
                /* Use a fast linear congruential generator */
                generator_state = 5 + 3 * generator_state;

                /* Normally using the modulus would give a bad
                 * distribution but CHAR_MAX + 1 is a power of two
                 */
                unsigned char const possible_value = generator_state % (CHAR_MAX + 1);

                /* Throw out results and retry for an even
                 * distribution
                 */
                if ((possible_value >= 'a' && possible_value <= 'z')
                    || (possible_value >= 'A' && possible_value <= 'Z')
                    || (possible_value >= '0' && possible_value <= '9')) {
                    random_mq_name[ii] = possible_value;
                    break;
                }
            }
        }

        write_end = mq_open(random_mq_name,
                            oflag | O_WRONLY | O_CREAT | O_EXCL,
                            S_IRUSR,
                            attr);
    } while (-1 == write_end && EEXIST == errno);
    if (-1 == write_end) {
        goto exit_with_error;
    }

    read_end = mq_open(random_mq_name, oflag | O_RDONLY);
    if (-1 == read_end) {
        goto exit_with_error_and_close_write_end;
    }

    if (-1 == mq_unlink(random_mq_name)) {
        goto exit_with_error_and_close_read_end;
    }

    mqs[0] = read_end;
    mqs[1] = write_end;

    return 0;

exit_with_error_and_close_read_end:
    {
        int errnum = errno;
        mq_close(write_end);
        errno = errnum;
    }

exit_with_error_and_close_write_end:
    {
        int errnum = errno;
        mq_close(write_end);
        errno = errnum;
    }

exit_with_error:
    return -1;
}
