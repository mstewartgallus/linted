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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_FIFO_H
#define LNTD_FIFO_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <sys/types.h>

/**
 * @file
 *
 * Abstracts over the concept of a filesystem pipe.
 */

typedef lntd_ko lntd_fifo;

#define LNTD_FIFO_RDONLY 1UL
#define LNTD_FIFO_WRONLY (1UL << 1U)
#define LNTD_FIFO_RDWR (1UL << 2U)

#define LNTD_FIFO_ONLY (1UL << 3U)

lntd_error lntd_fifo_create(lntd_fifo *fifop, lntd_ko dirko,
                            char const *pathname, unsigned long flags,
                            mode_t mode);

lntd_error lntd_fifo_pair(lntd_fifo *readerp, lntd_fifo *writerp,
                          unsigned long flags);

#endif /* LNTD_FIFO_H */
