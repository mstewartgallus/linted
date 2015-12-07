/*
 * Copyright 2015 Steven Stewart-Gallus
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
#ifndef LNTD_CHANNEL_H
#define LNTD_CHANNEL_H

#include "lntd/error.h"

/**
 * @file
 *
 * Mediates one-by-one communication between threads.
 *
 * @warning Is not cancellation safe.
 */
struct lntd_channel;

lntd_error lntd_channel_create(struct lntd_channel **channelp);

void lntd_channel_destroy(struct lntd_channel *channel);

lntd_error lntd_channel_try_send(struct lntd_channel *channel,
                                 void *node);

void lntd_channel_recv(struct lntd_channel *channel, void **node);

#endif /* LNTD_CHANNEL_H */