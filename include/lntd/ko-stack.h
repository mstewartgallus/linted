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
#ifndef LNTD_KO_STACK_H
#define LNTD_KO_STACK_H

#include "lntd/ko.h"

struct lntd_node;

struct lntd_ko_stack;

lntd_error lntd_ko_stack_create(struct lntd_ko_stack **queuep);
void lntd_ko_stack_destroy(struct lntd_ko_stack *queue);

void lntd_ko_stack_send(struct lntd_ko_stack *queue,
                        struct lntd_node *node);
lntd_error lntd_ko_stack_try_recv(struct lntd_ko_stack *queue,
                                  struct lntd_node **nodep);

lntd_ko lntd_ko_stack_ko(struct lntd_ko_stack *queue);

#endif /* LNTD_KO_STACK_H */
