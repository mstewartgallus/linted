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
#ifndef LINTED_POOL_H
#define LINTED_POOL_H

#include "linted/error.h"

/**
 * @file
 *
 * Holds onto a pool of items
 */
struct linted_pool;

struct linted_pool_node
{
	struct linted_pool_node *left;
	struct linted_pool_node *right;
};

void linted_pool_node(struct linted_pool_node *node);

linted_error linted_pool_create(struct linted_pool **poolp);

/**
 * @warning It is the responsibility of the caller to fetch and
 * destroy all nodes in the queue.
 */
void linted_pool_destroy(struct linted_pool *pool);

void linted_pool_insert_node(struct linted_pool *pool,
                             struct linted_pool_node *node);

linted_error linted_pool_remove_node(struct linted_pool *pool,
                                     struct linted_pool_node **nodep);

void linted_pool_node_create(struct linted_pool_node *node);
void linted_pool_node_discard(struct linted_pool_node *node);

#endif /* LINTED_POOL_H */
