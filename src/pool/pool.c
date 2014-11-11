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

#include "linted/pool.h"

#include "linted/mem.h"

#include <assert.h>
#include <errno.h>

struct linted_pool
{
	struct linted_pool_node tip;
};

linted_error linted_pool_create(struct linted_pool **poolp)
{
	linted_error errnum;
	struct linted_pool *pool;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *pool);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	struct linted_pool_node *tip = &pool->tip;

	tip->left = tip;
	tip->right = tip;

	*poolp = pool;

	return 0;
}

void linted_pool_destroy(struct linted_pool *pool)
{
	assert(&pool->tip == pool->tip.left);
	assert(&pool->tip == pool->tip.right);

	linted_mem_free(pool);
}

void linted_pool_insert_node(struct linted_pool *pool,
                             struct linted_pool_node *node)
{
	/* Guard against double insertions */
	assert(NULL == node->right);
	assert(NULL == node->left);

	struct linted_pool_node *tip = &pool->tip;

	struct linted_pool_node *right = tip->right;

	tip->right = node;
	right->left = node;

	node->left = tip;
	node->right = right;
}

void linted_pool_node_create(struct linted_pool_node *node)
{
	node->left = NULL;
	node->right = NULL;
}

void linted_pool_node_discard(struct linted_pool_node *node)
{
	struct linted_pool_node *left = node->left;
	struct linted_pool_node *right = node->right;

	node->left = NULL;
	node->right = NULL;

	left->right = right;
	right->left = left;
}

linted_error linted_pool_remove_node(struct linted_pool *pool,
                                     struct linted_pool_node **nodep)
{
	struct linted_pool_node *tip = &pool->tip;

	struct linted_pool_node *right = tip->right;

	if (tip == right)
		return EAGAIN;

	linted_pool_node_discard(right);

	*nodep = right;

	return 0;
}
