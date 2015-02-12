/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#ifndef LINTED_UNIT_H
#define LINTED_UNIT_H

#include "linted/error.h"

#include <signal.h>
#include <stddef.h>

/**
 * @file
 *
 * Units.
 */

enum { LINTED_UNIT_NAME_MAX = 255 };
#define LINTED_UNIT_NAME_MAX ((unsigned)LINTED_UNIT_NAME_MAX)

enum { LINTED_UNIT_TYPE_SOCKET, LINTED_UNIT_TYPE_SERVICE };
typedef unsigned char linted_unit_type;

enum { LINTED_UNIT_SOCKET_TYPE_DIR,
       LINTED_UNIT_SOCKET_TYPE_FILE,
       LINTED_UNIT_SOCKET_TYPE_FIFO };
typedef unsigned char linted_unit_socket_type;

struct linted_unit_db;

struct linted_unit;
struct linted_unit_service;
struct linted_unit_socket;

linted_error linted_unit_db_create(struct linted_unit_db **unitsp);
void linted_unit_db_destroy(struct linted_unit_db *units);

linted_error linted_unit_db_add_unit(struct linted_unit_db *units,
                                     struct linted_unit **unitp);
size_t linted_unit_db_size(struct linted_unit_db *units);
struct linted_unit *linted_unit_db_get_unit(struct linted_unit_db *units,
                                            size_t ii);
struct linted_unit *linted_unit_db_get_unit_by_name(struct linted_unit_db *unit,
                                                    char const *name);

struct linted_unit
{
	char *name;
	linted_unit_type type;
};

struct linted_unit_socket
{
	struct linted_unit common;
	char const *path;
	linted_unit_socket_type type;
	_Bool is_open : 1U;
};

#if defined _POSIX_C_SOURCE && _POSIX_C_SOURCE >= 1
struct linted_unit_service
{
	struct linted_unit common;

	char const *const *exec_start;
	char const *fstab;
	char const *chdir_path;
	char const *const *env_whitelist;

	sigset_t const *sigmask;
	char const *sandbox;
	char const *waiter;

	_Bool clone_newuser : 1U;
	_Bool clone_newpid : 1U;
	_Bool clone_newipc : 1U;
	_Bool clone_newnet : 1U;
	_Bool clone_newns : 1U;
	_Bool clone_newuts : 1U;

	_Bool no_new_privs : 1U;
};
#endif

#endif /* LINTED_UNIT_H */
