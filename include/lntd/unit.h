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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_UNIT_H
#define LNTD_UNIT_H

#include "lntd/error.h"
#include "lntd/proc.h"
#include "lntd/sched.h"

#include <stddef.h>
#include <stdint.h>

/**
 * @file
 *
 * Units.
 */

enum { LNTD_UNIT_NAME_MAX = 128 };
#define LNTD_UNIT_NAME_MAX ((unsigned)LNTD_UNIT_NAME_MAX)

enum { LNTD_UNIT_TYPE_SOCKET, LNTD_UNIT_TYPE_SERVICE };
typedef unsigned char lntd_unit_type;

enum { LNTD_UNIT_SOCKET_TYPE_DIR,
       LNTD_UNIT_SOCKET_TYPE_FILE,
       LNTD_UNIT_SOCKET_TYPE_FIFO };
typedef unsigned char lntd_unit_socket_type;

struct lntd_unit_db;

struct lntd_unit;
struct lntd_unit_service;
struct lntd_unit_socket;

lntd_error lntd_unit_db_create(struct lntd_unit_db **unitsp);
void lntd_unit_db_destroy(struct lntd_unit_db *units);

lntd_error lntd_unit_db_add_unit(struct lntd_unit_db *units,
                                 struct lntd_unit **unitp);
size_t lntd_unit_db_size(struct lntd_unit_db *units);
struct lntd_unit *lntd_unit_db_get_unit(struct lntd_unit_db *units,
                                        size_t ii);
struct lntd_unit *
lntd_unit_db_get_unit_by_name(struct lntd_unit_db *unit,
                              char const *name);

lntd_error lntd_unit_name(lntd_proc pid,
                          char name[static LNTD_UNIT_NAME_MAX + 1U]);
lntd_error lntd_unit_pid(lntd_proc *pidp, lntd_proc manager_pid,
                         char const *name);

struct lntd_unit_socket {
	char const *path;
	int_least32_t fifo_size;
	lntd_unit_socket_type type;
};

struct lntd_unit_service {
	char const *const *command;
	char const *const *environment;

	char const *fstab;
	char const *chdir_path;

	int_least64_t timer_slack_nsec;

	int_least64_t limit_no_file;
	int_least64_t limit_msgqueue;
	int_least64_t limit_locks;
	int_least64_t limit_memlock;

	int priority;

	_Bool has_timer_slack_nsec : 1U;
	_Bool has_priority : 1U;
	_Bool has_limit_no_file : 1U;
	_Bool has_limit_msgqueue : 1U;
	_Bool has_limit_locks : 1U;
	_Bool has_limit_memlock : 1U;

	_Bool clone_newuser : 1U;
	_Bool clone_newpid : 1U;
	_Bool clone_newipc : 1U;
	_Bool clone_newnet : 1U;
	_Bool clone_newns : 1U;
	_Bool clone_newuts : 1U;

	_Bool no_new_privs : 1U;
};

struct lntd_unit {
	char *name;
	lntd_unit_type type;

	union {
		struct lntd_unit_socket socket;
		struct lntd_unit_service service;
	} lntd_unit_u;
};

#endif /* LNTD_UNIT_H */
