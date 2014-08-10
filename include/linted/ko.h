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
#ifndef LINTED_KO_H
#define LINTED_KO_H

#include "linted/asynch.h"
#include "linted/error.h"

/**
 * @file
 *
 * Abstracts over the concept of a kernel object.
 */

#if defined _WIN32 || defined _WIN64
typedef void *linted_ko;
#elif defined __linux__
typedef int linted_ko;
#else
#error no known most primitive platform kernel object type
#endif

#define LINTED_KO_RDONLY 1
#define LINTED_KO_WRONLY (1 << 2)
#define LINTED_KO_RDWR (1 << 3)

#define LINTED_KO_SYNC (1 << 4)

struct linted_ko_task_accept
{
    struct linted_asynch_task parent;
    linted_ko ko;
    linted_ko returned_ko;
};

struct linted_ko_task_poll
{
    struct linted_asynch_task parent;
    linted_ko ko;
    short events;
    short revents;
};

struct linted_ko_task_read
{
    struct linted_asynch_task parent;
    char *buf;
    size_t size;
    size_t current_position;
    size_t bytes_read;
    linted_ko ko;
};

struct linted_ko_task_write
{
    struct linted_asynch_task parent;
    char const *buf;
    size_t size;
    size_t current_position;
    size_t bytes_wrote;
    linted_ko ko;
};

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop);

linted_error linted_ko_dummy(linted_ko * restrict kop);

linted_error linted_ko_open(linted_ko * restrict kop, linted_ko dirko,
                            char const *pathname, int flags);

linted_error linted_ko_reopen(linted_ko * restrict kooutp,
                              linted_ko koin, int flags);

/**
 * The linted_ko_close function closes a kernel object.
 *
 * @param ko The kernel object to close.
 *
 * @returns Zero on success or an error code.
 *
 * @error EIO I/O error.
 *
 * @error EBADF Not a valid file descriptor.
 */
linted_error linted_ko_close(linted_ko ko);

void linted_ko_task_poll(struct linted_ko_task_poll *task, unsigned task_action,
                         linted_ko ko, short events);

void linted_ko_task_read(struct linted_ko_task_read *task, unsigned task_action,
                         linted_ko ko, char *buf, size_t size);

void linted_ko_task_write(struct linted_ko_task_write *task,
                          unsigned task_action, linted_ko ko, char const *buf,
                          size_t size);

void linted_ko_task_accept(struct linted_ko_task_accept *task,
                           unsigned task_action, linted_ko ko);

#endif /* LINTED_KO_H */
