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

#define LINTED_KO_CWD ((linted_ko) - 1)
#elif defined __linux__
typedef int linted_ko;

#define LINTED_KO_CWD (-1)
#else
#error kernel object support has not beeen implemented for this platform
#endif

#define LINTED_KO_RDONLY 1UL
#define LINTED_KO_WRONLY (1UL << 1U)
#define LINTED_KO_RDWR (1UL << 2U)

#define LINTED_KO_SYNC (1UL << 3U)

#define LINTED_KO_DIRECTORY (1UL << 4U)

struct linted_ko_task_poll;
struct linted_ko_task_read;
struct linted_ko_task_write;

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop);

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags);

/**
 * @bug Does not work for sockets. Sockets are complicated because
 *      they one can bind or connect to them. Only one socket can be
 *      bound to a socket at a time but many can connect. Checking for
 *      the socket type with fstat should work and connecting through
 *      /proc/self/fd but dealing with bind is more complicated and
 *      not something I fully understand.
 */
linted_error linted_ko_reopen(linted_ko *kooutp, linted_ko koin,
                              unsigned long flags);

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

linted_error linted_ko_task_poll_create(struct linted_ko_task_poll **taskp,
                                        void *data);
void linted_ko_task_poll_destroy(struct linted_ko_task_poll *task);

struct linted_ko_task_poll *
linted_ko_task_poll_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_ko_task_poll_to_asynch(struct linted_ko_task_poll *);
void linted_ko_task_poll_prepare(struct linted_ko_task_poll *task,
                                 unsigned task_action, linted_ko ko, int flags);
void *linted_ko_task_poll_data(struct linted_ko_task_poll *task);

linted_error linted_ko_task_write_create(struct linted_ko_task_write **taskp,
                                         void *data);
void linted_ko_task_write_destroy(struct linted_ko_task_write *task);

struct linted_ko_task_write *
linted_ko_task_write_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_ko_task_write_to_asynch(struct linted_ko_task_write *);
void linted_ko_task_write_prepare(struct linted_ko_task_write *task,
                                  unsigned task_action, linted_ko ko,
                                  char const *buf, size_t size);
void *linted_ko_task_write_data(struct linted_ko_task_write *task);

void linted_ko_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task);

/**
 * @warning Consumes pending SIGPIPEs
 */
void linted_ko_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *task);

#endif /* LINTED_KO_H */
