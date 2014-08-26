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
#ifndef LINTED_DB_H
#define LINTED_DB_H

#include "linted/error.h"
#include "linted/ko.h"

/**
 * @file
 *
 * Safely store and read shared (concurrently accessed) persistent
 * data.
 *
 * @todo Add garbage collection.
 */

#define LINTED_DB_CREAT 1UL

#define LINTED_DB_EXCL (1UL << 1U)
#define LINTED_DB_RDONLY (1UL << 2U)
#define LINTED_DB_WRONLY (1UL << 3U)
#define LINTED_DB_RDWR (1UL << 4U)

typedef linted_ko linted_db;

linted_error linted_db_open(linted_db *dbp, linted_ko cwd, char const *pathname,
                            unsigned long flags);
linted_error linted_db_close(linted_db db);

linted_error linted_db_temp_file(linted_db db, linted_ko *kop, char **pathp);
linted_error linted_db_temp_send(linted_db db, char const *path,
                                 char const *name);

#endif /* LINTED_DB_H */
