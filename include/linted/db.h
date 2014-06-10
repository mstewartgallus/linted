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
 *
 * @todo Use a better random number generator.
 */

#define LINTED_DB_CREAT 1

#define LINTED_DB_EXCL (1 << 1)
#define LINTED_DB_RDONLY (1 << 2)
#define LINTED_DB_WRONLY (1 << 3)
#define LINTED_DB_RDWR (1 << 4)

typedef int linted_db;

linted_error linted_db_open(linted_db *dbp, linted_ko cwd, char const *pathname,
                            int flags);
linted_error linted_db_close(linted_db *dbp);

linted_error linted_db_temp_file(linted_db *dbp, linted_ko *kop);
linted_error linted_db_temp_send(linted_db *db, char const *name, linted_ko ko);

#endif /* LINTED_DB_H */
