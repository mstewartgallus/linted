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
#ifndef LINTED_CONF_H
#define LINTED_CONF_H

#include "linted/error.h"
#include "linted/ko.h"

#include <stddef.h>
#include <stdint.h>

/**
 * @file
 *
 * Parses configuration data.
 *
 * @todo Abstract out configurations as abstract ids that a conf DB
 *       hands out.
 */

struct linted_conf_db;
struct linted_conf;

typedef uint_fast64_t linted_conf_section;

struct linted_conf_db;

linted_error linted_conf_db_create(struct linted_conf_db **dbp);
void linted_conf_db_destroy(struct linted_conf_db *db);

linted_error linted_conf_db_add_conf(struct linted_conf_db *db,
                                     struct linted_conf *conf);

size_t linted_conf_db_size(struct linted_conf_db *db);
struct linted_conf *linted_conf_db_get_conf(struct linted_conf_db *db,
                                            size_t ii);

linted_error linted_conf_create(struct linted_conf **confp,
                                char *file_name);

linted_error linted_conf_parse_file(struct linted_conf *conf,
                                    linted_ko dir_ko,
                                    char const *file_name);

void linted_conf_put(struct linted_conf *conf);

char const *linted_conf_peek_name(struct linted_conf *conf);

char const *const *linted_conf_find(struct linted_conf *conf,
                                    char const *section,
                                    char const *field);

linted_error linted_conf_add_section(struct linted_conf *conf,
                                     linted_conf_section *sectionp,
                                     char *section_name);

linted_error linted_conf_add_setting(struct linted_conf *conf,
                                     linted_conf_section section,
                                     char *field,
                                     char const *const *value);

#endif /* LINTED_CONF_H */
