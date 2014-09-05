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
#ifndef LINTED_CONF_H
#define LINTED_CONF_H

#include "linted/error.h"

struct conf;
struct conf_section;
struct conf_setting;

linted_error conf_create(struct conf **unitp, char const *file_name);

void conf_put(struct conf *unit);

char const *conf_peek_name(struct conf *unit);

char const *const *conf_find(struct conf *unit, char const *section,
                             char const *field);

linted_error conf_add_section(struct conf *unit, struct conf_section **sectionp,
                              char *section_name);

linted_error conf_add_setting(struct conf_section *section, char *field,
                              char const *const *value);

#endif /* LINTED_CONF_H */
