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
#ifndef LINTED_START_H
#define LINTED_START_H

#include "linted/ko.h"
#include "linted/str.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct linted_start_config {
    char const * canonical_process_name;
    bool open_current_working_directory: 1;
};

extern struct linted_start_config const linted_start_config;

uint_fast8_t linted_start(linted_ko cwd, char const *const program_name,
                          size_t argc, char const *const argv[const]);

#endif /* LINTED_START_H */
