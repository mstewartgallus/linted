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
#ifndef LINTED_START_H
#define LINTED_START_H

#include <stddef.h>

/**
 * @file
 *
 * Implements common startup functionality.
 */

struct linted_start_config {
	char const *canonical_process_name;
	unsigned char (*start)(char const *process_name, size_t argc,
	                       char const *const argv[]);
	_Bool dont_handle_signals : 1U;
};

extern struct linted_start_config const linted_start_config;

#if defined HAVE_WINDOWS_API
int linted_start_show_command(void);
#endif

extern char const linted_start__useme;

#ifndef LINTED_START_OBJECT
void const *const linted_start__user = &linted_start__useme;
#endif

#endif /* LINTED_START_H */
