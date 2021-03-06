/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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

#ifndef LNTD_TEST_H
#define LNTD_TEST_H

#include "lntd/io.h"
#include "lntd/ko.h"

/**
 * @file
 *
 * Implements common test functionality.
 */

#define LNTD_TEST_FAILURE(format_string, ...)                          \
	do {                                                           \
		extern void abort(void);                               \
		lntd_io_write_format(LNTD_KO_STDERR, 0, "\
impossible error in function %s, and line %i: " format_string,         \
		                     __func__, __LINE__, __VA_ARGS__); \
		abort();                                               \
	} while (0)

#endif /* LNTD_TEST_H */
