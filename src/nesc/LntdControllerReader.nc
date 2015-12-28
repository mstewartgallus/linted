/*
 * Copyright 2015 Steven Stewart-Gallus
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
#include "config.h"

#include "bool.h"

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stddef.h>
#include <stdint.h>

struct lntd_controller_reader_input {
	int_least32_t z_tilt;
	int_least32_t x_tilt;

	unsigned left : 1U;
	unsigned right : 1U;
	unsigned forward : 1U;
	unsigned back : 1U;

	unsigned jumping : 1U;
};

interface LntdControllerReader
{
	command void start(lntd_ko ko);
	command void stop(void);

	event void read_input(
	    lntd_error err,
	    struct lntd_controller_reader_input const *input);
}
