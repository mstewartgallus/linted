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
#include "config.h"

#include "simulator.h"

#include "linted/test.h"
#include "linted/util.h"

#include <stdlib.h>

/*
 * LINTED_SIMULATOR_UINT_MAX / 4U
 * / \
 *  |
 *  |
 *  |
 *  |
 *  |------------> 0U
 *
 *
 * |   -|-   |    |    |
 * | /  |  \ |    |    |
 * |/   |   \|    |    |
 * |----|----|----|-----
 * |    |    |\   |   /|
 * |    |    | \  |  / |
 * |    |    |   -|-   |
 */

int main(void)
{
	{
		linted_simulator_angle zero =
		    LINTED_SIMULATOR_ANGLE(0U, 4U);
		linted_simulator_int sin_zero =
		    linted_simulator_sin(zero);
		if (sin_zero != 0) {
			LINTED_TEST_FAILURE("linted_simulator_sin(zero)"
			                    " == %" LINTED_SIMULATOR_Id
			                    "\n",
			                    sin_zero);
		}

		linted_simulator_angle quarter =
		    LINTED_SIMULATOR_ANGLE(1U, 4U);
		linted_simulator_int sin_quarter =
		    linted_simulator_sin(quarter);
		if (sin_quarter != LINTED_SIMULATOR_INT_MAX) {
			LINTED_TEST_FAILURE(
			    "linted_simulator_sin(quarter) == "
			    "%" LINTED_SIMULATOR_Id "\n",
			    sin_quarter);
		}

		/* TODO This isn't totally correct */
		linted_simulator_angle half =
		    LINTED_SIMULATOR_ANGLE(1U, 2U);
		linted_simulator_int sin_half =
		    linted_simulator_sin(half);
		if (sin_half != 0) {
			LINTED_TEST_FAILURE("linted_simulator_sin(half)"
			                    " == %" LINTED_SIMULATOR_Id
			                    "\n",
			                    sin_half);
		}

		linted_simulator_angle three_quarters =
		    LINTED_SIMULATOR_ANGLE(3U, 4U);
		linted_simulator_int sin_three_quarters =
		    linted_simulator_sin(three_quarters);
		if (sin_three_quarters != -LINTED_SIMULATOR_INT_MAX) {
			LINTED_TEST_FAILURE(
			    "linted_simulator_sin(three_"
			    "quarters) == %" LINTED_SIMULATOR_Id "\n",
			    sin_three_quarters);
		}

		linted_simulator_angle full =
		    LINTED_SIMULATOR_ANGLE(1U, 1U);
		linted_simulator_int sin_full =
		    linted_simulator_sin(full);
		if (sin_full != 0) {
			LINTED_TEST_FAILURE("linted_simulator_sin(full)"
			                    " == %" LINTED_SIMULATOR_Id
			                    "\n",
			                    sin_full);
		}
	}

	{
		linted_simulator_angle zero =
		    LINTED_SIMULATOR_ANGLE(0U, 4U);
		linted_simulator_int cos_zero =
		    linted_simulator_cos(zero);
		if (cos_zero != LINTED_SIMULATOR_INT_MAX) {
			LINTED_TEST_FAILURE("linted_simulator_cos(zero)"
			                    " == %" LINTED_SIMULATOR_Id
			                    "\n",
			                    cos_zero);
		}

		/* TODO This isn't totally correct */
		linted_simulator_angle quarter =
		    LINTED_SIMULATOR_ANGLE(1U, 4U);
		linted_simulator_int cos_quarter =
		    linted_simulator_cos(quarter);
		if (cos_quarter != 0) {
			LINTED_TEST_FAILURE(
			    "linted_simulator_cos(quarter) == "
			    "%" LINTED_SIMULATOR_Id "\n",
			    cos_quarter);
		}

		linted_simulator_angle half =
		    LINTED_SIMULATOR_ANGLE(1U, 2U);
		linted_simulator_int cos_half =
		    linted_simulator_cos(half);
		if (cos_half != -LINTED_SIMULATOR_INT_MAX) {
			LINTED_TEST_FAILURE("linted_simulator_cos(half)"
			                    " == %" LINTED_SIMULATOR_Id
			                    "\n",
			                    cos_half);
		}

		/* TODO This isn't totally correct */
		linted_simulator_angle three_quarters =
		    LINTED_SIMULATOR_ANGLE(3U, 4U);
		linted_simulator_int cos_three_quarters =
		    linted_simulator_cos(three_quarters);
		if (cos_three_quarters != 0) {
			LINTED_TEST_FAILURE(
			    "linted_simulator_cos(three_"
			    "quarters) == %" LINTED_SIMULATOR_Id "\n",
			    cos_three_quarters);
		}

		linted_simulator_angle full =
		    LINTED_SIMULATOR_ANGLE(1U, 1U);
		linted_simulator_int cos_full =
		    linted_simulator_cos(full);
		if (cos_full != LINTED_SIMULATOR_INT_MAX) {
			LINTED_TEST_FAILURE("linted_simulator_cos(full)"
			                    " == %" LINTED_SIMULATOR_Id
			                    "\n",
			                    cos_full);
		}
	}

	return EXIT_SUCCESS;
}
