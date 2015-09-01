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

#include "sim.h"

#include "linted/test.h"

#include <stdlib.h>

/*
 * SIM_UINT_MAX / 4U
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
		sim_angle zero = SIM_ANGLE(0U, 4U);
		sim_int sin_zero = sim_sin(zero);
		if (sin_zero != 0) {
			LINTED_TEST_FAILURE("sim_sin(zero)"
			                    " == %" SIM_Id "\n",
			                    sin_zero);
		}

		sim_angle quarter = SIM_ANGLE(1U, 4U);
		sim_int sin_quarter = sim_sin(quarter);
		if (sin_quarter != SIM_INT_MAX) {
			LINTED_TEST_FAILURE("sim_sin(quarter) == "
			                    "%" SIM_Id "\n",
			                    sin_quarter);
		}

		/* TODO This isn't totally correct */
		sim_angle half = SIM_ANGLE(1U, 2U);
		sim_int sin_half = sim_sin(half);
		if (sin_half != 0) {
			LINTED_TEST_FAILURE("sim_sin(half)"
			                    " == %" SIM_Id "\n",
			                    sin_half);
		}

		sim_angle three_quarters = SIM_ANGLE(3U, 4U);
		sim_int sin_three_quarters = sim_sin(three_quarters);
		if (sin_three_quarters != -SIM_INT_MAX) {
			LINTED_TEST_FAILURE("sim_sin(three_"
			                    "quarters) == %" SIM_Id
			                    "\n",
			                    sin_three_quarters);
		}

		sim_angle full = SIM_ANGLE(1U, 1U);
		sim_int sin_full = sim_sin(full);
		if (sin_full != 0) {
			LINTED_TEST_FAILURE("sim_sin(full)"
			                    " == %" SIM_Id "\n",
			                    sin_full);
		}
	}

	{
		sim_angle zero = SIM_ANGLE(0U, 4U);
		sim_int cos_zero = sim_cos(zero);
		if (cos_zero != SIM_INT_MAX) {
			LINTED_TEST_FAILURE("sim_cos(zero)"
			                    " == %" SIM_Id "\n",
			                    cos_zero);
		}

		/* TODO This isn't totally correct */
		sim_angle quarter = SIM_ANGLE(1U, 4U);
		sim_int cos_quarter = sim_cos(quarter);
		if (cos_quarter != 0) {
			LINTED_TEST_FAILURE("sim_cos(quarter) == "
			                    "%" SIM_Id "\n",
			                    cos_quarter);
		}

		sim_angle half = SIM_ANGLE(1U, 2U);
		sim_int cos_half = sim_cos(half);
		if (cos_half != -SIM_INT_MAX) {
			LINTED_TEST_FAILURE("sim_cos(half)"
			                    " == %" SIM_Id "\n",
			                    cos_half);
		}

		/* TODO This isn't totally correct */
		sim_angle three_quarters = SIM_ANGLE(3U, 4U);
		sim_int cos_three_quarters = sim_cos(three_quarters);
		if (cos_three_quarters != 0) {
			LINTED_TEST_FAILURE("sim_cos(three_"
			                    "quarters) == %" SIM_Id
			                    "\n",
			                    cos_three_quarters);
		}

		sim_angle full = SIM_ANGLE(1U, 1U);
		sim_int cos_full = sim_cos(full);
		if (cos_full != SIM_INT_MAX) {
			LINTED_TEST_FAILURE("sim_cos(full)"
			                    " == %" SIM_Id "\n",
			                    cos_full);
		}
	}

	return EXIT_SUCCESS;
}
