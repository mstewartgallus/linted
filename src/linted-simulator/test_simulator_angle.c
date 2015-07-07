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
 * LINTED_SIM_UINT_MAX / 4U
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
		linted_sim_angle zero = LINTED_SIM_ANGLE(0U, 4U);
		linted_sim_int sin_zero = linted_sim_sin(zero);
		if (sin_zero != 0) {
			LINTED_TEST_FAILURE("linted_sim_sin(zero)"
			                    " == %" LINTED_SIM_Id "\n",
			                    sin_zero);
		}

		linted_sim_angle quarter = LINTED_SIM_ANGLE(1U, 4U);
		linted_sim_int sin_quarter = linted_sim_sin(quarter);
		if (sin_quarter != LINTED_SIM_INT_MAX) {
			LINTED_TEST_FAILURE(
			    "linted_sim_sin(quarter) == "
			    "%" LINTED_SIM_Id "\n",
			    sin_quarter);
		}

		/* TODO This isn't totally correct */
		linted_sim_angle half = LINTED_SIM_ANGLE(1U, 2U);
		linted_sim_int sin_half = linted_sim_sin(half);
		if (sin_half != 0) {
			LINTED_TEST_FAILURE("linted_sim_sin(half)"
			                    " == %" LINTED_SIM_Id "\n",
			                    sin_half);
		}

		linted_sim_angle three_quarters =
		    LINTED_SIM_ANGLE(3U, 4U);
		linted_sim_int sin_three_quarters =
		    linted_sim_sin(three_quarters);
		if (sin_three_quarters != -LINTED_SIM_INT_MAX) {
			LINTED_TEST_FAILURE(
			    "linted_sim_sin(three_"
			    "quarters) == %" LINTED_SIM_Id "\n",
			    sin_three_quarters);
		}

		linted_sim_angle full = LINTED_SIM_ANGLE(1U, 1U);
		linted_sim_int sin_full = linted_sim_sin(full);
		if (sin_full != 0) {
			LINTED_TEST_FAILURE("linted_sim_sin(full)"
			                    " == %" LINTED_SIM_Id "\n",
			                    sin_full);
		}
	}

	{
		linted_sim_angle zero = LINTED_SIM_ANGLE(0U, 4U);
		linted_sim_int cos_zero = linted_sim_cos(zero);
		if (cos_zero != LINTED_SIM_INT_MAX) {
			LINTED_TEST_FAILURE("linted_sim_cos(zero)"
			                    " == %" LINTED_SIM_Id "\n",
			                    cos_zero);
		}

		/* TODO This isn't totally correct */
		linted_sim_angle quarter = LINTED_SIM_ANGLE(1U, 4U);
		linted_sim_int cos_quarter = linted_sim_cos(quarter);
		if (cos_quarter != 0) {
			LINTED_TEST_FAILURE(
			    "linted_sim_cos(quarter) == "
			    "%" LINTED_SIM_Id "\n",
			    cos_quarter);
		}

		linted_sim_angle half = LINTED_SIM_ANGLE(1U, 2U);
		linted_sim_int cos_half = linted_sim_cos(half);
		if (cos_half != -LINTED_SIM_INT_MAX) {
			LINTED_TEST_FAILURE("linted_sim_cos(half)"
			                    " == %" LINTED_SIM_Id "\n",
			                    cos_half);
		}

		/* TODO This isn't totally correct */
		linted_sim_angle three_quarters =
		    LINTED_SIM_ANGLE(3U, 4U);
		linted_sim_int cos_three_quarters =
		    linted_sim_cos(three_quarters);
		if (cos_three_quarters != 0) {
			LINTED_TEST_FAILURE(
			    "linted_sim_cos(three_"
			    "quarters) == %" LINTED_SIM_Id "\n",
			    cos_three_quarters);
		}

		linted_sim_angle full = LINTED_SIM_ANGLE(1U, 1U);
		linted_sim_int cos_full = linted_sim_cos(full);
		if (cos_full != LINTED_SIM_INT_MAX) {
			LINTED_TEST_FAILURE("linted_sim_cos(full)"
			                    " == %" LINTED_SIM_Id "\n",
			                    cos_full);
		}
	}

	return EXIT_SUCCESS;
}
