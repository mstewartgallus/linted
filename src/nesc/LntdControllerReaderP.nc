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

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stddef.h>
#include <stdint.h>

#include "controller.h"

generic module LntdControllerReaderP()
{
	provides interface LntdControllerReader;
	uses interface LntdReader;
}
implementation
{
	nx_struct lntd_controller_input control_input;
	lntd_ko ko;

	command void LntdControllerReader.start(lntd_ko the_ko)
	{
		ko = the_ko;
		call LntdReader.read_start(the_ko,
		                           (char *)&control_input,
		                           sizeof control_input);
	}

	command void LntdControllerReader.stop(void)
	{
		call LntdReader.read_cancel();
	}

	event void LntdReader.read_done(lntd_error err,
	                                size_t bytes_read)
	{
		if (LNTD_ERROR_CANCELLED == err)
			return;

		if (err != 0) {
			signal LntdControllerReader.read_input(err, 0);
			return;
		}

		if (bytes_read != sizeof control_input) {
			signal LntdControllerReader.read_input(EPROTO,
			                                       0);
			return;
		}

		{
			struct lntd_controller_reader_input input;

			input.z_tilt = control_input.z_tilt;
			input.x_tilt = control_input.x_tilt;

			input.left = control_input.left;
			input.right = control_input.right;
			input.forward = control_input.forward;
			input.back = control_input.back;

			input.jumping = control_input.jumping;

			signal LntdControllerReader.read_input(0,
			                                       &input);
		}

		call LntdReader.execute(ko, (char *)&control_input,
		                        sizeof control_input);
	}
}
