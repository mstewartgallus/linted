/*
 * Copyright 2015,2016 Steven Stewart-Gallus
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

#include "update.h"

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stddef.h>
#include <stdint.h>

generic module LntdUpdateReaderP()
{
	provides interface LntdUpdateReader;
	uses interface LntdReader;
}
implementation
{
	nx_struct lntd_update_input control_input;
	lntd_ko ko;

	command void LntdUpdateReader.start(lntd_ko the_ko)
	{
		ko = the_ko;
		call LntdReader.read_start(the_ko,
		                           (char *)&control_input,
		                           sizeof control_input);
	}

	command void LntdUpdateReader.stop(void)
	{
		call LntdReader.read_cancel();
	}

	event void LntdReader.read_done(lntd_error err,
	                                size_t bytes_read)
	{
		if (LNTD_ERROR_CANCELLED == err)
			return;

		if (err != 0) {
			signal LntdUpdateReader.read_input(err, 0);
			return;
		}

		if (bytes_read != sizeof control_input) {
			signal LntdUpdateReader.read_input(EPROTO, 0);
			return;
		}

		{
			struct lntd_update_reader_input input;

			input.mx_position = control_input.mx_position;
			input.my_position = control_input.my_position;
			input.mz_position = control_input.mz_position;

			input.x_position = control_input.x_position;
			input.y_position = control_input.y_position;
			input.z_position = control_input.z_position;

			input.z_rotation = control_input.z_rotation;
			input.x_rotation = control_input.x_rotation;

			signal LntdUpdateReader.read_input(0, &input);
		}

		call LntdReader.read_start(ko, (char *)&control_input,
		                           sizeof control_input);
	}
}
