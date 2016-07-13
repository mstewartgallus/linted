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

#include "controller.h"

#include <stddef.h>
#include <stdint.h>

generic module LntdControllerWriterP()
{
	provides interface LntdControllerWriter;
	uses interface LntdWriter;
}
implementation
{
	nx_struct lntd_controller_input update_being_written;
	struct lntd_controller_writer_input pending_update;
	bool update_pending;
	bool update_in_progress;
	lntd_ko ko;

	void maybe_update(void);

	command void LntdControllerWriter.write(
	    lntd_ko the_ko,
	    struct lntd_controller_writer_input const *update)
	{
		ko = the_ko;

		update_pending = true;
		pending_update = *update;

		maybe_update();
	}

	command void LntdControllerWriter.cancel(void)
	{
		update_in_progress = false;
		call LntdWriter.write_cancel();
	}

	event void LntdWriter.write_done(lntd_error err)
	{
		update_in_progress = false;

		signal LntdControllerWriter.write_done(err);

		maybe_update();
	}

	void maybe_update(void)
	{
		if (!update_pending)
			return;

		if (update_in_progress)
			return;

		update_in_progress = true;
		update_pending = false;

		update_being_written.z_tilt = pending_update.z_tilt;
		update_being_written.x_tilt = pending_update.x_tilt;

		update_being_written.left = pending_update.left;
		update_being_written.right = pending_update.right;
		update_being_written.forward = pending_update.forward;
		update_being_written.back = pending_update.back;

		update_being_written.jumping = pending_update.jumping;

		call LntdWriter.write_start(
		    ko, (char const *)&update_being_written,
		    sizeof update_being_written);
	}
}
