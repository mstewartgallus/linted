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

generic module LntdUpdateWriterP()
{
	provides interface LntdUpdateWriter;
	uses interface LntdWriter;
}
implementation
{
	nx_struct lntd_update_input update_being_written;
	struct lntd_update_writer_update pending_update;
	bool update_pending;
	bool update_in_progress;
	lntd_ko ko;

	void maybe_update(void);

	command void LntdUpdateWriter.write(
	    lntd_ko the_ko,
	    struct lntd_update_writer_update const *update)
	{
		ko = the_ko;

		update_pending = true;
		pending_update = *update;

		maybe_update();
	}

	command void LntdUpdateWriter.write_cancel(void)
	{
		update_in_progress = false;
		call LntdWriter.write_cancel();
	}

	event void LntdWriter.write_done(lntd_error err)
	{
		update_in_progress = false;

		signal LntdUpdateWriter.write_done(err);

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

		update_being_written.x_position =
		    pending_update.x_position;
		update_being_written.y_position =
		    pending_update.y_position;
		update_being_written.z_position =
		    pending_update.z_position;

		update_being_written.z_rotation =
		    pending_update.z_rotation;
		update_being_written.x_rotation =
		    pending_update.x_rotation;

		call LntdWriter.write_start(
		    ko, (char const *)&update_being_written,
		    sizeof update_being_written);
	}
}
