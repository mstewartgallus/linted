/*
 * Copyright 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "config.h"

#include "linted/xcb.h"
#include "linted/util.h"

#include <errno.h>

#include <xcb/xcb.h>
#include <X11/Xlib.h>

linted_error linted_xcb_conn_error(xcb_connection_t *connection)
{
	switch (xcb_connection_has_error(connection)) {
	case 0:
		return 0;

	case XCB_CONN_ERROR:
		return EPROTO;

	case XCB_CONN_CLOSED_EXT_NOTSUPPORTED:
		return ENOSYS;

	case XCB_CONN_CLOSED_MEM_INSUFFICIENT:
		return ENOMEM;

	case XCB_CONN_CLOSED_REQ_LEN_EXCEED:
		return EINVAL;

	case XCB_CONN_CLOSED_PARSE_ERR:
		return EINVAL;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

linted_error linted_xcb_error(xcb_generic_error_t *error)
{
	switch (error->error_code) {
	case Success:
		return 0;

	case BadRequest:
	case BadValue:
	case BadWindow:
	case BadPixmap:
	case BadAtom:
	case BadCursor:
	case BadFont:
	case BadMatch:
	case BadDrawable:
	case BadColor:
	case BadGC:
	case BadIDChoice:
	case BadName:
	case BadLength:
		return EINVAL;

	case BadAccess:
		return EPERM;

	case BadAlloc:
		return ENOMEM;

	case BadImplementation:
		return ENOSYS;

	default:
		return ENOSYS;
	}
}
