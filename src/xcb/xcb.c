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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#include "config.h"

#include "linted/xcb.h"

#include "linted/error.h"
#include "linted/util.h"

#include <errno.h>
#include <xcb/xcb.h>

#define Success 0
#define BadRequest 1
#define BadValue 2
#define BadWindow 3
#define BadPixmap 4
#define BadAtom 5
#define BadCursor 6
#define BadFont 7
#define BadMatch 8
#define BadDrawable 9
#define BadAccess 10
#define BadAlloc 11
#define BadColor 12
#define BadGC 13
#define BadIDChoice 14
#define BadName 15
#define BadLength 16
#define BadImplementation 17

linted_error linted_xcb_conn_error(xcb_connection_t *connection)
{
	switch (xcb_connection_has_error(connection)) {
	case 0:
		return 0;

	case XCB_CONN_ERROR:
		return EPROTO;

	case XCB_CONN_CLOSED_EXT_NOTSUPPORTED:
		return LINTED_ERROR_UNIMPLEMENTED;

	case XCB_CONN_CLOSED_MEM_INSUFFICIENT:
		return LINTED_ERROR_OUT_OF_MEMORY;

	case XCB_CONN_CLOSED_REQ_LEN_EXCEED:
		return LINTED_ERROR_INVALID_PARAMETER;

	case XCB_CONN_CLOSED_PARSE_ERR:
		return LINTED_ERROR_INVALID_PARAMETER;

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
		return LINTED_ERROR_INVALID_PARAMETER;

	case BadAccess:
		return LINTED_ERROR_PERMISSION;

	case BadAlloc:
		return LINTED_ERROR_OUT_OF_MEMORY;

	case BadImplementation:
		return LINTED_ERROR_UNIMPLEMENTED;

	default:
		return LINTED_ERROR_UNIMPLEMENTED;
	}
}
