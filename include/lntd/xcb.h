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
#ifndef LNTD_XCB_H
#define LNTD_XCB_H

#include "lntd/error.h"

#include <xcb/xcb.h>

/**
 * @file
 *
 * XCB functionality.
 */

lntd_error lntd_xcb_error(xcb_generic_error_t *error);
lntd_error lntd_xcb_conn_error(xcb_connection_t *connection);

#endif /* LNTD_XCB_H */
