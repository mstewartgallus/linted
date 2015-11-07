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

enum linted_admin_proto_type {
	LINTED_ADMIN_PROTO_ADD_UNIT,
	LINTED_ADMIN_PROTO_ADD_SOCKET,
	LINTED_ADMIN_PROTO_STATUS,
	LINTED_ADMIN_PROTO_STOP };

struct linted_admin_proto_reply_add_unit {
	char dummy;
};
struct linted_admin_proto_reply_add_socket {
	char dummy;
};
struct linted_admin_proto_reply_status {
	bool is_up;
};
struct linted_admin_proto_reply_stop {
	bool was_up;
};

union linted_admin_proto_reply switch(enum linted_admin_proto_type type) {
case LINTED_ADMIN_PROTO_ADD_UNIT:
	struct linted_admin_proto_reply_add_unit add_unit;
case  LINTED_ADMIN_PROTO_ADD_SOCKET:
	struct linted_admin_proto_reply_add_socket add_socket;
case LINTED_ADMIN_PROTO_STATUS:
	struct linted_admin_proto_reply_status status;
case LINTED_ADMIN_PROTO_STOP:
	struct linted_admin_proto_reply_stop stop;
};
