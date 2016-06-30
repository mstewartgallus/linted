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

enum lntd_admin_proto_type {
	LNTD_ADMIN_PROTO_ADD_UNIT,
	LNTD_ADMIN_PROTO_ADD_SOCKET,
	LNTD_ADMIN_PROTO_STATUS,
	LNTD_ADMIN_PROTO_STOP };

typedef string lntd_admin_proto_string<255>;

struct lntd_admin_proto_request_add_unit {
	lntd_admin_proto_string name;
	lntd_admin_proto_string fstab;
	lntd_admin_proto_string chdir_path;

	lntd_admin_proto_string command<>;
	lntd_admin_proto_string environment<>;

	hyper *timer_slack_nsec;
	hyper *priority;
	hyper *limit_no_file;
	hyper *limit_msgqueue;
	hyper *limit_locks;
	hyper *limit_memlock;

	bool clone_newcgroup;
	bool clone_newuser;
	bool clone_newpid;
	bool clone_newipc;
	bool clone_newnet;
	bool clone_newns;
	bool clone_newuts;

	bool no_new_privs;
};
struct lntd_admin_proto_request_add_socket {
	lntd_admin_proto_string name;
	lntd_admin_proto_string path;
	int fifo_size;
	int sock_type;
};
struct lntd_admin_proto_request_status {
	lntd_admin_proto_string name;
};
struct lntd_admin_proto_request_stop {
	lntd_admin_proto_string name;
};

union lntd_admin_proto_request switch(enum lntd_admin_proto_type type) {
case LNTD_ADMIN_PROTO_ADD_UNIT:
	struct lntd_admin_proto_request_add_unit add_unit;
case  LNTD_ADMIN_PROTO_ADD_SOCKET:
	struct lntd_admin_proto_request_add_socket add_socket;
case LNTD_ADMIN_PROTO_STATUS:
	struct lntd_admin_proto_request_status status;
case LNTD_ADMIN_PROTO_STOP:
	struct lntd_admin_proto_request_stop stop;
};


struct lntd_admin_proto_reply_add_unit {
	char dummy;
};
struct lntd_admin_proto_reply_add_socket {
	char dummy;
};
struct lntd_admin_proto_reply_status {
	bool is_up;
};
struct lntd_admin_proto_reply_stop {
	bool was_up;
};

union lntd_admin_proto_reply switch(enum lntd_admin_proto_type type) {
case LNTD_ADMIN_PROTO_ADD_UNIT:
	struct lntd_admin_proto_reply_add_unit add_unit;
case  LNTD_ADMIN_PROTO_ADD_SOCKET:
	struct lntd_admin_proto_reply_add_socket add_socket;
case LNTD_ADMIN_PROTO_STATUS:
	struct lntd_admin_proto_reply_status status;
case LNTD_ADMIN_PROTO_STOP:
	struct lntd_admin_proto_reply_stop stop;
};
