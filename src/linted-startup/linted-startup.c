/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/admin.h"
#include "linted/conf.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct startup {
	char const *unit_path;
	linted_ko admin_in;
	linted_ko admin_out;
};

static linted_error startup_init(struct startup *startup,
                                 char const *controller_path,
                                 char const *updater_path,
                                 char const *unit_path);

static linted_error startup_destroy(struct startup *startup);

static linted_error startup_start(struct startup *startup);
static linted_error startup_stop(struct startup *startup);

static char const *const default_envvars[] = {
    "MALLOC_CHECK_", "MALLOC_PERTURB_", "MANAGERPID", "USER", "LOGNAME",
    "HOME", "SHELL", "XDG_RUNTIME_DIR"
                     "XDG_SESSION_ID",
    "XDG_SEAT", "TERM", "LD_DEBUG", "LD_DEBUG_OUTPUT", 0};

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      linted_ko cwd, char const *path);
static linted_error add_unit_dir_to_db(struct linted_conf_db *db,
                                       linted_ko cwd,
                                       char const *dir_name);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

static linted_error str_from_strs(char const *const *strs,
                                  char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);

static linted_error socket_activate(struct linted_unit *unit,
                                    linted_ko admin_in,
                                    linted_ko admin_out);

static linted_error service_activate(struct linted_unit *unit,
                                     linted_ko admin_in,
                                     linted_ko admin_out);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-startup", 0};

static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	if (argc != 4U) {
		linted_log(LINTED_LOG_ERROR,
		           "missing some of 3 file operands");
		return EXIT_FAILURE;
	}

	char const *admin_in = argv[1U];
	char const *admin_out = argv[2U];
	char const *unit_path = argv[3U];

	static struct startup startup = {0};
	err = startup_init(&startup, admin_in, admin_out, unit_path);
	if (err != 0)
		goto log_error;

	err = startup_start(&startup);
	if (err != 0)
		goto destroy_startup;

	linted_error stop_err = startup_stop(&startup);
	if (0 == err)
		err = stop_err;

destroy_startup:
	;
	linted_error destroy_err = startup_destroy(&startup);
	if (0 == err)
		err = destroy_err;

log_error:
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error startup_init(struct startup *startup,
                                 char const *admin_in_path,
                                 char const *admin_out_path,
                                 char const *unit_path)
{
	linted_error err = 0;

	linted_admin_in admin_in;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, admin_in_path,
		                     LINTED_KO_RDWR);
		if (err != 0)
			return err;
		admin_in = xx;
	}

	linted_admin_out admin_out;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, admin_out_path,
		                     LINTED_KO_RDWR);
		if (err != 0)
			goto close_admin_in;
		admin_out = xx;
	}

	startup->admin_in = admin_in;
	startup->admin_out = admin_out;
	startup->unit_path = unit_path;

	return 0;

close_admin_in:
	linted_ko_close(admin_in);

	return err;
}

static linted_error startup_destroy(struct startup *startup)
{
	linted_ko admin_in = startup->admin_in;
	linted_ko admin_out = startup->admin_out;

	linted_ko_close(admin_in);

	linted_ko_close(admin_out);

	return 0;
}

static linted_error startup_start(struct startup *startup)
{
	char const *unit_path = startup->unit_path;
	linted_ko admin_in = startup->admin_in;
	linted_ko admin_out = startup->admin_out;

	linted_error err = 0;

	struct linted_conf_db *conf_db;
	{
		struct linted_conf_db *xx;
		err = conf_db_from_path(&xx, LINTED_KO_CWD, unit_path);
		if (err != 0)
			return err;
		conf_db = xx;
	}

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		err = linted_unit_db_create(&xx);
		if (err != 0)
			return err;
		unit_db = xx;
	}

	size_t size = linted_conf_db_size(conf_db);
	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_unit *unit;
		{
			struct linted_unit *xx;
			err = linted_unit_db_add_unit(unit_db, &xx);
			if (err != 0)
				goto destroy_unit_db;
			unit = xx;
		}

		struct linted_conf *conf =
		    linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		linted_unit_type unit_type;
		if (0 == strcmp(suffix, "socket")) {
			unit_type = LINTED_UNIT_TYPE_SOCKET;
		} else if (0 == strcmp(suffix, "service")) {
			unit_type = LINTED_UNIT_TYPE_SERVICE;
		} else {
			err = LINTED_ERROR_INVALID_PARAMETER;
			goto destroy_unit_db;
		}

		char *unit_name;
		{
			char *xx;
			err = linted_str_dup_len(&xx, file_name,
			                         dot - file_name);
			if (err != 0)
				goto destroy_unit_db;
			unit_name = xx;
		}

		unit->type = unit_type;
		unit->name = unit_name;

		switch (unit_type) {
		case LINTED_UNIT_TYPE_SERVICE: {
			struct linted_unit_service *s = (void *)unit;

			err = service_create(s, conf);
			if (err != 0)
				goto destroy_unit_db;
			break;
		}

		case LINTED_UNIT_TYPE_SOCKET: {
			struct linted_unit_socket *s = (void *)unit;

			err = socket_create(s, conf);
			if (err != 0)
				goto destroy_unit_db;
			break;
		}
		}
	}

	size_t db_size = linted_unit_db_size(unit_db);

	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct linted_unit *unit =
		    linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SOCKET)
			continue;

		err = socket_activate(unit, admin_in, admin_out);
		if (err != 0)
			goto destroy_unit_db;
	}

	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct linted_unit *unit =
		    linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		err = service_activate(unit, admin_in, admin_out);
		if (err != 0)
			goto destroy_unit_db;
	}

	return 0;

destroy_unit_db:
	linted_unit_db_destroy(unit_db);
	return err;
}

static linted_error startup_stop(struct startup *startup)
{
	return 0;
}

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      linted_ko cwd, char const *path)
{
	linted_error err = 0;

	struct linted_conf_db *db;
	{
		struct linted_conf_db *xx;
		err = linted_conf_db_create(&xx);
		if (err != 0)
			return err;
		db = xx;
	}

	char const *dirstart = path;
	for (;;) {
		char const *dirend = strchr(dirstart, ':');

		size_t len;
		if (0 == dirend) {
			len = strlen(dirstart);
		} else {
			len = dirend - dirstart;
		}

		char *dir_name;
		{
			char *xx;
			err = linted_str_dup_len(&xx, dirstart, len);
			if (err != 0)
				goto free_units;
			dir_name = xx;
		}

		err = add_unit_dir_to_db(db, cwd, dir_name);

		linted_mem_free(dir_name);

		if (err != 0)
			goto free_units;

		if (0 == dirend)
			break;

		dirstart = dirend + 1U;
	}

free_units:
	if (err != 0) {
		linted_conf_db_destroy(db);

		return err;
	}

	*dbp = db;

	return 0;
}

static linted_error add_unit_dir_to_db(struct linted_conf_db *db,
                                       linted_ko cwd,
                                       char const *dir_name)
{
	linted_error err;

	linted_ko units_ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, cwd, dir_name,
		                     LINTED_KO_DIRECTORY);
		/* Just treat as an empty directory */
		if (LINTED_ERROR_FILE_NOT_FOUND == err)
			return 0;
		if (err != 0)
			return err;
		units_ko = xx;
	}

	DIR *units_dir = fdopendir(units_ko);
	if (0 == units_dir) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(units_ko);
	}

	if (LINTED_ERROR_FILE_NOT_FOUND == err)
		return 0;

	if (err != 0)
		return err;

	size_t files_count = 0U;
	char **files = 0;
	for (;;) {
		errno = 0;
		struct dirent const *entry = readdir(units_dir);
		if (0 == entry) {
			err = errno;
			if (0 == err)
				break;

			goto free_file_names;
		}

		char const *name = entry->d_name;

		if (0 == strcmp(".", name))
			continue;

		if (0 == strcmp("..", name))
			continue;

		char *name_copy;
		{
			char *xx;
			err = linted_str_dup(&xx, name);
			if (err != 0)
				goto free_file_names;
			name_copy = xx;
		}

		size_t new_files_count = files_count + 1U;
		char **new_files;
		{
			void *xx;
			err = linted_mem_realloc_array(
			    &xx, files, new_files_count,
			    sizeof files[0U]);
			if (err != 0)
				goto free_file_name;
			new_files = xx;
		}
		new_files[files_count] = name_copy;

		files = new_files;
		files_count = new_files_count;

		if (err != 0) {
		free_file_name:
			linted_mem_free(name_copy);
			goto free_file_names;
		}
	}

	for (size_t ii = 0U; ii < files_count; ++ii) {
		char *file_name = files[ii];

		linted_unit_type unit_type;
		{
			char const *dot = strchr(file_name, '.');

			char const *suffix = dot + 1U;

			if (0 == strcmp(suffix, "socket")) {
				unit_type = LINTED_UNIT_TYPE_SOCKET;
			} else if (0 == strcmp(suffix, "service")) {
				unit_type = LINTED_UNIT_TYPE_SERVICE;
			} else {
				err = LINTED_ERROR_INVALID_PARAMETER;
				goto free_file_names;
			}
		}
		struct linted_conf *conf = 0;
		{
			struct linted_conf *xx;
			err = linted_conf_create(&xx, file_name);
			if (err != 0)
				goto close_unit_file;
			conf = xx;
		}

		files[ii] = 0;

		switch (unit_type) {
		case LINTED_UNIT_TYPE_SOCKET:
			/* Okay but we have no defaults for this */
			break;

		case LINTED_UNIT_TYPE_SERVICE: {
			char *section_name;
			{
				char *xx;
				err = linted_str_dup(&xx, "Service");
				if (err != 0)
					goto close_unit_file;
				section_name = xx;
			}

			char *env_whitelist;
			{
				char *xx;
				err = linted_str_dup(
				    &xx,
				    "X-LintedEnvironmentWhitelist");
				if (err != 0) {
					linted_mem_free(section_name);
					goto close_unit_file;
				}
				env_whitelist = xx;
			}

			linted_conf_section service;
			{
				linted_conf_section xx;
				err = linted_conf_add_section(
				    conf, &xx, section_name);
				if (err != 0) {
					linted_mem_free(env_whitelist);
					linted_mem_free(section_name);
					goto close_unit_file;
				}
				service = xx;
			}

			err = linted_conf_add_setting(conf, service,
			                              env_whitelist,
			                              default_envvars);
			if (err != 0)
				goto close_unit_file;
			break;
		}
		}

		err = linted_conf_parse_file(conf, units_ko, file_name);

	close_unit_file:
		if (err != 0)
			goto free_unit;

		err = linted_conf_db_add_conf(db, conf);

	free_unit:
		if (err != 0)
			linted_conf_put(conf);
	}

free_file_names:
	for (size_t ii = 0U; ii < files_count; ++ii)
		linted_mem_free(files[ii]);
	linted_mem_free(files);

	if (-1 == closedir(units_dir)) {
		if (0 == err) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}
	}

	return err;
}

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf)
{
	linted_error err;

	char const *const *types =
	    linted_conf_find(conf, "Service", "Type");
	char const *const *command =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *no_new_privss =
	    linted_conf_find(conf, "Service", "NoNewPrivileges");
	char const *const *chdir_paths =
	    linted_conf_find(conf, "Service", "WorkingDirectory");
	char const *const *fstabs =
	    linted_conf_find(conf, "Service", "X-LintedFstab");
	char const *const *env_whitelist = linted_conf_find(
	    conf, "Service", "X-LintedEnvironmentWhitelist");
	char const *const *clone_flags =
	    linted_conf_find(conf, "Service", "X-LintedCloneFlags");
	char const *const *priority_strs =
	    linted_conf_find(conf, "Service", "Priority");

	char const *type;
	{
		char const *xx;
		err = str_from_strs(types, &xx);
		if (err != 0)
			return err;
		type = xx;
	}

	if (0 == command)
		return LINTED_ERROR_INVALID_PARAMETER;

	char const *no_new_privs;
	{
		char const *xx;
		err = str_from_strs(no_new_privss, &xx);
		if (err != 0)
			return err;
		no_new_privs = xx;
	}

	char const *fstab;
	{
		char const *xx;
		err = str_from_strs(fstabs, &xx);
		if (err != 0)
			return err;
		fstab = xx;
	}

	char const *chdir_path;
	{
		char const *xx;
		err = str_from_strs(chdir_paths, &xx);
		if (err != 0)
			return err;
		chdir_path = xx;
	}

	char const *priority_str;
	{
		char const *xx;
		err = str_from_strs(priority_strs, &xx);
		if (err != 0)
			return err;
		priority_str = xx;
	}

	bool clone_newuser = false;
	bool clone_newpid = false;
	bool clone_newipc = false;
	bool clone_newnet = false;
	bool clone_newns = false;
	bool clone_newuts = false;
	if (clone_flags != 0) {
		for (size_t ii = 0U; clone_flags[ii] != 0; ++ii) {
			char const *flag = clone_flags[ii];
			if (0 == strcmp("CLONE_NEWUSER", flag)) {
				clone_newuser = true;
			} else if (0 == strcmp("CLONE_NEWPID", flag)) {
				clone_newpid = true;
			} else if (0 == strcmp("CLONE_NEWIPC", flag)) {
				clone_newipc = true;
			} else if (0 == strcmp("CLONE_NEWNET", flag)) {
				clone_newnet = true;
			} else if (0 == strcmp("CLONE_NEWNS", flag)) {
				clone_newns = true;
			} else if (0 == strcmp("CLONE_NEWUTS", flag)) {
				clone_newuts = true;
			} else {
				return LINTED_ERROR_INVALID_PARAMETER;
			}
		}
	}

	if (0 == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		return LINTED_ERROR_INVALID_PARAMETER;
	}

	bool no_new_privs_value = false;
	if (no_new_privs != 0) {
		bool xx;
		err = bool_from_cstring(no_new_privs, &xx);
		if (err != 0)
			return err;
		no_new_privs_value = xx;
	}

	int priority_value = -1;
	if (priority_str != 0) {
		priority_value = atoi(priority_str);
	}

	unit->command = command;
	unit->no_new_privs = no_new_privs_value;

	unit->limit_no_file = 15;
	unit->has_limit_no_file = no_new_privs_value;

	unit->limit_msgqueue = 0;
	unit->has_limit_msgqueue = no_new_privs_value;

	unit->limit_locks = 0;
	unit->has_limit_locks = no_new_privs_value;

	unit->fstab = fstab;
	unit->chdir_path = chdir_path;
	unit->env_whitelist = env_whitelist;

	unit->priority = priority_value;
	unit->has_priority = priority_str != 0;

	unit->clone_newuser = clone_newuser;
	unit->clone_newpid = clone_newpid;
	unit->clone_newipc = clone_newipc;
	unit->clone_newnet = clone_newnet;
	unit->clone_newns = clone_newns;
	unit->clone_newuts = clone_newuts;

	return 0;
}

static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf)
{
	linted_error err;

	char const *const *listen_dirs =
	    linted_conf_find(conf, "Socket", "ListenDirectory");

	char const *const *listen_files =
	    linted_conf_find(conf, "Socket", "ListenFile");

	char const *const *listen_fifos =
	    linted_conf_find(conf, "Socket", "ListenFIFO");

	char const *const *fifo_sizes =
	    linted_conf_find(conf, "Socket", "PipeSize");

	char const *listen_dir;
	{
		char const *xx;
		err = str_from_strs(listen_dirs, &xx);
		if (err != 0)
			return err;
		listen_dir = xx;
	}

	char const *listen_file;
	{
		char const *xx;
		err = str_from_strs(listen_files, &xx);
		if (err != 0)
			return err;
		listen_file = xx;
	}

	char const *listen_fifo;
	{
		char const *xx;
		err = str_from_strs(listen_fifos, &xx);
		if (err != 0)
			return err;
		listen_fifo = xx;
	}

	char const *fifo_size;
	{
		char const *xx;
		err = str_from_strs(fifo_sizes, &xx);
		if (err != 0)
			return err;
		fifo_size = xx;
	}

	linted_unit_socket_type socket_type;
	char const *path = 0;

	if (listen_dir != 0) {
		socket_type = LINTED_UNIT_SOCKET_TYPE_DIR;
		path = listen_dir;
	}

	if (listen_file != 0) {
		if (path != 0)
			return LINTED_ERROR_INVALID_PARAMETER;
		socket_type = LINTED_UNIT_SOCKET_TYPE_FILE;
		path = listen_file;
	}

	if (listen_fifo != 0) {
		if (path != 0)
			return LINTED_ERROR_INVALID_PARAMETER;
		socket_type = LINTED_UNIT_SOCKET_TYPE_FIFO;
		path = listen_fifo;
	}

	if (0 == path)
		return LINTED_ERROR_INVALID_PARAMETER;

	int fifo_size_value = -1;
	if (fifo_size != 0) {
		if (0 == listen_fifo)
			return LINTED_ERROR_INVALID_PARAMETER;
		fifo_size_value = atoi(fifo_size);
	}

	switch (socket_type) {
	case LINTED_UNIT_SOCKET_TYPE_DIR:
	case LINTED_UNIT_SOCKET_TYPE_FILE:
		break;

	case LINTED_UNIT_SOCKET_TYPE_FIFO:
		unit->fifo_size = fifo_size_value;
		break;
	}

	unit->type = socket_type;
	unit->path = path;

	return 0;
}

static linted_error str_from_strs(char const *const *strs,
                                  char const **strp)
{
	char const *str;
	if (0 == strs) {
		str = 0;
	} else {
		str = strs[0U];

		if (strs[1U] != 0)
			return LINTED_ERROR_INVALID_PARAMETER;
	}

	*strp = str;
	return 0;
}

static linted_error bool_from_cstring(char const *str, bool *boolp)
{
	static char const *const yes_strs[] = {"1", "yes", "true",
	                                       "on"};
	static char const *const no_strs[] = {"0", "no", "false",
	                                      "off"};

	bool result;

	if (0 == str)
		return LINTED_ERROR_INVALID_PARAMETER;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(yes_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = true;
			goto return_result;
		}
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(no_strs); ++ii) {
		if (0 == strcmp(str, no_strs[ii])) {
			result = false;
			goto return_result;
		}
	}

	return LINTED_ERROR_INVALID_PARAMETER;

return_result:
	*boolp = result;
	return 0;
}

static linted_error socket_activate(struct linted_unit *unit,
                                    linted_ko admin_in,
                                    linted_ko admin_out)
{
	linted_error err = 0;

	struct linted_unit_socket *unit_socket = (void *)unit;

	char const *name = unit->name;
	char const *path = unit_socket->path;
	int fifo_size = unit_socket->fifo_size;
	linted_unit_socket_type sock_type = unit_socket->type;

	{
		struct linted_admin_request request = {0};

		request.x.type = LINTED_ADMIN_ADD_SOCKET;

		request.x.add_socket.name = name;
		request.x.add_socket.path = path;
		request.x.add_socket.fifo_size = fifo_size;
		request.x.add_socket.sock_type = sock_type;

		err = linted_admin_in_send(admin_in, &request);
	}
	if (err != 0)
		return err;

	{
		union linted_admin_reply xx;
		err = linted_admin_out_recv(admin_out, &xx);
		if (err != 0)
			return err;
	}

	return 0;
}

static linted_error service_activate(struct linted_unit *unit,
                                     linted_ko admin_in,
                                     linted_ko admin_out)
{
	linted_error err = 0;

	struct linted_unit_service *unit_service = (void *)unit;

	char const *name = unit->name;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *command = unit_service->command;
	char const *const *env_whitelist = unit_service->env_whitelist;

	bool has_priority = unit_service->has_priority;
	bool has_limit_no_file = unit_service->has_limit_no_file;
	bool has_limit_msgqueue = unit_service->has_limit_msgqueue;
	bool has_limit_locks = unit_service->has_limit_locks;

	bool clone_newuser = unit_service->clone_newuser;
	bool clone_newpid = unit_service->clone_newpid;
	bool clone_newipc = unit_service->clone_newipc;
	bool clone_newnet = unit_service->clone_newnet;
	bool clone_newns = unit_service->clone_newns;
	bool clone_newuts = unit_service->clone_newuts;

	bool no_new_privs = unit_service->no_new_privs;

	if (0 == fstab)
		fstab = "";

	if (0 == chdir_path)
		chdir_path = "";

	char const *const null_list[] = {0};
	if (0 == env_whitelist)
		env_whitelist = null_list;
	{
		struct linted_admin_request request = {0};

		request.x.type = LINTED_ADMIN_ADD_UNIT;

		request.x.add_unit.has_priority = has_priority;
		request.x.add_unit.has_limit_no_file =
		    has_limit_no_file;
		request.x.add_unit.has_limit_msgqueue =
		    has_limit_msgqueue;
		request.x.add_unit.has_limit_locks = has_limit_locks;

		request.x.add_unit.clone_newuser = clone_newuser;
		request.x.add_unit.clone_newpid = clone_newpid;
		request.x.add_unit.clone_newipc = clone_newipc;
		request.x.add_unit.clone_newnet = clone_newnet;
		request.x.add_unit.clone_newns = clone_newns;
		request.x.add_unit.clone_newuts = clone_newuts;

		request.x.add_unit.no_new_privs = no_new_privs;

		request.x.add_unit.name = name;
		request.x.add_unit.fstab = fstab;
		request.x.add_unit.chdir_path = chdir_path;
		request.x.add_unit.command = command;
		request.x.add_unit.env_whitelist = env_whitelist;

		err = linted_admin_in_send(admin_in, &request);
	}
	if (err != 0)
		return err;

	{
		union linted_admin_reply xx;
		err = linted_admin_out_recv(admin_out, &xx);
		if (err != 0)
			return err;
	}

	return 0;
}
