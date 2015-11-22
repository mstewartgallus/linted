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
#include "linted/env.h"
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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum { LINTED_SYSTEM_CONF_PATH, LINTED_UNIT_PATH };

static char const *const required_envs[] =
    {[LINTED_SYSTEM_CONF_PATH] = "LINTED_SYSTEM_CONF_PATH",
     [LINTED_UNIT_PATH] = "LINTED_UNIT_PATH"};

struct startup {
	char const *system_conf_path;
	char const *unit_path;
	linted_ko admin_in;
	linted_ko admin_out;
};

struct system_conf {
	int_least64_t limit_locks;
	int_least64_t limit_msgqueue;
	int_least64_t limit_no_file;

	bool has_limit_locks : 1U;
	bool has_limit_msgqueue : 1U;
	bool has_limit_no_file : 1U;
};

static linted_error startup_init(struct startup *startup,
                                 char const *controller_path,
                                 char const *updater_path,
                                 char const *system_conf_path,
                                 char const *unit_path);

static linted_error startup_destroy(struct startup *startup);

static linted_error startup_start(struct startup *startup);
static linted_error startup_stop(struct startup *startup);

static char const *const default_envvars[] = {
    "MALLOC_CHECK_", "MALLOC_PERTURB_", "MANAGERPID", "USER", "LOGNAME",
    "HOME", "SHELL", "XDG_RUNTIME_DIR"
                     "XDG_SESSION_ID",
    "XDG_SEAT", "TERM", "LD_WARN", "LD_VERBOSE", "LD_DEBUG",
    "LD_DEBUG_OUTPUT", 0};

static linted_error populate_conf_db(struct linted_conf_db *conf_db,
                                     linted_ko cwd,
                                     char const *system_conf_path,
                                     char const *unit_path);
static linted_error populate_system_conf(struct linted_conf_db *db,
                                         linted_ko cwd,
                                         char const *file_name);
static linted_error add_unit_dir_to_db(struct linted_conf_db *db,
                                       linted_ko cwd,
                                       char const *dir_name);

static linted_error socket_activate(struct linted_conf *conf,
                                    linted_ko admin_in,
                                    linted_ko admin_out);

static linted_error
service_activate(struct system_conf const *system_conf,
                 struct linted_conf *conf, linted_ko admin_in,
                 linted_ko admin_out);

static size_t null_list_size(char const *const *list);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-startup", 0};

static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	if (argc != 3U) {
		linted_log(LINTED_LOG_ERROR,
		           "missing some of 2 file operands");
		return EXIT_FAILURE;
	}

	char const *system_conf_path;
	char const *unit_path;
	{
		char *envs[LINTED_ARRAY_SIZE(required_envs)];
		for (size_t ii = 0U;
		     ii < LINTED_ARRAY_SIZE(required_envs); ++ii) {
			char const *req = required_envs[ii];
			char *value;
			{
				char *xx;
				err = linted_env_get(req, &xx);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_env_get: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				value = xx;
			}
			if (0 == value) {
				linted_log(LINTED_LOG_ERROR,
				           "%s is a required "
				           "environment variable",
				           req);
				return EXIT_FAILURE;
			}
			envs[ii] = value;
		}
		system_conf_path = envs[LINTED_SYSTEM_CONF_PATH];
		unit_path = envs[LINTED_UNIT_PATH];
	}

	char const *admin_in = argv[1U];
	char const *admin_out = argv[2U];

	static struct startup startup = {0};
	err = startup_init(&startup, admin_in, admin_out,
	                   system_conf_path, unit_path);
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
                                 char const *system_conf_path,
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
	startup->system_conf_path = system_conf_path;
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
	char const *system_conf_path = startup->system_conf_path;
	char const *unit_path = startup->unit_path;
	linted_ko admin_in = startup->admin_in;
	linted_ko admin_out = startup->admin_out;

	linted_error err = 0;

	struct linted_conf_db *conf_db;
	{
		struct linted_conf_db *xx;
		err = linted_conf_db_create(&xx);
		if (err != 0)
			return err;
		conf_db = xx;
	}

	err = populate_conf_db(conf_db, LINTED_KO_CWD, system_conf_path,
	                       unit_path);
	if (err != 0)
		goto destroy_conf_db;

	size_t size = linted_conf_db_size(conf_db);
	struct linted_conf *system_conf = 0;
	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_conf *conf =
		    linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		if (0 == strcmp("system.conf", file_name)) {
			system_conf = conf;
			break;
		}
	}
	if (0 == system_conf)
		return LINTED_ERROR_FILE_NOT_FOUND;

	struct system_conf system_conf_struct = {0};

	{
		int xx;
		bool yy;
		err = linted_conf_find_int(system_conf, "Manager",
		                           "DefaultLimitLOCKS", &xx);
		if (0 == err) {
			yy = true;
		} else if (ENOENT == err) {
			yy = false;
		} else {
			goto destroy_conf_db;
		}
		system_conf_struct.limit_locks = xx;
		system_conf_struct.has_limit_locks = yy;
	}
	{
		int xx;
		bool yy;
		err = linted_conf_find_int(system_conf, "Manager",
		                           "DefaultLimitMSGQUEUE", &xx);
		if (0 == err) {
			yy = true;
		} else if (ENOENT == err) {
			yy = false;
		} else {
			goto destroy_conf_db;
		}
		system_conf_struct.limit_msgqueue = xx;
		system_conf_struct.has_limit_msgqueue = yy;
	}
	{
		int xx;
		bool yy;
		err = linted_conf_find_int(system_conf, "Manager",
		                           "DefaultLimitNOFILE", &xx);
		if (0 == err) {
			yy = true;
		} else if (ENOENT == err) {
			yy = false;
		} else {
			goto destroy_conf_db;
		}
		system_conf_struct.limit_no_file = xx;
		system_conf_struct.has_limit_no_file = yy;
	}

	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_conf *conf =
		    linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		if (strcmp(suffix, "socket") != 0)
			continue;

		err = socket_activate(conf, admin_in, admin_out);
		if (err != 0)
			goto destroy_conf_db;
	}
	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_conf *conf =
		    linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		if (strcmp(suffix, "service") != 0)
			continue;

		err = service_activate(&system_conf_struct, conf,
		                       admin_in, admin_out);
		if (err != 0)
			goto destroy_conf_db;
	}

	return 0;

destroy_conf_db:
	linted_conf_db_destroy(conf_db);

	return err;
}

static linted_error startup_stop(struct startup *startup)
{
	return 0;
}

static linted_error populate_conf_db(struct linted_conf_db *db,
                                     linted_ko cwd,
                                     char const *system_conf_path,
                                     char const *unit_path)
{
	linted_error err = 0;

	for (char const *start = system_conf_path;;) {
		char const *end = strchr(start, ':');

		size_t len;
		if (0 == end) {
			len = strlen(start);
		} else {
			len = end - start;
		}

		char *file_name_dup;
		{
			char *xx;
			err = linted_str_dup_len(&xx, start, len);
			if (err != 0)
				return err;
			file_name_dup = xx;
		}

		err = populate_system_conf(db, cwd, file_name_dup);

		linted_mem_free(file_name_dup);

		if (err != 0)
			return err;

		if (0 == end)
			break;

		start = end + 1U;
	}

	for (char const *dirstart = unit_path;;) {
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
				return err;
			dir_name = xx;
		}

		err = add_unit_dir_to_db(db, cwd, dir_name);

		linted_mem_free(dir_name);

		if (err != 0)
			return err;

		if (0 == dirend)
			break;

		dirstart = dirend + 1U;
	}

	return 0;
}

static linted_error populate_system_conf(struct linted_conf_db *db,
                                         linted_ko cwd,
                                         char const *filename)
{
	linted_error err;

	linted_ko conf_ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, cwd, filename,
		                     LINTED_KO_RDONLY);
		/* Just treat as an empty file */
		if (LINTED_ERROR_FILE_NOT_FOUND == err)
			return 0;
		if (err != 0)
			return err;
		conf_ko = xx;
	}

	char *filename_dup;
	{
		char *xx;
		err = linted_str_dup(&xx, "system.conf");
		if (err != 0)
			goto close_ko;
		filename_dup = xx;
	}

	struct linted_conf *conf = 0;
	{
		struct linted_conf *xx;
		err = linted_conf_create(&xx, filename_dup);
		if (err != 0) {
			linted_mem_free(filename_dup);
			goto close_ko;
		}
		conf = xx;
	}

	err = linted_conf_parse_file(conf, conf_ko, filename);
	if (err != 0)
		goto free_conf;

	err = linted_conf_db_add_conf(db, conf);

free_conf:
	if (err != 0)
		linted_conf_put(conf);

close_ko:
	linted_ko_close(conf_ko);

	return err;
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

static linted_error socket_activate(struct linted_conf *conf,
                                    linted_ko admin_in,
                                    linted_ko admin_out)
{
	linted_error err = 0;

	char const *file_name = linted_conf_peek_name(conf);

	char const *dot = strchr(file_name, '.');

	char *unit_name;
	{
		char *xx;
		err =
		    linted_str_dup_len(&xx, file_name, dot - file_name);
		if (err != 0)
			return err;
		unit_name = xx;
	}

	char const *listen_dir;
	{
		char const *xx = 0;
		err = linted_conf_find_str(conf, "Socket",
		                           "ListenDirectory", &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		listen_dir = xx;
	}

	char const *listen_file;
	{
		char const *xx = 0;
		err = linted_conf_find_str(conf, "Socket", "ListenFile",
		                           &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		listen_file = xx;
	}

	char const *listen_fifo;
	{
		char const *xx = 0;
		err = linted_conf_find_str(conf, "Socket", "ListenFIFO",
		                           &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		listen_fifo = xx;
	}

	int fifo_size;
	bool have_fifo_size;
	{
		int xx = -1;
		err = linted_conf_find_int(conf, "Socket", "PipeSize",
		                           &xx);
		if (0 == err) {
			have_fifo_size = true;
		} else if (ENOENT == err) {
			have_fifo_size = false;
		} else {
			goto free_unit_name;
		}
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

	if (have_fifo_size) {
		if (0 == listen_fifo)
			return LINTED_ERROR_INVALID_PARAMETER;
	}

	{
		struct linted_admin_request request = {0};

		request.type = LINTED_ADMIN_ADD_SOCKET;

		struct linted_admin_request_add_socket *xx =
		    &request.linted_admin_request_u.add_socket;

		xx->name = (char *)unit_name;
		xx->path = (char *)path;
		xx->fifo_size = fifo_size;
		xx->sock_type = socket_type;

		err = linted_admin_in_send(admin_in, &request);
	}
	if (err != 0)
		goto free_unit_name;

	{
		struct linted_admin_reply xx;
		err = linted_admin_out_recv(admin_out, &xx);
		if (err != 0)
			goto free_unit_name;
	}

free_unit_name:
	linted_mem_free(unit_name);

	return err;
}

static linted_error
service_activate(struct system_conf const *system_conf,
                 struct linted_conf *conf, linted_ko admin_in,
                 linted_ko admin_out)
{
	linted_error err = 0;

	char const *file_name = linted_conf_peek_name(conf);

	char const *dot = strchr(file_name, '.');

	char *unit_name;
	{
		char *xx;
		err =
		    linted_str_dup_len(&xx, file_name, dot - file_name);
		if (err != 0)
			return err;
		unit_name = xx;
	}

	char const *const *command =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *env_whitelist = linted_conf_find(
	    conf, "Service", "X-LintedEnvironmentWhitelist");
	char const *const *clone_flags =
	    linted_conf_find(conf, "Service", "X-LintedCloneFlags");

	char const *type;
	{
		char const *xx = 0;
		err =
		    linted_conf_find_str(conf, "Service", "Type", &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		type = xx;
	}

	bool no_new_privs;
	{
		bool xx = false;
		err = linted_conf_find_bool(conf, "Service",
		                            "NoNewPrivileges", &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		no_new_privs = xx;
	}

	char const *fstab;
	{
		char const *xx = 0;
		err = linted_conf_find_str(conf, "Service",
		                           "X-LintedFstab", &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		fstab = xx;
	}

	char const *chdir_path;
	{
		char const *xx = 0;
		err = linted_conf_find_str(conf, "Service",
		                           "WorkingDirectory", &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		chdir_path = xx;
	}

	int_least64_t timer_slack_nsec;
	bool has_timer_slack_nsec;
	{
		int xx = -1;
		err = linted_conf_find_int(conf, "Service",
		                           "TimerSlackNSec", &xx);
		if (0 == err) {
			has_timer_slack_nsec = true;
		} else if (ENOENT == err) {
			has_timer_slack_nsec = false;
		} else {
			goto free_unit_name;
		}
		timer_slack_nsec = xx;
	}

	int_least64_t priority;
	bool has_priority;
	{
		int xx = -1;
		err = linted_conf_find_int(conf, "Service", "Priority",
		                           &xx);
		if (0 == err) {
			has_priority = true;
		} else if (ENOENT == err) {
			has_priority = false;
		} else {
			goto free_unit_name;
		}
		priority = xx;
	}

	if (0 == type || 0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto free_unit_name;
	}

	if (0 == command) {
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto free_unit_name;
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

	char **envvars;
	{
		char **xx;
		err = filter_envvars(&xx, env_whitelist);
		if (err != 0)
			goto free_unit_name;
		envvars = xx;
	}

	size_t envvars_size =
	    null_list_size((char const *const *)envvars);

	char *service_name_setting;
	{
		char *xx;
		err = linted_str_format(&xx, "LINTED_SERVICE=%s",
		                        unit_name);
		if (err != 0)
			goto free_envvars;
		service_name_setting = xx;
	}

	size_t new_size = envvars_size + 2U;
	LINTED_ASSERT(new_size > 0U);
	{
		void *xx;
		err = linted_mem_realloc_array(&xx, envvars, new_size,
		                               sizeof envvars[0U]);
		if (err != 0)
			goto envvar_allocate_failed;
		envvars = xx;
		goto envvar_allocate_succeeded;
	}
envvar_allocate_failed:
	linted_mem_free(service_name_setting);
	goto free_envvars;

envvar_allocate_succeeded:
	envvars[envvars_size] = service_name_setting;
	envvars[envvars_size + 1U] = 0;

	int_least64_t limit_no_file = system_conf->limit_no_file;
	bool has_limit_no_file = system_conf->has_limit_no_file;

	int_least64_t limit_msgqueue = system_conf->limit_msgqueue;
	bool has_limit_msgqueue = system_conf->has_limit_msgqueue;

	int_least64_t limit_locks = system_conf->limit_locks;
	bool has_limit_locks = system_conf->has_limit_locks;

	char const *const *environment = (char const *const *)envvars;

	if (0 == fstab)
		fstab = "";

	if (0 == chdir_path)
		chdir_path = "";

	{
		struct linted_admin_request request = {0};

		request.type = LINTED_ADMIN_ADD_UNIT;

		struct linted_admin_request_add_unit *xx =
		    &request.linted_admin_request_u.add_unit;

		xx->timer_slack_nsec =
		    has_timer_slack_nsec ? &timer_slack_nsec : 0;
		xx->priority = has_priority ? &priority : 0;
		xx->limit_no_file =
		    has_limit_no_file ? &limit_no_file : 0;
		xx->limit_msgqueue =
		    has_limit_msgqueue ? &limit_msgqueue : 0;
		xx->limit_locks = has_limit_locks ? &limit_locks : 0;

		xx->clone_newuser = clone_newuser;
		xx->clone_newpid = clone_newpid;
		xx->clone_newipc = clone_newipc;
		xx->clone_newnet = clone_newnet;
		xx->clone_newns = clone_newns;
		xx->clone_newuts = clone_newuts;

		xx->no_new_privs = no_new_privs;

		xx->name = (char *)unit_name;
		xx->fstab = (char *)fstab;
		xx->chdir_path = (char *)chdir_path;

		xx->command.command_len = null_list_size(command);
		xx->command.command_val = (char **)command;

		xx->environment.environment_len =
		    null_list_size(environment);
		xx->environment.environment_val = (char **)environment;

		err = linted_admin_in_send(admin_in, &request);
	}
	if (err != 0)
		goto free_envvars;

	{
		struct linted_admin_reply xx;
		err = linted_admin_out_recv(admin_out, &xx);
		if (err != 0)
			goto free_envvars;
	}

free_envvars:
	for (size_t ii = 0U; ii < envvars_size; ++ii) {
		linted_mem_free(envvars[ii]);
	}
	linted_mem_free(envvars);

free_unit_name:
	linted_mem_free(unit_name);

	return err;
}

static size_t null_list_size(char const *const *list)
{
	if (0 == list)
		return 0U;

	for (size_t ii = 0U;; ++ii)
		if (0 == list[ii])
			return ii;
}
extern char **environ;

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	char **result_envvars;
	linted_error err;

	if (0 == allowed_envvars) {
		size_t size =
		    null_list_size((char const *const *)environ);

		{
			void *xx;
			err = linted_mem_alloc_array(
			    &xx, size + 1U, sizeof result_envvars[0U]);
			if (err != 0)
				return err;
			result_envvars = xx;
		}

		for (size_t ii = 0U; ii < size; ++ii) {
			err = linted_str_dup(&result_envvars[ii],
			                     environ[ii]);
			if (err != 0) {
				for (size_t jj = 0; jj <= ii; ++jj) {
					linted_mem_free(
					    result_envvars[jj]);
				}
				linted_mem_free(result_envvars);
				return err;
			}
		}
		result_envvars[size] = 0;
		*result_envvarsp = result_envvars;
		return 0;
	}

	size_t allowed_envvars_size = null_list_size(allowed_envvars);
	{
		void *xx;
		err = linted_mem_alloc_array(&xx,
		                             allowed_envvars_size + 1U,
		                             sizeof result_envvars[0U]);
		if (err != 0)
			return err;
		result_envvars = xx;
	}

	size_t result_envvars_size = 0U;
	for (size_t ii = 0U; ii < allowed_envvars_size; ++ii) {
		char const *envvar_name = allowed_envvars[ii];

		char *envvar_value;
		{
			char *xx;
			err = linted_env_get(envvar_name, &xx);
			if (err != 0)
				goto free_result_envvars;
			envvar_value = xx;
		}
		if (0 == envvar_value)
			continue;

		++result_envvars_size;

		err = linted_str_format(
		    &result_envvars[result_envvars_size - 1U], "%s=%s",
		    envvar_name, envvar_value);

		linted_mem_free(envvar_value);

		if (err != 0)
			goto free_result_envvars;
	}
	result_envvars[result_envvars_size] = 0;

	*result_envvarsp = result_envvars;

	return 0;

free_result_envvars:
	for (size_t ii = 0U; ii < result_envvars_size; ++ii)
		linted_mem_free(result_envvars[ii]);
	linted_mem_free(result_envvars);
	return err;
}
