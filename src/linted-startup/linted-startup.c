/*
 * Copyright 2013, 2014, 2015, 2016 Steven Stewart-Gallus
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

#include "lntd/admin.h"
#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/start.h"
#include "lntd/str.h"
#include "lntd/unit.h"
#include "lntd/util.h"

#include <dirent.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <wordexp.h>

struct conf;

static lntd_error
conf_system_default_limit_locks(struct conf *conf,
                                int_least64_t *limitp);
static lntd_error
conf_system_default_limit_memlock(struct conf *conf,
                                  int_least64_t *limitp);
static lntd_error
conf_system_default_limit_msgqueue(struct conf *conf,
                                   int_least64_t *limitp);
static lntd_error
conf_system_default_limit_nofile(struct conf *conf,
                                 int_least64_t *limitp);
static lntd_error
conf_system_default_no_new_privileges(struct conf *conf,
                                      bool *no_new_privs);
static lntd_error conf_system_default_seccomp(struct conf *conf,
                                              bool *boolp);
static char const *const *
conf_system_default_pass_env(struct conf *conf);

static lntd_error conf_socket_listen_directory(struct conf *conf,
                                               char const **strp);
static lntd_error conf_socket_listen_file(struct conf *conf,
                                          char const **strp);
static lntd_error conf_socket_listen_fifo(struct conf *conf,
                                          char const **strp);
static lntd_error conf_socket_pipe_size(struct conf *conf,
                                        int_least64_t *intp);

static char const *const *conf_service_exec_start(struct conf *conf);
static char const *const *
conf_service_pass_environment(struct conf *conf);
static char const *const *conf_service_environment(struct conf *conf);
static char const *const *
conf_service_x_lntd_clone_flags(struct conf *conf);

static lntd_error conf_service_timer_slack_nsec(struct conf *conf,
                                                int_least64_t *intp);
static lntd_error conf_service_priority(struct conf *conf,
                                        int_least64_t *intp);
static lntd_error conf_service_limit_no_file(struct conf *conf,
                                             int_least64_t *intp);
static lntd_error conf_service_limit_msgqueue(struct conf *conf,
                                              int_least64_t *intp);
static lntd_error conf_service_limit_locks(struct conf *conf,
                                           int_least64_t *intp);
static lntd_error conf_service_limit_memlock(struct conf *conf,
                                             int_least64_t *intp);

static lntd_error conf_service_type(struct conf *conf,
                                    char const **strp);
static lntd_error conf_service_no_new_privileges(struct conf *conf,
                                                 bool *boolp);
static lntd_error conf_service_seccomp(struct conf *conf, bool *boolp);
static lntd_error conf_service_x_lntd_fstab(struct conf *conf,
                                            char const **strp);
static lntd_error conf_service_working_directory(struct conf *conf,
                                                 char const **strp);

struct conf_db;
struct conf;

typedef uint_fast64_t conf_section;

struct conf_db;

static lntd_error conf_db_create(struct conf_db **dbp);
static void conf_db_destroy(struct conf_db *db);

static lntd_error conf_db_add_conf(struct conf_db *db,
                                   struct conf *conf);

static size_t conf_db_size(struct conf_db *db);
static struct conf *conf_db_get_conf(struct conf_db *db, size_t ii);

static lntd_error conf_create(struct conf **confp, char *file_name);

static lntd_error conf_parse_file(struct conf *conf, lntd_ko dir_ko,
                                  char const *file_name);

static void conf_put(struct conf *conf);

static char const *conf_peek_name(struct conf *conf);

static lntd_error conf_add_section(struct conf *conf,
                                   conf_section *sectionp,
                                   char const *section_name);

static lntd_error conf_add_setting(struct conf *conf,
                                   conf_section section,
                                   char const *field,
                                   char const *const *value);

enum { LNTD_SYSTEM_CONF_PATH, LNTD_UNIT_PATH };

static char const *const required_envs[] =
    {[LNTD_SYSTEM_CONF_PATH] = "LINTED_SYSTEM_CONF_PATH",
     [LNTD_UNIT_PATH] = "LINTED_UNIT_PATH"};

struct startup {
	char const *system_conf_path;
	char const *unit_path;
	lntd_ko admin_in;
	lntd_ko admin_out;
};

struct system_conf {
	int_least64_t *limit_locks;
	int_least64_t *limit_msgqueue;
	int_least64_t *limit_no_file;
	int_least64_t *limit_memlock;
	bool *no_new_privs;
	bool *seccomp;
	char const *const *pass_env;
};

static lntd_error startup_init(struct startup *startup,
                               char const *controller_path,
                               char const *updater_path,
                               char const *system_conf_path,
                               char const *unit_path);

static lntd_error startup_destroy(struct startup *startup);

static lntd_error startup_start(struct startup *startup);
static lntd_error startup_stop(struct startup *startup);

static lntd_error
populate_conf_db(struct conf_db *conf_db,
                 struct system_conf const *system_conf, lntd_ko cwd,
                 char const *unit_path);
static lntd_error populate_system_conf(struct conf_db *db, lntd_ko cwd,
                                       char const *file_name);
static lntd_error
add_unit_dir_to_db(struct conf_db *db,
                   struct system_conf const *system_conf, lntd_ko cwd,
                   char const *dir_name);

static lntd_error socket_activate(struct conf *conf, lntd_ko admin_in,
                                  lntd_ko admin_out);

static lntd_error
service_activate(struct system_conf const *system_conf,
                 struct conf *conf, lntd_ko admin_in,
                 lntd_ko admin_out);

static size_t null_list_size(char const *const *list);

static lntd_error filter_envvars(char ***resultsp,
                                 char const *const *allowed_envvars);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-startup", 0};

static unsigned char lntd_start_main(char const *const process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	if (argc != 3U) {
		lntd_log(LNTD_LOG_ERROR,
		         "missing some of 2 file operands");
		return EXIT_FAILURE;
	}

	char const *system_conf_path;
	char const *unit_path;
	{
		char *envs[LNTD_ARRAY_SIZE(required_envs)];
		for (size_t ii = 0U;
		     ii < LNTD_ARRAY_SIZE(required_envs); ++ii) {
			char const *req = required_envs[ii];
			char *value;
			{
				char *xx;
				err = lntd_env_get(req, &xx);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_env_get: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				value = xx;
			}
			if (0 == value) {
				lntd_log(LNTD_LOG_ERROR,
				         "%s is a required "
				         "environment variable",
				         req);
				return EXIT_FAILURE;
			}
			envs[ii] = value;
		}
		system_conf_path = envs[LNTD_SYSTEM_CONF_PATH];
		unit_path = envs[LNTD_UNIT_PATH];
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

	lntd_error stop_err = startup_stop(&startup);
	if (0 == err)
		err = stop_err;

destroy_startup:
	;
	lntd_error destroy_err = startup_destroy(&startup);
	if (0 == err)
		err = destroy_err;

log_error:
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "%s", lntd_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static lntd_error startup_init(struct startup *startup,
                               char const *admin_in_path,
                               char const *admin_out_path,
                               char const *system_conf_path,
                               char const *unit_path)
{
	lntd_error err = 0;

	lntd_admin_in admin_in;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, admin_in_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			return err;
		admin_in = xx;
	}

	lntd_admin_out admin_out;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, admin_out_path,
		                   LNTD_KO_RDWR);
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
	lntd_ko_close(admin_in);

	return err;
}

static lntd_error startup_destroy(struct startup *startup)
{
	lntd_ko admin_in = startup->admin_in;
	lntd_ko admin_out = startup->admin_out;

	lntd_ko_close(admin_in);

	lntd_ko_close(admin_out);

	return 0;
}

static lntd_error startup_start(struct startup *startup)
{
	char const *system_conf_path = startup->system_conf_path;
	char const *unit_path = startup->unit_path;
	lntd_ko admin_in = startup->admin_in;
	lntd_ko admin_out = startup->admin_out;

	lntd_error err = 0;

	struct conf_db *conf_db;
	{
		struct conf_db *xx;
		err = conf_db_create(&xx);
		if (err != 0)
			return err;
		conf_db = xx;
	}

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
			err = lntd_str_dup_len(&xx, start, len);
			if (err != 0)
				goto destroy_conf_db;
			file_name_dup = xx;
		}

		err = populate_system_conf(conf_db, LNTD_KO_CWD,
		                           file_name_dup);

		lntd_mem_free(file_name_dup);

		if (err != 0)
			goto destroy_conf_db;

		if (0 == end)
			break;

		start = end + 1U;
	}

	struct conf *system_conf = 0;
	for (size_t ii = 0U, size = conf_db_size(conf_db); ii < size;
	     ++ii) {
		struct conf *conf = conf_db_get_conf(conf_db, ii);

		char const *file_name = conf_peek_name(conf);

		if (0 == strcmp("system.conf", file_name)) {
			system_conf = conf;
			break;
		}
	}
	if (0 == system_conf) {
		err = LNTD_ERROR_FILE_NOT_FOUND;
		goto destroy_conf_db;
	}

	struct system_conf system_conf_struct = {0};

	int_least64_t limit_locks;
	int_least64_t limit_memlock;
	int_least64_t limit_msgqueue;
	int_least64_t limit_no_file;
	bool no_new_privs;
	bool seccomp;

	err =
	    conf_system_default_limit_locks(system_conf, &limit_locks);
	if (0 == err) {
		system_conf_struct.limit_locks = &limit_locks;
	} else if (ENOENT == err) {
	} else {
		goto destroy_conf_db;
	}
	err = conf_system_default_limit_memlock(system_conf,
	                                        &limit_memlock);
	if (0 == err) {
		system_conf_struct.limit_memlock = &limit_memlock;
	} else if (ENOENT == err) {
	} else {
		goto destroy_conf_db;
	}
	err = conf_system_default_limit_msgqueue(system_conf,
	                                         &limit_msgqueue);
	if (0 == err) {
		system_conf_struct.limit_msgqueue = &limit_msgqueue;
	} else if (ENOENT == err) {
	} else {
		goto destroy_conf_db;
	}
	err = conf_system_default_limit_nofile(system_conf,
	                                       &limit_no_file);
	if (0 == err) {
		system_conf_struct.limit_no_file = &limit_no_file;
	} else if (ENOENT == err) {
	} else {
		goto destroy_conf_db;
	}

	err = conf_system_default_no_new_privileges(system_conf,
	                                            &no_new_privs);
	if (0 == err) {
		system_conf_struct.no_new_privs = &no_new_privs;
	} else if (ENOENT == err) {
	} else {
		goto destroy_conf_db;
	}

	err = conf_system_default_seccomp(system_conf, &seccomp);
	if (0 == err) {
		system_conf_struct.seccomp = &seccomp;
	} else if (ENOENT == err) {
	} else {
		goto destroy_conf_db;
	}

	system_conf_struct.pass_env =
	    conf_system_default_pass_env(system_conf);

	err = populate_conf_db(conf_db, &system_conf_struct,
	                       LNTD_KO_CWD, unit_path);
	if (err != 0)
		goto destroy_conf_db;

	size_t size = conf_db_size(conf_db);
	for (size_t ii = 0U; ii < size; ++ii) {
		struct conf *conf = conf_db_get_conf(conf_db, ii);

		char const *file_name = conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		if (strcmp(suffix, "socket") != 0)
			continue;

		err = socket_activate(conf, admin_in, admin_out);
		if (err != 0)
			goto destroy_conf_db;
	}
	for (size_t ii = 0U; ii < size; ++ii) {
		struct conf *conf = conf_db_get_conf(conf_db, ii);

		char const *file_name = conf_peek_name(conf);

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
	conf_db_destroy(conf_db);

	return err;
}

static lntd_error startup_stop(struct startup *startup)
{
	return 0;
}

static lntd_error
populate_conf_db(struct conf_db *db,
                 struct system_conf const *system_conf, lntd_ko cwd,
                 char const *unit_path)
{
	lntd_error err = 0;

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
			err = lntd_str_dup_len(&xx, dirstart, len);
			if (err != 0)
				return err;
			dir_name = xx;
		}

		err =
		    add_unit_dir_to_db(db, system_conf, cwd, dir_name);

		lntd_mem_free(dir_name);

		if (err != 0)
			return err;

		if (0 == dirend)
			break;

		dirstart = dirend + 1U;
	}

	return 0;
}

static lntd_error populate_system_conf(struct conf_db *db, lntd_ko cwd,
                                       char const *filename)
{
	lntd_error err;

	lntd_ko conf_ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, cwd, filename, LNTD_KO_RDONLY);
		/* Just treat as an empty file */
		if (LNTD_ERROR_FILE_NOT_FOUND == err)
			return 0;
		if (err != 0)
			return err;
		conf_ko = xx;
	}

	char *filename_dup;
	{
		char *xx;
		err = lntd_str_dup(&xx, "system.conf");
		if (err != 0)
			goto close_ko;
		filename_dup = xx;
	}

	struct conf *conf = 0;
	{
		struct conf *xx;
		err = conf_create(&xx, filename_dup);
		if (err != 0) {
			lntd_mem_free(filename_dup);
			goto close_ko;
		}
		conf = xx;
	}

	err = conf_parse_file(conf, conf_ko, filename);
	if (err != 0)
		goto free_conf;

	err = conf_db_add_conf(db, conf);

free_conf:
	if (err != 0)
		conf_put(conf);

close_ko:
	lntd_ko_close(conf_ko);

	return err;
}

static lntd_error
add_unit_dir_to_db(struct conf_db *db,
                   struct system_conf const *system_conf, lntd_ko cwd,
                   char const *dir_name)
{
	lntd_error err;

	lntd_ko units_ko;
	{
		lntd_ko xx;
		err =
		    lntd_ko_open(&xx, cwd, dir_name, LNTD_KO_DIRECTORY);
		/* Just treat as an empty directory */
		if (LNTD_ERROR_FILE_NOT_FOUND == err)
			return 0;
		if (err != 0)
			return err;
		units_ko = xx;
	}

	DIR *units_dir = fdopendir(units_ko);
	if (0 == units_dir) {
		err = errno;
		LNTD_ASSUME(err != 0);

		lntd_ko_close(units_ko);
	}

	if (LNTD_ERROR_FILE_NOT_FOUND == err)
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
			err = lntd_str_dup(&xx, name);
			if (err != 0)
				goto free_file_names;
			name_copy = xx;
		}

		size_t new_files_count = files_count + 1U;
		char **new_files;
		{
			void *xx;
			err = lntd_mem_realloc_array(&xx, files,
			                             new_files_count,
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
			lntd_mem_free(name_copy);
			goto free_file_names;
		}
	}

	for (size_t ii = 0U; ii < files_count; ++ii) {
		char *file_name = files[ii];

		lntd_unit_type unit_type;
		{
			char const *dot = strchr(file_name, '.');

			char const *suffix = dot + 1U;

			if (0 == strcmp(suffix, "socket")) {
				unit_type = LNTD_UNIT_TYPE_SOCKET;
			} else if (0 == strcmp(suffix, "service")) {
				unit_type = LNTD_UNIT_TYPE_SERVICE;
			} else {
				err = LNTD_ERROR_INVALID_PARAMETER;
				goto free_file_names;
			}
		}
		struct conf *conf = 0;
		{
			struct conf *xx;
			err = conf_create(&xx, file_name);
			if (err != 0)
				goto close_unit_file;
			conf = xx;
		}

		files[ii] = 0;

		switch (unit_type) {
		case LNTD_UNIT_TYPE_SOCKET:
			/* Okay but we have no defaults for this */
			break;

		case LNTD_UNIT_TYPE_SERVICE: {
			conf_section service;
			{
				conf_section xx;
				err = conf_add_section(conf, &xx,
				                       "Service");
				if (err != 0)
					goto close_unit_file;
				service = xx;
			}

			if (system_conf->pass_env != 0) {
				err = conf_add_setting(
				    conf, service, "PassEnvironment",
				    system_conf->pass_env);
				if (err != 0)
					goto close_unit_file;
			}

			if (system_conf->limit_no_file != 0) {
				char
				    limits[LNTD_NUMBER_TYPE_STRING_SIZE(
				        int_least64_t)];
				sprintf(limits, "%" PRId64,
				        *system_conf->limit_no_file);
				char const *const expr[] = {limits, 0};
				err = conf_add_setting(
				    conf, service, "LimitNOFILE", expr);
				if (err != 0)
					goto close_unit_file;
			}

			if (system_conf->limit_locks != 0) {
				char
				    limits[LNTD_NUMBER_TYPE_STRING_SIZE(
				        int_least64_t)];
				sprintf(limits, "%" PRId64,
				        *system_conf->limit_locks);

				char const *const expr[] = {limits, 0};
				err = conf_add_setting(
				    conf, service, "LimitLOCKS", expr);
				if (err != 0)
					goto close_unit_file;
			}

			if (system_conf->limit_msgqueue != 0) {
				char
				    limits[LNTD_NUMBER_TYPE_STRING_SIZE(
				        int_least64_t)];
				sprintf(limits, "%" PRId64,
				        *system_conf->limit_msgqueue);

				char const *const expr[] = {limits, 0};
				err = conf_add_setting(conf, service,
				                       "LimitMSGQUEUE",
				                       expr);
				if (err != 0)
					goto close_unit_file;
			}

			if (system_conf->limit_memlock != 0) {
				char
				    limits[LNTD_NUMBER_TYPE_STRING_SIZE(
				        int_least64_t)];
				sprintf(limits, "%" PRId64,
				        *system_conf->limit_memlock);

				char const *const expr[] = {limits, 0};
				err = conf_add_setting(conf, service,
				                       "LimitMEMLOCK",
				                       expr);
				if (err != 0)
					goto close_unit_file;
			}

			if (system_conf->no_new_privs != 0) {
				char const *const expr[] = {
				    *system_conf->no_new_privs
				        ? "true"
				        : "false",
				    0};
				err = conf_add_setting(
				    conf, service, "NoNewPrivileges",
				    expr);
				if (err != 0)
					goto close_unit_file;
			}

			if (system_conf->seccomp != 0) {
				char const *const expr[] = {
				    *system_conf->seccomp ? "true"
				                          : "false",
				    0};
				err = conf_add_setting(conf, service,
				                       "Seccomp", expr);
				if (err != 0)
					goto close_unit_file;
			}
			break;
		}
		}

		err = conf_parse_file(conf, units_ko, file_name);

	close_unit_file:
		if (err != 0)
			goto free_unit;

		err = conf_db_add_conf(db, conf);

	free_unit:
		if (err != 0)
			conf_put(conf);
	}

free_file_names:
	for (size_t ii = 0U; ii < files_count; ++ii)
		lntd_mem_free(files[ii]);
	lntd_mem_free(files);

	if (-1 == closedir(units_dir)) {
		if (0 == err) {
			err = errno;
			LNTD_ASSUME(err != 0);
		}
	}

	return err;
}

static lntd_error socket_activate(struct conf *conf, lntd_ko admin_in,
                                  lntd_ko admin_out)
{
	lntd_error err = 0;

	char const *file_name = conf_peek_name(conf);

	char const *dot = strchr(file_name, '.');

	char *unit_name;
	{
		char *xx;
		err = lntd_str_dup_len(&xx, file_name, dot - file_name);
		if (err != 0)
			return err;
		unit_name = xx;
	}
	char const *listen_dir;
	{
		char const *xx = 0;
		err = conf_socket_listen_directory(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		listen_dir = xx;
	}

	char const *listen_file;
	{
		char const *xx = 0;
		err = conf_socket_listen_file(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		listen_file = xx;
	}

	char const *listen_fifo;
	{
		char const *xx = 0;
		err = conf_socket_listen_fifo(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		listen_fifo = xx;
	}

	int fifo_size;
	bool have_fifo_size;
	{
		int_least64_t xx = -1;
		err = conf_socket_pipe_size(conf, &xx);
		if (0 == err) {
			have_fifo_size = true;
		} else if (ENOENT == err) {
			have_fifo_size = false;
		} else {
			goto free_unit_name;
		}
		fifo_size = xx;
	}

	lntd_unit_socket_type socket_type;
	char const *path = 0;

	if (listen_dir != 0) {
		socket_type = LNTD_UNIT_SOCKET_TYPE_DIR;
		path = listen_dir;
	}

	if (listen_file != 0) {
		if (path != 0)
			return LNTD_ERROR_INVALID_PARAMETER;
		socket_type = LNTD_UNIT_SOCKET_TYPE_FILE;
		path = listen_file;
	}

	if (listen_fifo != 0) {
		if (path != 0)
			return LNTD_ERROR_INVALID_PARAMETER;
		socket_type = LNTD_UNIT_SOCKET_TYPE_FIFO;
		path = listen_fifo;
	}

	if (0 == path)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (have_fifo_size) {
		if (0 == listen_fifo)
			return LNTD_ERROR_INVALID_PARAMETER;
	}

	{
		struct lntd_admin_request request = {0};

		request.type = LNTD_ADMIN_ADD_SOCKET;

		struct lntd_admin_request_add_socket *xx =
		    &request.lntd_admin_request_u.add_socket;

		xx->name = (char *)unit_name;
		xx->path = (char *)path;
		xx->fifo_size = fifo_size;
		xx->sock_type = socket_type;

		err = lntd_admin_in_send(admin_in, &request);
	}
	if (err != 0)
		goto free_unit_name;

	{
		struct lntd_admin_reply xx;
		err = lntd_admin_out_recv(admin_out, &xx);
		if (err != 0)
			goto free_unit_name;
	}

free_unit_name:
	lntd_mem_free(unit_name);

	return err;
}

static lntd_error
service_activate(struct system_conf const *system_conf,
                 struct conf *conf, lntd_ko admin_in, lntd_ko admin_out)
{
	lntd_error err = 0;

	char const *file_name = conf_peek_name(conf);

	char const *dot = strchr(file_name, '.');

	char *unit_name;
	{
		char *xx = 0;
		err = lntd_str_dup_len(&xx, file_name, dot - file_name);
		if (err != 0)
			return err;
		unit_name = xx;
		LNTD_ASSERT(unit_name != 0);
	}

	char const *const *command = conf_service_exec_start(conf);
	char const *const *env_whitelist =
	    conf_service_pass_environment(conf);
	char const *const *env = conf_service_environment(conf);
	char const *const *clone_flags =
	    conf_service_x_lntd_clone_flags(conf);

	char const *type;
	{
		char const *xx = 0;
		err = conf_service_type(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		type = xx;
	}

	bool no_new_privs;
	{
		bool xx = false;
		err = conf_service_no_new_privileges(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		no_new_privs = xx;
	}

	bool seccomp;
	{
		bool xx = false;
		err = conf_service_seccomp(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		seccomp = xx;
	}

	char const *fstab;
	{
		char const *xx = 0;
		err = conf_service_x_lntd_fstab(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		fstab = xx;
	}

	char const *chdir_path;
	{
		char const *xx = 0;
		err = conf_service_working_directory(conf, &xx);
		if (err != 0 && err != ENOENT)
			goto free_unit_name;
		chdir_path = xx;
	}

	int_least64_t timer_slack_nsec;
	bool has_timer_slack_nsec;
	{
		int_least64_t xx = -1;
		err = conf_service_timer_slack_nsec(conf, &xx);
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
		int_least64_t xx = -1;
		err = conf_service_priority(conf, &xx);
		if (0 == err) {
			has_priority = true;
		} else if (ENOENT == err) {
			has_priority = false;
		} else {
			goto free_unit_name;
		}
		priority = xx;
	}

	int_least64_t limit_no_file;
	bool has_limit_no_file;
	{
		int_least64_t xx = -1;
		err = conf_service_limit_no_file(conf, &xx);
		if (0 == err) {
			has_limit_no_file = true;
		} else if (ENOENT == err) {
			has_limit_no_file = false;
		} else {
			goto free_unit_name;
		}
		limit_no_file = xx;
	}

	int_least64_t limit_msgqueue;
	bool has_limit_msgqueue;
	{
		int_least64_t xx = -1;
		err = conf_service_limit_msgqueue(conf, &xx);
		if (0 == err) {
			has_limit_msgqueue = true;
		} else if (ENOENT == err) {
			has_limit_msgqueue = false;
		} else {
			goto free_unit_name;
		}
		limit_msgqueue = xx;
	}

	int_least64_t limit_locks;
	bool has_limit_locks;
	{
		int_least64_t xx = -1;
		err = conf_service_limit_locks(conf, &xx);
		if (0 == err) {
			has_limit_locks = true;
		} else if (ENOENT == err) {
			has_limit_locks = false;
		} else {
			goto free_unit_name;
		}
		limit_locks = xx;
	}

	int_least64_t limit_memlock;
	bool has_limit_memlock;
	{
		int_least64_t xx = -1;
		err = conf_service_limit_memlock(conf, &xx);
		if (0 == err) {
			has_limit_memlock = true;
		} else if (ENOENT == err) {
			has_limit_memlock = false;
		} else {
			goto free_unit_name;
		}
		limit_memlock = xx;
	}

	if (0 == type || 0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		err = LNTD_ERROR_INVALID_PARAMETER;
		goto free_unit_name;
	}

	if (0 == command) {
		err = LNTD_ERROR_INVALID_PARAMETER;
		goto free_unit_name;
	}

	bool clone_newuser = false;
	bool clone_newcgroup = false;
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
			} else if (0 ==
			           strcmp("CLONE_NEWCGROUP", flag)) {
				clone_newcgroup = true;
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
				return LNTD_ERROR_INVALID_PARAMETER;
			}
		}
	}

	if (0 == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		return LNTD_ERROR_INVALID_PARAMETER;
	}

	char **envvars;
	{
		char **xx = 0;
		err = filter_envvars(&xx, env_whitelist);
		if (err != 0)
			goto free_unit_name;
		LNTD_ASSERT(xx != 0);
		envvars = xx;
	}

	if (env != 0) {
		size_t envvars_size =
		    null_list_size((char const *const *)envvars);

		for (size_t ii = 0U; env[ii] != 0; ++ii) {
			{
				void *xx = 0;
				err = lntd_mem_realloc_array(
				    &xx, envvars, envvars_size + 2U,
				    sizeof envvars[0U]);
				if (err != 0)
					goto free_unit_name;
				LNTD_ASSERT(xx != 0);
				envvars = xx;
			}

			++envvars_size;

			envvars[envvars_size] = 0;

			{
				char *yy = 0;
				err = lntd_str_dup(&yy, env[ii]);
				if (err != 0)
					goto free_unit_name;
				LNTD_ASSERT(yy != 0);
				envvars[envvars_size - 1U] = yy;
			}
		}
	}

	char *service_name_setting;
	{
		char *xx = 0;
		err = lntd_str_format(&xx, "LINTED_SERVICE=%s",
		                      unit_name);
		if (err != 0)
			goto free_envvars;
		LNTD_ASSERT(xx != 0);
		service_name_setting = xx;
	}

	{
		size_t envvars_size =
		    null_list_size((char const *const *)envvars);

		size_t new_size = envvars_size + 2U;
		LNTD_ASSERT(new_size > 0U);
		{
			void *xx = 0;
			err = lntd_mem_realloc_array(
			    &xx, envvars, new_size, sizeof envvars[0U]);
			if (err != 0)
				goto envvar_allocate_failed;
			LNTD_ASSERT(xx != 0);
			envvars = xx;
			goto envvar_allocate_succeeded;
		}
	envvar_allocate_failed:
		lntd_mem_free(service_name_setting);
		service_name_setting = 0;
		goto free_envvars;

	envvar_allocate_succeeded:
		envvars[envvars_size] = service_name_setting;
		envvars[envvars_size + 1U] = 0;
	}

	char const *const *environment = (char const *const *)envvars;

	if (0 == fstab)
		fstab = "";

	if (0 == chdir_path)
		chdir_path = "";

	{
		struct lntd_admin_request request = {0};

		request.type = LNTD_ADMIN_ADD_UNIT;

		struct lntd_admin_request_add_unit *xx =
		    &request.lntd_admin_request_u.add_unit;

		xx->timer_slack_nsec =
		    has_timer_slack_nsec ? &timer_slack_nsec : 0;
		xx->priority = has_priority ? &priority : 0;
		xx->limit_no_file =
		    has_limit_no_file ? &limit_no_file : 0;
		xx->limit_msgqueue =
		    has_limit_msgqueue ? &limit_msgqueue : 0;
		xx->limit_locks = has_limit_locks ? &limit_locks : 0;
		xx->limit_memlock =
		    has_limit_memlock ? &limit_memlock : 0;

		xx->clone_newuser = clone_newuser;
		xx->clone_newcgroup = clone_newcgroup;
		xx->clone_newpid = clone_newpid;
		xx->clone_newipc = clone_newipc;
		xx->clone_newnet = clone_newnet;
		xx->clone_newns = clone_newns;
		xx->clone_newuts = clone_newuts;

		xx->no_new_privs = no_new_privs;
		xx->seccomp = seccomp;

		xx->name = (char *)unit_name;
		xx->fstab = (char *)fstab;
		xx->chdir_path = (char *)chdir_path;

		xx->command.command_len = null_list_size(command);
		xx->command.command_val = (char **)command;

		xx->environment.environment_len =
		    null_list_size(environment);
		xx->environment.environment_val = (char **)environment;

		err = lntd_admin_in_send(admin_in, &request);
	}
	if (err != 0)
		goto free_envvars;

	{
		struct lntd_admin_reply xx;
		err = lntd_admin_out_recv(admin_out, &xx);
		if (err != 0)
			goto free_envvars;
	}

free_envvars:
	;
	size_t envvars_size =
	    null_list_size((char const *const *)envvars);

	for (size_t ii = 0U; ii < envvars_size; ++ii) {
		lntd_mem_free(envvars[ii]);
		envvars[ii] = 0;
	}
	lntd_mem_free(envvars);
	envvars = 0;

free_unit_name:
	lntd_mem_free(unit_name);
	unit_name = 0;

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

static lntd_error filter_envvars(char ***result_envvarsp,
                                 char const *const *allowed_envvars)
{
	char **result_envvars;
	lntd_error err;

	if (0 == allowed_envvars) {
		char const *const *envs = (char const *const *)environ;

		size_t size = null_list_size(envs);

		{
			void *xx = 0;
			err = lntd_mem_alloc_array(
			    &xx, size + 1U, sizeof result_envvars[0U]);
			if (err != 0)
				return err;
			LNTD_ASSERT(xx != 0);
			result_envvars = xx;
		}

		for (size_t ii = 0U; ii < size; ++ii) {
			char const *input = envs[ii];

			char *xx = 0;
			err = lntd_str_dup(&xx, input);
			if (err != 0)
				goto free_result_envvars;
			LNTD_ASSERT(xx != 0);
			result_envvars[ii] = xx;
		}
		result_envvars[size] = 0;
		*result_envvarsp = result_envvars;
		return 0;
	}

	size_t allowed_envvars_size = null_list_size(allowed_envvars);
	{
		void *xx = 0;
		err =
		    lntd_mem_alloc_array(&xx, allowed_envvars_size + 1U,
		                         sizeof result_envvars[0U]);
		if (err != 0)
			return err;
		LNTD_ASSERT(xx != 0);
		result_envvars = xx;
	}

	size_t result_envvars_size = 0U;
	for (size_t ii = 0U; ii < allowed_envvars_size; ++ii) {
		char const *envvar_name = allowed_envvars[ii];

		char *envvar_value;
		{
			char *xx = 0;
			err = lntd_env_get(envvar_name, &xx);
			if (err != 0)
				goto free_result_envvars;
			envvar_value = xx;
		}
		if (0 == envvar_value)
			continue;

		++result_envvars_size;

		{
			char *xx = 0;
			err = lntd_str_format(&xx, "%s=%s", envvar_name,
			                      envvar_value);

			lntd_mem_free(envvar_value);
			envvar_value = 0;

			if (err != 0)
				goto free_result_envvars;

			LNTD_ASSERT(xx != 0);
			result_envvars[result_envvars_size - 1U] = xx;
		}
	}
	result_envvars[result_envvars_size] = 0;

	*result_envvarsp = result_envvars;

	return 0;

free_result_envvars:
	for (size_t ii = 0U; ii < result_envvars_size; ++ii) {
		lntd_mem_free(result_envvars[ii]);
		result_envvars[ii] = 0;
	}
	lntd_mem_free(result_envvars);
	result_envvars = 0;
	return err;
}

static size_t string_list_size(char const *const *list);

enum { PARSER_ERROR,
       PARSER_EOF,
       PARSER_EMPTY_LINE,
       PARSER_SECTION,
       PARSER_VALUE };

struct parser {
	FILE *file;
	char *line_buffer;
	size_t line_capacity;
	unsigned char state;

	char *section_name;
	char *field;
	wordexp_t expr;
};

struct parser_result_section {
	char const *section_name;
};

struct parser_result_value {
	char const *field;
	char const *const *value;
};

union parser_result_union {
	struct parser_result_section section;
	struct parser_result_value value;
};

struct parser_result {
	unsigned char state;
	union parser_result_union result_union;
};

static void parser_init(struct parser *parser, FILE *file);

static lntd_error parser_get_line(struct parser *parser,
                                  struct parser_result *resultp);

static lntd_error conf_parse_file(struct conf *conf, lntd_ko dir_ko,
                                  char const *file_name)
{
	lntd_error err = 0;

	conf_section current_section;

	lntd_ko conf_fd;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, dir_ko, file_name,
		                   LNTD_KO_RDONLY);
		if (err != 0)
			return err;
		conf_fd = xx;
	}

	FILE *conf_file = fdopen(conf_fd, "r");
	if (0 == conf_file) {
		err = errno;
		LNTD_ASSUME(err != 0);

		lntd_ko_close(conf_fd);

		return err;
	}

	struct parser parser;
	parser_init(&parser, conf_file);

	for (;;) {
		struct parser_result result;
		err = parser_get_line(&parser, &result);
		if (err != 0)
			break;
		switch (result.state) {
		case PARSER_EOF:
			goto free_parser;

		/* Ignore empty lines */
		case PARSER_EMPTY_LINE:
			continue;

		case PARSER_SECTION: {
			conf_section xx;
			err = conf_add_section(
			    conf, &xx,
			    result.result_union.section.section_name);
			if (err != 0)
				break;
			current_section = xx;
			break;
		}

		case PARSER_VALUE: {
			err = conf_add_setting(
			    conf, current_section,
			    result.result_union.value.field,
			    result.result_union.value.value);
			if (err != 0)
				break;
		}
		}
	}

free_parser:
	switch (parser.state) {
	case PARSER_VALUE:
		lntd_mem_free(parser.field);
		wordfree(&parser.expr);
		break;
	}
	lntd_mem_free(parser.section_name);
	lntd_mem_free(parser.line_buffer);

	if (err != 0)
		conf_put(conf);

	if (EOF == fclose(conf_file)) {
		if (0 == err) {
			err = errno;
			LNTD_ASSUME(err != 0);
		}
	}

	return err;
}

static char const *const *
conf_find(struct conf *conf, char const *section, char const *field);

static lntd_error conf_find_str(struct conf *conf, char const *section,
                                char const *field, char const **strp);

static lntd_error conf_find_int(struct conf *conf, char const *section,
                                char const *field, int_least64_t *intp);

static lntd_error conf_find_bool(struct conf *conf, char const *section,
                                 char const *field, _Bool *boolp);

static lntd_error conf_system_default_limit_locks(struct conf *conf,
                                                  int_least64_t *limitp)
{
	return conf_find_int(conf, "Manager", "DefaultLimitLOCKS",
	                     limitp);
}

static lntd_error
conf_system_default_limit_memlock(struct conf *conf,
                                  int_least64_t *limitp)
{
	return conf_find_int(conf, "Manager", "DefaultLimitMEMLOCK",
	                     limitp);
}

static lntd_error
conf_system_default_limit_msgqueue(struct conf *conf,
                                   int_least64_t *limitp)
{
	return conf_find_int(conf, "Manager", "DefaultLimitMSGQUEUE",
	                     limitp);
}

static lntd_error
conf_system_default_limit_nofile(struct conf *conf,
                                 int_least64_t *limitp)
{
	return conf_find_int(conf, "Manager", "DefaultLimitNOFILE",
	                     limitp);
}
static lntd_error
conf_system_default_no_new_privileges(struct conf *conf,
                                      bool *no_new_privs)
{
	return conf_find_bool(conf, "Manager",
	                      "X-LintedNoNewPrivileges", no_new_privs);
}
static lntd_error conf_system_default_seccomp(struct conf *conf,
                                              bool *seccomp)
{
	return conf_find_bool(conf, "Manager", "X-LintedSeccomp",
	                      seccomp);
}
static char const *const *
conf_system_default_pass_env(struct conf *conf)
{
	return conf_find(conf, "Manager", "X-LintedPassEnvironment");
}

static lntd_error conf_socket_listen_directory(struct conf *conf,
                                               char const **strp)
{
	return conf_find_str(conf, "Socket", "ListenDirectory", strp);
}

static lntd_error conf_socket_listen_file(struct conf *conf,
                                          char const **strp)
{
	return conf_find_str(conf, "Socket", "ListenFile", strp);
}

static lntd_error conf_socket_listen_fifo(struct conf *conf,
                                          char const **strp)
{
	return conf_find_str(conf, "Socket", "ListenFIFO", strp);
}

static lntd_error conf_socket_pipe_size(struct conf *conf,
                                        int_least64_t *intp)
{
	return conf_find_int(conf, "Socket", "PipeSize", intp);
}

static char const *const *conf_service_exec_start(struct conf *conf)
{
	return conf_find(conf, "Service", "ExecStart");
}
static char const *const *
conf_service_pass_environment(struct conf *conf)
{
	return conf_find(conf, "Service", "PassEnvironment");
}
static char const *const *conf_service_environment(struct conf *conf)
{
	return conf_find(conf, "Service", "Environment");
}
static char const *const *
conf_service_x_lntd_clone_flags(struct conf *conf)
{
	return conf_find(conf, "Service", "X-LintedCloneFlags");
}

static lntd_error conf_service_timer_slack_nsec(struct conf *conf,
                                                int_least64_t *intp)
{
	return conf_find_int(conf, "Service", "TimerSlackNSec", intp);
}
static lntd_error conf_service_priority(struct conf *conf,
                                        int_least64_t *intp)
{
	return conf_find_int(conf, "Service", "Nice", intp);
}
static lntd_error conf_service_limit_no_file(struct conf *conf,
                                             int_least64_t *intp)
{
	return conf_find_int(conf, "Service", "LimitNOFILE", intp);
}
static lntd_error conf_service_limit_msgqueue(struct conf *conf,
                                              int_least64_t *intp)
{
	return conf_find_int(conf, "Service", "LimitMSGQUEUE", intp);
}
static lntd_error conf_service_limit_locks(struct conf *conf,
                                           int_least64_t *intp)
{
	return conf_find_int(conf, "Service", "LimitLOCKS", intp);
}
static lntd_error conf_service_limit_memlock(struct conf *conf,
                                             int_least64_t *intp)
{
	return conf_find_int(conf, "Service", "LimitMEMLOCK", intp);
}

static lntd_error conf_service_type(struct conf *conf,
                                    char const **strp)
{
	return conf_find_str(conf, "Service", "Type", strp);
}
static lntd_error conf_service_no_new_privileges(struct conf *conf,
                                                 bool *boolp)
{
	return conf_find_bool(conf, "Service", "NoNewPrivileges",
	                      boolp);
}
static lntd_error conf_service_seccomp(struct conf *conf, bool *boolp)
{
	return conf_find_bool(conf, "Service", "Seccomp", boolp);
}
static lntd_error conf_service_x_lntd_fstab(struct conf *conf,
                                            char const **strp)
{
	return conf_find_str(conf, "Service", "X-LintedFstab", strp);
}
static lntd_error conf_service_working_directory(struct conf *conf,
                                                 char const **strp)
{
	return conf_find_str(conf, "Service", "WorkingDirectory", strp);
}

struct conf_db {
	struct conf **confs;
	size_t size;
};

static size_t string_hash(char const *str);

static lntd_error conf_db_create(struct conf_db **dbp)
{
	struct conf_db *db;
	{
		void *xx;
		lntd_error err = lntd_mem_alloc(&xx, sizeof *db);
		if (err != 0)
			return err;
		db = xx;
	}

	db->size = 0U;
	db->confs = 0;

	*dbp = db;

	return 0;
}

static void conf_db_destroy(struct conf_db *db)
{
	size_t size = db->size;
	struct conf **confs = db->confs;

	for (size_t ii = 0U; ii < size; ++ii)
		conf_put(confs[ii]);
	lntd_mem_free(confs);

	lntd_mem_free(db);
}

static lntd_error conf_db_add_conf(struct conf_db *db,
                                   struct conf *conf)
{
	struct conf **confs = db->confs;
	size_t confs_size = db->size;

	size_t new_confs_size = confs_size + 1U;
	struct conf **new_confs;
	{
		void *xx;
		lntd_error err = lntd_mem_realloc_array(
		    &xx, confs, new_confs_size, sizeof confs[0U]);
		if (err != 0)
			return err;
		new_confs = xx;
	}
	new_confs[confs_size] = conf;

	confs = new_confs;
	confs_size = new_confs_size;

	db->confs = confs;
	db->size = confs_size;

	return 0;
}

static size_t conf_db_size(struct conf_db *db)
{
	return db->size;
}

static struct conf *conf_db_get_conf(struct conf_db *db, size_t ii)
{
	return db->confs[ii];
}

struct conf_setting;

enum { SECTION_MANAGER,
       SECTION_UNIT,
       SECTION_SERVICE,
       SECTION_SOCKET,
       SECTION_COUNT };

#define SETTING_BUCKETS_SIZE 1024U

struct conf_setting_bucket {
	size_t settings_size;
	struct conf_setting *settings;
};

struct conf_section {
	size_t buckets_used;
	struct conf_setting_bucket buckets[SETTING_BUCKETS_SIZE];
};

struct conf {
	char *name;
	unsigned long refcount;
	struct conf_section sections[SECTION_COUNT];
};

static lntd_error conf_create(struct conf **confp, char *name)
{
	lntd_error err = 0;

	struct conf *conf;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *conf);
		if (err != 0)
			return err;
		conf = xx;
	}

	conf->name = name;
	conf->refcount = 1;

	for (size_t ii = 0U; ii < SECTION_COUNT; ++ii) {
		struct conf_section *section = &conf->sections[ii];

		section->buckets_used = 0U;

		for (size_t jj = 0U; jj < SETTING_BUCKETS_SIZE; ++jj) {
			struct conf_setting_bucket *bucket =
			    &section->buckets[jj];

			bucket->settings_size = 0U;
			bucket->settings = 0;
		}
	}

	*confp = conf;

	return 0;
}

static void free_settings(struct conf_setting *settings,
                          size_t settings_size);

static void conf_put(struct conf *conf)
{
	if (0 == conf)
		return;

	if (--conf->refcount != 0)
		return;

	for (size_t ii = 0U; ii < SECTION_COUNT; ++ii) {
		struct conf_section *section = &conf->sections[ii];

		struct conf_setting_bucket *buckets = section->buckets;

		for (size_t kk = 0U; kk < SETTING_BUCKETS_SIZE; ++kk) {
			struct conf_setting_bucket const *
			    setting_bucket = &buckets[kk];
			size_t settings_size =
			    setting_bucket->settings_size;
			struct conf_setting *settings =
			    setting_bucket->settings;

			free_settings(settings, settings_size);

			lntd_mem_free(settings);
		}
	}

	lntd_mem_free(conf->name);
	lntd_mem_free(conf);
}

static char const *conf_peek_name(struct conf *conf)
{
	return conf->name;
}

static lntd_error get_section(conf_section *sectionp, char const *name)
{
	conf_section section_id;
	if (0 == strcmp("Manager", name)) {
		section_id = SECTION_MANAGER;
	} else if (0 == strcmp("Unit", name)) {
		section_id = SECTION_UNIT;
	} else if (0 == strcmp("Service", name)) {
		section_id = SECTION_SERVICE;
	} else if (0 == strcmp("Socket", name)) {
		section_id = SECTION_SOCKET;
	} else {
		return ENOENT;
	}
	*sectionp = section_id;
	return 0;
}

static lntd_error conf_add_section(struct conf *conf,
                                   conf_section *sectionp,
                                   char const *section_name)
{
	return get_section(sectionp, section_name);
}

struct conf_setting {
	char *field;
	char **value;
};

static void free_settings(struct conf_setting *settings,
                          size_t settings_size)
{
	for (size_t ww = 0U; ww < settings_size; ++ww) {
		struct conf_setting *setting = &settings[ww];
		lntd_mem_free(setting->field);

		for (char **value = setting->value; *value != 0;
		     ++value)
			lntd_mem_free(*value);
		lntd_mem_free(setting->value);
	}
}

static char const *const *conf_find(struct conf *conf,
                                    char const *section_name,
                                    char const *field)
{
	lntd_error err = 0;

	conf_section section_id;
	err = get_section(&section_id, section_name);
	if (err != 0)
		return 0;

	struct conf_section *section = &conf->sections[section_id];

	struct conf_setting_bucket *buckets = section->buckets;
	struct conf_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct conf_setting *settings = bucket->settings;

	size_t setting_index;
	bool have_found_setting = false;
	for (size_t ii = 0U; ii < settings_size; ++ii) {
		if (0 == strcmp(settings[ii].field, field)) {
			have_found_setting = true;
			setting_index = ii;
			break;
		}
	}

	if (!have_found_setting)
		return 0;

	return (char const *const *)settings[setting_index].value;
}

static lntd_error conf_find_str(struct conf *conf, char const *section,
                                char const *field, char const **strp)
{
	char const *const *strs = conf_find(conf, section, field);
	if (0 == strs)
		return ENOENT;

	char const *str = strs[0U];
	if (0 == str)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (strs[1U] != 0)
		return LNTD_ERROR_INVALID_PARAMETER;

	*strp = str;
	return 0;
}

static lntd_error conf_find_int(struct conf *conf, char const *section,
                                char const *field, int_least64_t *intp)
{
	char const *const *strs = conf_find(conf, section, field);
	if (0 == strs)
		return ENOENT;

	char const *str = strs[0U];
	if (0 == str)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (strs[1U] != 0)
		return LNTD_ERROR_INVALID_PARAMETER;

	*intp = atoi(str);
	return 0;
}

static lntd_error conf_find_bool(struct conf *conf, char const *section,
                                 char const *field, _Bool *boolp)
{
	char const *const *strs = conf_find(conf, section, field);
	if (0 == strs)
		return ENOENT;

	char const *str = strs[0U];
	if (0 == str)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (strs[1U] != 0)
		return LNTD_ERROR_INVALID_PARAMETER;

	static char const *const yes_strs[] = {"1", "yes", "true",
	                                       "on"};
	static char const *const no_strs[] = {"0", "no", "false",
	                                      "off"};

	bool result;
	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(yes_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = true;
			goto return_result;
		}
	}

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(no_strs); ++ii) {
		if (0 == strcmp(str, no_strs[ii])) {
			result = false;
			goto return_result;
		}
	}

	return LNTD_ERROR_INVALID_PARAMETER;

return_result:
	*boolp = result;
	return 0;
}

static lntd_error conf_add_setting(struct conf *conf,
                                   conf_section section,
                                   char const *field,
                                   char const *const *additional_values)
{
	lntd_error err;

	struct conf_setting_bucket *buckets =
	    conf->sections[section].buckets;

	struct conf_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct conf_setting *settings = bucket->settings;

	size_t additional_values_len =
	    string_list_size(additional_values);

	{
		size_t found_field;
		bool have_found_field = false;
		for (size_t ii = 0U; ii < settings_size; ++ii) {
			if (0 == strcmp(settings[ii].field, field)) {
				found_field = ii;
				have_found_field = true;
				break;
			}
		}

		if (!have_found_field)
			goto have_not_found_field;

		struct conf_setting *setting = &settings[found_field];
		char *setting_field = setting->field;
		char **setting_values = setting->value;

		if (0U == additional_values_len) {
			lntd_mem_free(setting_field);

			for (size_t ii = 0U; setting_values[ii] != 0;
			     ++ii)
				lntd_mem_free(setting_values[ii]);

			lntd_mem_free(setting_values);

			bucket->settings_size = settings_size - 1U;
			memcpy(bucket->settings + found_field,
			       buckets->settings + found_field + 1U,
			       (settings_size - 1U - found_field) *
			           sizeof bucket->settings[0U]);
		} else {
			size_t setting_values_len = string_list_size(
			    (char const *const *)setting_values);

			size_t new_value_len =
			    setting_values_len + additional_values_len;

			char **new_value;
			{
				void *xx;
				err = lntd_mem_alloc_array(
				    &xx, new_value_len + 1U,
				    sizeof additional_values[0U]);
				if (err != 0)
					return err;
				new_value = xx;
			}

			for (size_t ii = 0U; ii < setting_values_len;
			     ++ii)
				new_value[ii] = setting_values[ii];

			for (size_t ii = 0U; ii < additional_values_len;
			     ++ii) {
				char *copy;
				{
					char *xx;
					err = lntd_str_dup(
					    &xx, additional_values[ii]);
					if (err != 0) {
						for (; ii != 0; --ii)
							lntd_mem_free(
							    new_value
							        [ii -
							         1U]);

						lntd_mem_free(
						    new_value);
						return err;
					}
					copy = xx;
				}
				new_value[setting_values_len + ii] =
				    copy;
			}

			new_value[new_value_len] = 0;

			lntd_mem_free(setting_values);

			setting->field = setting_field;
			setting->value = new_value;
		}
		return 0;
	}
have_not_found_field:
	;
	char *field_dup;
	{
		char *xx;
		err = lntd_str_dup(&xx, field);
		if (err != 0)
			return err;
		field_dup = xx;
	}

	char **value_copy;
	{
		void *xx;
		err = lntd_mem_alloc_array(
		    &xx, additional_values_len + 1U,
		    sizeof additional_values[0U]);
		if (err != 0)
			goto free_field_dup;
		value_copy = xx;
	}
	size_t values = 0U;
	for (; values < additional_values_len; ++values) {
		char *copy;
		{
			char *xx;
			err = lntd_str_dup(&xx,
			                   additional_values[values]);
			if (err != 0)
				goto free_value_copy;
			copy = xx;
		}
		value_copy[values] = copy;
	}
	value_copy[additional_values_len] = 0;

	size_t new_settings_size = settings_size + 1U;
	struct conf_setting *new_settings;
	{
		void *xx;
		err = lntd_mem_realloc_array(&xx, settings,
		                             new_settings_size,
		                             sizeof settings[0U]);
		if (err != 0) {
			for (size_t ii = 0U; value_copy[ii] != 0; ++ii)
				lntd_mem_free(value_copy[ii]);
			lntd_mem_free(value_copy);

			return err;
		}
		new_settings = xx;
	}
	new_settings[settings_size].field = field_dup;
	new_settings[settings_size].value = value_copy;

	bucket->settings_size = new_settings_size;
	bucket->settings = new_settings;

	return 0;

free_value_copy:
	for (size_t jj = 0U; jj < values; ++jj)
		lntd_mem_free(value_copy[jj]);
	lntd_mem_free(value_copy);

free_field_dup:
	lntd_mem_free(field_dup);
	return err;
}

static size_t string_list_size(char const *const *list)
{
	size_t ii = 0U;
	for (;; ++ii) {
		if (0 == list[ii])
			break;
	}
	return ii;
}

static size_t string_hash(char const *str)
{
	size_t hash = 0U;
	for (size_t ii = 0U; str[ii] != '\0'; ++ii)
		hash = hash * 31U + (unsigned)str[ii];
	return hash;
}

static void parser_init(struct parser *parser, FILE *file)
{
	parser->file = file;
	parser->line_buffer = 0;
	parser->line_capacity = 0U;
	parser->section_name = 0;
	parser->state = PARSER_ERROR;
}

static lntd_error parser_get_line(struct parser *parser,
                                  struct parser_result *result)
{
	unsigned char state;
	lntd_error err = 0;

	char *line_buffer = parser->line_buffer;
	size_t line_capacity = parser->line_capacity;
	FILE *file = parser->file;

	switch (parser->state) {
	case PARSER_VALUE:
		lntd_mem_free(parser->field);
		wordfree(&parser->expr);
		break;
	}

	ssize_t zz;
	{
		char *xx = line_buffer;
		size_t yy = line_capacity;

		errno = 0;
		zz = getline(&xx, &yy, file);
		if (zz < 0) {
			/* May be 0 to indicate end of line */
			err = errno;
			if (0 == err) {
				state = PARSER_EOF;
			} else {
				state = PARSER_ERROR;
			}
			goto set_values;
		}

		line_buffer = xx;
		line_capacity = yy;
	}
	size_t line_size = zz;
	if (0 == line_size) {
		state = PARSER_EOF;
		goto set_values;
	}

	if ('\n' == line_buffer[line_size - 1U])
		--line_size;

	if (0U == line_size) {
		state = PARSER_EMPTY_LINE;
		goto set_values;
	}

	switch (line_buffer[0U]) {
	/* Ignore comments */
	case ';':
	case '#':
		state = PARSER_EMPTY_LINE;
		break;

	case '[': {
		if (line_buffer[line_size - 1U] != ']') {
			err = LNTD_ERROR_INVALID_PARAMETER;
			state = PARSER_ERROR;
			break;
		}

		char *section_name;
		{
			char *xx;
			err = lntd_str_dup_len(&xx, line_buffer + 1U,
			                       line_size - 2U);
			if (err != 0) {
				state = PARSER_ERROR;
				break;
			}
			section_name = xx;
		}

		lntd_mem_free(parser->section_name);
		parser->section_name = section_name;

		result->result_union.section.section_name =
		    section_name;

		state = PARSER_SECTION;
		break;
	}

	default: {
		if (0 == parser->section_name) {
			err = LNTD_ERROR_INVALID_PARAMETER;
			state = PARSER_ERROR;
			break;
		}

		size_t equals_position;
		size_t whitespace_position;
		size_t field_len;
		for (size_t ii = 0U; ii < line_size; ++ii) {
			switch (line_buffer[ii]) {
			case '=':
				equals_position = ii;
				field_len = ii;
				goto found_equal;

			case ' ':
			case '\t':
				if (0U == ii) {
					err =
					    LNTD_ERROR_INVALID_PARAMETER;
					state = PARSER_ERROR;
					break;
				}

				whitespace_position = ii;
				field_len = ii;
				goto found_whitespace;

			case '\n':
				err = LNTD_ERROR_INVALID_PARAMETER;
				state = PARSER_ERROR;
				break;

			default:
				break;
			}
			if (err != 0) {
				state = PARSER_ERROR;
				break;
			}
		}
		err = LNTD_ERROR_INVALID_PARAMETER;
		state = PARSER_ERROR;
		break;

	found_whitespace:
		for (size_t ii = whitespace_position + 1U;
		     ii < line_size; ++ii) {
			switch (line_buffer[ii]) {
			case '=':
				equals_position = ii;
				goto found_equal;

			case ' ':
			case '\t':
				break;

			default:
				err = LNTD_ERROR_INVALID_PARAMETER;
				state = PARSER_ERROR;
				break;
			}
			if (err != 0)
				break;
		}
		if (err != 0)
			break;

		err = LNTD_ERROR_INVALID_PARAMETER;
		state = PARSER_ERROR;
		break;

	found_equal:
		;
		size_t value_offset = equals_position + 1U;
		size_t value_len = line_size - value_offset;

		char *field;
		{
			void *xx;
			err = lntd_mem_alloc(&xx, field_len + 1U);
			if (err != 0) {
				state = PARSER_ERROR;
				break;
			}
			field = xx;
		}
		if (field_len > 0U)
			memcpy(field, line_buffer, field_len);
		field[field_len] = '\0';

		char *value;
		{
			void *xx;
			err = lntd_mem_alloc(&xx, value_len + 1U);
			if (err != 0) {
				lntd_mem_free(field);
				state = PARSER_ERROR;
				break;
			}
			value = xx;
		}
		memcpy(value, line_buffer + value_offset, value_len);
		value[value_len] = '\0';

		wordexp_t expr;
		switch (wordexp(value, &expr, WRDE_NOCMD)) {
		case WRDE_BADCHAR:
		case WRDE_CMDSUB:
		case WRDE_SYNTAX:
			err = LNTD_ERROR_INVALID_PARAMETER;
			break;

		case WRDE_NOSPACE:
			err = LNTD_ERROR_OUT_OF_MEMORY;
			break;
		}

		lntd_mem_free(value);

		if (err != 0) {
			lntd_mem_free(field);

			wordfree(&expr);
			state = PARSER_ERROR;
			break;
		}

		parser->field = field;
		parser->expr = expr;

		result->result_union.value.field = field;
		result->result_union.value.value =
		    (char const *const *)expr.we_wordv;

		state = PARSER_VALUE;
		break;
	}
	}

set_values:
	parser->line_buffer = line_buffer;
	parser->line_capacity = line_capacity;
	parser->state = state;

	result->state = state;

	return err;
}
