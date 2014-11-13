/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/conf.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <wordexp.h>

struct conf_setting;

struct conf_section_bucket
{
	size_t sections_size;
	struct linted_conf_section *sections;
};

#define SECTION_BUCKETS_SIZE 1024U

struct linted_conf
{
	char *name;
	unsigned long refcount;
	struct conf_section_bucket buckets[SECTION_BUCKETS_SIZE];
};

#define SETTING_BUCKETS_SIZE 1024U

struct conf_setting_bucket
{
	size_t settings_size;
	struct conf_setting *settings;
};

struct linted_conf_section
{
	unsigned long refcount;
	char *name;
	struct conf_setting_bucket buckets[SETTING_BUCKETS_SIZE];
};

struct conf_setting
{
	char *field;
	char **value;
};

struct linted_conf_db
{
	struct linted_conf **confs;
	size_t size;
};

static size_t string_hash(char const *str);

linted_error linted_conf_db_create_from_path(struct linted_conf_db **dbp,
                                             char const *path)
{
	linted_ko errnum = 0;

	struct linted_conf_db *db;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *db);
		if (errnum != 0)
			return errnum;
		db = xx;
	}

	struct linted_conf **units = NULL;
	size_t units_size = 0U;

	char const *dirstart = path;
	for (;;) {
		char const *dirend = strchr(dirstart, ':');

		char *dir_name;
		if (NULL == dirend) {
			dir_name = strdup(dirstart);
		} else {
			dir_name = strndup(dirstart, dirend - dirstart);
		}

		if (NULL == dir_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_units;
		}

		DIR *units_dir = opendir(dir_name);
		if (NULL == units_dir) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}

		linted_mem_free(dir_name);

		if (ENOENT == errnum) {
			errnum = 0;
			goto next_dir;
		}

		if (errnum != 0)
			goto free_units;

		linted_ko dirko = dirfd(units_dir);

		size_t files_count = 0U;
		char **files = NULL;
		for (;;) {
			errno = 0;
			struct dirent const *entry = readdir(units_dir);
			if (NULL == entry) {
				errnum = errno;
				if (0 == errnum)
					break;

				goto free_file_names;
			}

			char const *name = entry->d_name;

			if (0 == strcmp(".", name))
				continue;

			if (0 == strcmp("..", name))
				continue;

			char *name_copy = strdup(name);
			if (NULL == name_copy) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_file_names;
			}

			size_t new_files_count = files_count + 1U;
			char **new_files;
			{
				void *xx;
				errnum = linted_mem_realloc_array(
				    &xx, files, new_files_count,
				    sizeof files[0U]);
				if (errnum != 0)
					goto free_file_name;
				new_files = xx;
			}
			new_files[files_count] = name_copy;

			files = new_files;
			files_count = new_files_count;

			if (errnum != 0) {
			free_file_name:
				linted_mem_free(name_copy);
				goto free_file_names;
			}
		}

		for (size_t ii = 0U; ii < files_count; ++ii) {
			char const *file_name = files[ii];

			linted_ko unit_fd;
			{
				linted_ko xx;
				errnum = linted_ko_open(&xx, dirko, file_name,
				                        LINTED_KO_RDONLY);
				if (errnum != 0)
					goto free_file_names;
				unit_fd = xx;
			}

			FILE *unit_file = fdopen(unit_fd, "r");
			if (NULL == unit_file) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);

				linted_ko_close(unit_fd);

				goto free_file_names;
			}

			struct linted_conf *unit = NULL;
			{
				struct linted_conf *xx;
				errnum = linted_conf_parse_file(&xx, unit_file,
				                                file_name);
				if (errnum != 0)
					goto close_unit_file;
				unit = xx;
			}

		close_unit_file:
			if (EOF == fclose(unit_file)) {
				if (0 == errnum) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
				}
			}

			if (errnum != 0)
				goto free_unit;

			size_t new_units_size = units_size + 1U;
			struct linted_conf **new_units;
			{
				void *xx;
				errnum = linted_mem_realloc_array(
				    &xx, units, new_units_size,
				    sizeof units[0U]);
				if (errnum != 0)
					goto free_unit;
				new_units = xx;
			}
			new_units[units_size] = unit;

			units = new_units;
			units_size = new_units_size;

		free_unit:
			if (errnum != 0)
				linted_conf_put(unit);
		}

	free_file_names:
		for (size_t ii = 0U; ii < files_count; ++ii)
			linted_mem_free(files[ii]);
		linted_mem_free(files);

		if (-1 == closedir(units_dir)) {
			if (0 == errnum) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			}
		}
		if (errnum != 0)
			goto free_units;

	next_dir:
		if (NULL == dirend)
			break;

		dirstart = dirend + 1U;
	}

free_units:
	if (errnum != 0) {
		for (size_t ii = 0U; ii < units_size; ++ii)
			linted_conf_put(units[ii]);
		linted_mem_free(units);

		linted_mem_free(db);
	} else {
		db->size = units_size;
		db->confs = units;

		*dbp = db;
	}

	return errnum;
}

void linted_conf_db_destroy(struct linted_conf_db *db)
{
	for (size_t ii = 0U; ii < db->size; ++ii)
		linted_conf_put(db->confs[ii]);
	linted_mem_free(db->confs);

	linted_mem_free(db);
}

size_t linted_conf_db_size(struct linted_conf_db *db)
{
	return db->size;
}

struct linted_conf *linted_conf_db_get_conf(struct linted_conf_db *db,
                                            size_t ii)
{
	return db->confs[ii];
}

linted_error linted_conf_parse_file(struct linted_conf **confp, FILE *conf_file,
                                    char const *name)
{
	linted_error errnum = 0;

	char *line_buffer = NULL;
	size_t line_capacity = 0U;

	struct linted_conf *conf;
	{
		struct linted_conf *xx;
		errnum = linted_conf_create(&xx, name);
		if (errnum != 0)
			return errnum;
		conf = xx;
	}

	struct linted_conf_section *current_section = NULL;

	for (;;) {
		size_t line_size;
		{
			char *xx = line_buffer;
			size_t yy = line_capacity;
			errno = 0;
			ssize_t zz = getline(&xx, &yy, conf_file);
			if (-1 == zz)
				goto getline_failed;
			line_buffer = xx;
			line_capacity = yy;
			line_size = zz;
			goto getline_succeeded;
		}
	getline_failed:
		errnum = errno;
		/* May be 0 to indicate end of line */
		break;

	getline_succeeded:
		if (0U == line_size)
			break;

		if ('\n' == line_buffer[line_size - 1U])
			--line_size;

		/* Ignore empty lines */
		if (0U == line_size)
			continue;

		switch (line_buffer[0U]) {
		/* Ignore comments */
		case ';':
		case '#':
			continue;

		/* A section start */
		case '[': {
			if (line_buffer[line_size - 1U] != ']') {
				errnum = EINVAL;
				goto free_line_buffer;
			}

			char *section_name;
			{
				void *xx;
				errnum = linted_mem_alloc(&xx, line_size - 1U);
				if (errnum != 0)
					goto free_line_buffer;
				section_name = xx;
			}
			section_name[line_size - 2U] = '\0';
			memcpy(section_name, line_buffer + 1U, line_size - 2U);

			{
				struct linted_conf_section *xx;
				errnum = linted_conf_add_section(conf, &xx,
				                                 section_name);
				if (0 == errnum)
					current_section = xx;
			}

			if (errnum != 0) {
				linted_mem_free(section_name);
				goto free_line_buffer;
			}
			break;
		}

		default: {
			if (NULL == current_section) {
				errnum = EINVAL;
				goto free_line_buffer;
			}

			bool has_equals_sign = false;
			bool has_whitespace = false;
			size_t equals_position;
			size_t whitespace_position;
			size_t field_len;
			for (size_t ii = 0U; ii < line_size; ++ii) {
				switch (line_buffer[ii]) {
				case '=':
					has_equals_sign = true;
					equals_position = ii;
					field_len = ii;
					goto exit_whitespace_or_equals_search;

				case ' ':
				case '\t':
					if (0U == ii) {
						errnum = EINVAL;
						goto free_line_buffer;
					}

					has_whitespace = true;
					whitespace_position = ii;
					field_len = ii;
					goto exit_whitespace_or_equals_search;

				case '\n':
					errnum = EINVAL;
					goto free_line_buffer;
				}
			}
		exit_whitespace_or_equals_search:

			if (has_whitespace) {
				for (size_t ii = whitespace_position + 1U;
				     ii < line_size; ++ii) {
					switch (line_buffer[ii]) {
					case '=':
						has_equals_sign = true;
						equals_position = ii;
						goto exit_equals_search;

					case ' ':
					case '\t':
						break;

					default:
						errnum = EINVAL;
						goto free_line_buffer;
					}
				}
			}
		exit_equals_search:

			if (!has_equals_sign) {
				errnum = EINVAL;
				goto free_line_buffer;
			}

			size_t value_offset = equals_position + 1U;
			size_t value_len = line_size - value_offset;

			char *field;
			{
				void *xx;
				errnum = linted_mem_alloc(&xx, field_len + 1U);
				if (errnum != 0)
					goto free_line_buffer;
				field = xx;
			}
			memcpy(field, line_buffer, field_len);
			field[field_len] = '\0';

			char *value;
			{
				void *xx;
				errnum = linted_mem_alloc(&xx, value_len + 1U);
				if (errnum != 0)
					goto free_field;
				value = xx;
			}
			memcpy(value, line_buffer + value_offset, value_len);
			value[value_len] = '\0';

			wordexp_t expr;
			switch (wordexp(value, &expr, WRDE_NOCMD)) {
			case WRDE_BADCHAR:
			case WRDE_CMDSUB:
			case WRDE_SYNTAX:
				errnum = EINVAL;
				break;

			case WRDE_NOSPACE:
				errnum = ENOMEM;
				break;
			}

			linted_mem_free(value);

			if (errnum != 0)
				goto free_field;

			errnum = linted_conf_add_setting(
			    current_section, field,
			    (char const * const *)expr.we_wordv);

			wordfree(&expr);

		free_field:
			if (errnum != 0)
				linted_mem_free(field);

			if (errnum != 0)
				goto free_line_buffer;
		}
		}
	}

free_line_buffer:
	linted_mem_free(line_buffer);

	if (errnum != 0)
		linted_conf_put(conf);

	if (0 == errnum)
		*confp = conf;

	return errnum;
}

linted_error linted_conf_create(struct linted_conf **confp, char const *name)
{
	linted_error errnum = 0;
	struct linted_conf *conf;

	char *name_copy = strdup(name);
	if (NULL == name_copy) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *conf);
		if (errnum != 0)
			goto free_name_copy;
		conf = xx;
	}

	conf->name = name_copy;
	conf->refcount = 1;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct conf_section_bucket *bucket = &conf->buckets[ii];

		bucket->sections_size = 0U;
		bucket->sections = NULL;
	}

free_name_copy:
	if (errnum != 0) {
		linted_mem_free(name_copy);
		return errnum;
	}

	*confp = conf;

	return 0;
}

void linted_conf_put(struct linted_conf *conf)
{
	if (NULL == conf)
		return;

	if (--conf->refcount != 0)
		return;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct conf_section_bucket const *bucket = &conf->buckets[ii];

		size_t sections_size = bucket->sections_size;
		struct linted_conf_section *sections = bucket->sections;

		for (size_t jj = 0U; jj < sections_size; ++jj) {
			struct linted_conf_section const *section =
			    &sections[jj];

			for (size_t kk = 0U; kk < SETTING_BUCKETS_SIZE; ++kk) {
				struct conf_setting_bucket const *
				setting_bucket = &section->buckets[kk];
				size_t settings_size =
				    setting_bucket->settings_size;
				struct conf_setting *settings =
				    setting_bucket->settings;

				for (size_t ww = 0U; ww < settings_size; ++ww) {
					struct conf_setting *setting =
					    &settings[ww];
					linted_mem_free(setting->field);

					for (char **value = setting->value;
					     *value != NULL; ++value)
						linted_mem_free(*value);
					linted_mem_free(setting->value);
				}

				linted_mem_free(settings);
			}
			linted_mem_free(section->name);
		}

		linted_mem_free(sections);
	}
	linted_mem_free(conf->name);
	linted_mem_free(conf);
}

char const *linted_conf_peek_name(struct linted_conf *conf)
{
	return conf->name;
}

linted_error linted_conf_add_section(struct linted_conf *conf,
                                     struct linted_conf_section **sectionp,
                                     char *section_name)
{
	linted_error errnum;

	struct conf_section_bucket *buckets = conf->buckets;

	struct conf_section_bucket *bucket =
	    &buckets[string_hash(section_name) % SECTION_BUCKETS_SIZE];

	size_t sections_size = bucket->sections_size;
	struct linted_conf_section *sections = bucket->sections;

	bool have_found_field = false;
	size_t found_field;
	for (size_t ii = 0U; ii < sections_size; ++ii) {
		if (0 == strcmp(sections[ii].name, section_name)) {
			have_found_field = true;
			found_field = ii;
			break;
		}
	}

	if (have_found_field) {
		linted_mem_free(section_name);
		*sectionp = &sections[found_field];
	} else {
		size_t new_sections_size = sections_size + 1U;
		struct linted_conf_section *new_sections;
		{
			void *xx;
			errnum = linted_mem_realloc_array(&xx, sections,
			                                  new_sections_size,
			                                  sizeof sections[0U]);
			if (errnum != 0)
				return errnum;
			new_sections = xx;
		}

		struct linted_conf_section *new_section =
		    &new_sections[sections_size];

		new_section->name = section_name;

		for (size_t ii = 0U; ii < SETTING_BUCKETS_SIZE; ++ii) {
			new_section->buckets[ii].settings_size = 0U;
			new_section->buckets[ii].settings = NULL;
		}

		bucket->sections_size = new_sections_size;
		bucket->sections = new_sections;

		*sectionp = new_section;
	}

	return 0;
}

char const *const *linted_conf_find(struct linted_conf *conf,
                                    char const *section, char const *field)
{
	struct linted_conf_section *found_section;

	{
		struct conf_section_bucket *buckets = conf->buckets;
		struct conf_section_bucket *bucket =
		    &buckets[string_hash(section) % SECTION_BUCKETS_SIZE];

		size_t sections_size = bucket->sections_size;
		struct linted_conf_section *sections = bucket->sections;

		bool have_found_section = false;
		for (size_t ii = 0U; ii < sections_size; ++ii) {
			if (0 == strcmp(sections[ii].name, section)) {
				have_found_section = true;
				found_section = &sections[ii];
				break;
			}
		}

		if (!have_found_section)
			return NULL;
	}

	struct conf_setting_bucket *buckets = found_section->buckets;
	struct conf_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct conf_setting *settings = bucket->settings;

	struct conf_setting *found_setting;
	bool have_found_setting = false;
	for (size_t ii = 0U; ii < settings_size; ++ii) {
		if (0 == strcmp(settings[ii].field, field)) {
			have_found_setting = true;
			found_setting = &settings[ii];
			break;
		}
	}

	if (!have_found_setting)
		return NULL;

	return (char const * const *)found_setting->value;
}

linted_error linted_conf_add_setting(struct linted_conf_section *section,
                                     char *field, char const *const *value)
{
	linted_error errnum;

	struct conf_setting_bucket *buckets = section->buckets;

	struct conf_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct conf_setting *settings = bucket->settings;

	size_t found_field;
	for (size_t ii = 0U; ii < settings_size; ++ii) {
		if (0 == strcmp(settings[ii].field, field)) {
			found_field = ii;
			goto found_field;
		}
	}

	{
		size_t new_settings_size = settings_size + 1U;
		struct conf_setting *new_settings;
		{
			void *xx;
			errnum = linted_mem_realloc_array(&xx, settings,
			                                  new_settings_size,
			                                  sizeof settings[0U]);
			if (errnum != 0)
				return errnum;
			new_settings = xx;
		}

		size_t value_len;
		for (size_t ii = 0U;; ++ii) {
			if (NULL == value[ii]) {
				value_len = ii;
				break;
			}
		}

		char **value_copy;
		{
			void *xx;
			errnum = linted_mem_alloc_array(&xx, value_len + 1U,
			                                sizeof value[0U]);
			if (errnum != 0)
				return errnum;
			value_copy = xx;
		}
		for (size_t ii = 0U; ii < value_len; ++ii)
			value_copy[ii] = strdup(value[ii]);
		value_copy[value_len] = NULL;

		new_settings[settings_size].field = field;
		new_settings[settings_size].value = value_copy;

		bucket->settings_size = new_settings_size;
		bucket->settings = new_settings;

		return 0;
	}

found_field:
	if (NULL == value[0U]) {
		linted_mem_free(field);

		linted_mem_free(settings[found_field].field);

		char **values = settings[found_field].value;

		for (size_t ii = 0U; values[ii] != NULL; ++ii)
			linted_mem_free(values[ii]);

		linted_mem_free(values);

		bucket->settings_size = settings_size - 1U;
		memcpy(bucket->settings + found_field,
		       buckets->settings + found_field + 1U,
		       (settings_size - 1U - found_field) *
		           sizeof bucket->settings[0U]);
	} else {
		char **old_value = settings[found_field].value;

		size_t old_value_len;
		for (size_t ii = 0U;; ++ii) {
			if (NULL == old_value[ii]) {
				old_value_len = ii;
				break;
			}
		}

		size_t value_len;
		for (size_t ii = 0U;; ++ii) {
			if (NULL == value[ii]) {
				value_len = ii;
				break;
			}
		}

		size_t new_value_len = old_value_len + value_len;

		char **new_value;
		{
			void *xx;
			errnum = linted_mem_alloc_array(&xx, new_value_len + 1U,
			                                sizeof value[0U]);
			if (errnum != 0)
				return errnum;
			new_value = xx;
		}

		for (size_t ii = 0U; ii < old_value_len; ++ii)
			new_value[ii] = old_value[ii];

		for (size_t ii = 0U; ii < value_len; ++ii) {
			char *copy = strdup(value[ii]);
			if (NULL == copy) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				for (; ii != 0; --ii)
					linted_mem_free(new_value[ii - 1U]);

				linted_mem_free(new_value);
				return errnum;
			}
			new_value[old_value_len + ii] = copy;
		}

		new_value[new_value_len] = NULL;

		linted_mem_free(settings[found_field].field);
		linted_mem_free(old_value);

		settings[found_field].field = field;
		settings[found_field].value = new_value;
	}

	return 0;
}

static size_t string_hash(char const *str)
{
	size_t hash = 0U;
	for (size_t ii = 0U; str[ii] != '\0'; ++ii)
		hash = hash * 31 + str[ii];
	return hash;
}
