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

#include "linted/conf.h"
#include "linted/mem.h"
#include "linted/str.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <wordexp.h>

static size_t string_list_size(char const *const *list);

linted_error linted_conf_parse_file(struct linted_conf *conf,
                                    linted_ko dir_ko,
                                    char const *file_name)
{
	linted_error err = 0;

	char *line_buffer = 0;
	size_t line_capacity = 0U;

	linted_conf_section current_section;
	bool has_current_section = false;

	linted_ko unit_fd;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, dir_ko, file_name,
		                     LINTED_KO_RDONLY);
		if (err != 0)
			return err;
		unit_fd = xx;
	}

	FILE *conf_file = fdopen(unit_fd, "r");
	if (0 == conf_file) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(unit_fd);

		return err;
	}

	for (;;) {
		size_t line_size;
		ssize_t zz;
		{
			char *xx = line_buffer;
			size_t yy = line_capacity;
			errno = 0;
			zz = getline(&xx, &yy, conf_file);
			line_buffer = xx;
			line_capacity = yy;
			line_size = zz;
		}
		if (zz < 0) {
			err = errno;
			/* May be 0 to indicate end of line */
			break;
		}

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
				err = EINVAL;
				break;
			}

			char *section_name;
			{
				char *xx;
				err = linted_str_dup_len(
				    &xx, line_buffer + 1U,
				    line_size - 2U);
				if (err != 0)
					break;
				section_name = xx;
			}

			{
				linted_conf_section xx;
				err = linted_conf_add_section(
				    conf, &xx, section_name);
				if (err != 0)
					goto free_section_name;
				current_section = xx;
			}
			has_current_section = true;
			break;

		free_section_name:
			linted_mem_free(section_name);
			break;
		}

		default: {
			if (!has_current_section) {
				err = EINVAL;
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
						err = EINVAL;
						break;
					}

					whitespace_position = ii;
					field_len = ii;
					goto found_whitespace;

				case '\n':
					err = EINVAL;
					break;

				default:
					break;
				}
				if (err != 0)
					break;
			}
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
					err = EINVAL;
					break;
				}
				if (err != 0)
					break;
			}
			if (err != 0)
				break;

			err = EINVAL;
			break;

		found_equal:
			;
			size_t value_offset = equals_position + 1U;
			size_t value_len = line_size - value_offset;

			char *field;
			{
				void *xx;
				err = linted_mem_alloc(&xx,
				                       field_len + 1U);
				if (err != 0)
					break;
				field = xx;
			}
			if (field_len > 0U)
				memcpy(field, line_buffer, field_len);
			field[field_len] = '\0';

			char *value;
			{
				void *xx;
				err = linted_mem_alloc(&xx,
				                       value_len + 1U);
				if (err != 0)
					goto free_field;
				value = xx;
			}
			memcpy(value, line_buffer + value_offset,
			       value_len);
			value[value_len] = '\0';

			wordexp_t expr;
			switch (wordexp(value, &expr, WRDE_NOCMD)) {
			case WRDE_BADCHAR:
			case WRDE_CMDSUB:
			case WRDE_SYNTAX:
				err = EINVAL;
				break;

			case WRDE_NOSPACE:
				err = ENOMEM;
				break;
			}

			linted_mem_free(value);

			if (err != 0)
				goto free_field;

			err = linted_conf_add_setting(
			    conf, current_section, field,
			    (char const *const *)expr.we_wordv);

			wordfree(&expr);

		free_field:
			if (err != 0)
				linted_mem_free(field);

			if (err != 0)
				break;
		}
		}
	}

	linted_mem_free(line_buffer);

	if (err != 0)
		linted_conf_put(conf);

	if (EOF == fclose(conf_file)) {
		if (0 == err) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}
	}

	return err;
}

struct linted_conf_db {
	struct linted_conf **confs;
	size_t size;
};

static size_t string_hash(char const *str);

linted_error linted_conf_db_add_conf(struct linted_conf_db *db,
                                     struct linted_conf *conf)
{
	struct linted_conf **units = db->confs;
	size_t units_size = db->size;

	size_t new_units_size = units_size + 1U;
	struct linted_conf **new_units;
	{
		void *xx;
		linted_error err = linted_mem_realloc_array(
		    &xx, units, new_units_size, sizeof units[0U]);
		if (err != 0)
			return err;
		new_units = xx;
	}
	new_units[units_size] = conf;

	units = new_units;
	units_size = new_units_size;

	db->confs = units;
	db->size = units_size;

	return 0;
}

linted_error linted_conf_db_create(struct linted_conf_db **dbp)
{
	struct linted_conf_db *db;
	{
		void *xx;
		linted_error err = linted_mem_alloc(&xx, sizeof *db);
		if (err != 0)
			return err;
		db = xx;
	}

	db->size = 0U;
	db->confs = 0;

	*dbp = db;

	return 0;
}

void linted_conf_db_destroy(struct linted_conf_db *db)
{
	size_t size = db->size;
	struct linted_conf **confs = db->confs;

	for (size_t ii = 0U; ii < size; ++ii)
		linted_conf_put(confs[ii]);
	linted_mem_free(confs);

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

struct conf_setting;

struct conf_section_bucket {
	size_t sections_size;
	struct conf_section *sections;
};

#define SECTION_BUCKETS_SIZE 1024U

struct linted_conf {
	char *name;
	unsigned long refcount;
	struct conf_section_bucket buckets[SECTION_BUCKETS_SIZE];
};

struct conf_setting_bucket {
	size_t settings_size;
	struct conf_setting *settings;
};

linted_error linted_conf_create(struct linted_conf **confp, char *name)
{
	linted_error err = 0;

	struct linted_conf *conf;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *conf);
		if (err != 0)
			return err;
		conf = xx;
	}

	conf->name = name;
	conf->refcount = 1;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct conf_section_bucket *bucket = &conf->buckets[ii];

		bucket->sections_size = 0U;
		bucket->sections = 0;
	}

	*confp = conf;

	return 0;
}

static void free_sections(struct conf_section *sections, size_t size);

void linted_conf_put(struct linted_conf *conf)
{
	if (0 == conf)
		return;

	if (--conf->refcount != 0)
		return;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct conf_section_bucket const *bucket =
		    &conf->buckets[ii];

		size_t sections_size = bucket->sections_size;
		struct conf_section *sections = bucket->sections;

		free_sections(sections, sections_size);

		linted_mem_free(sections);
	}

	linted_mem_free(conf->name);
	linted_mem_free(conf);
}

char const *linted_conf_peek_name(struct linted_conf *conf)
{
	return conf->name;
}

#define SETTING_BUCKETS_SIZE 1024U

struct conf_section {
	unsigned long refcount;
	char *name;
	struct conf_setting_bucket buckets[SETTING_BUCKETS_SIZE];
};

static inline linted_conf_section section_id_create(unsigned hash,
                                                    unsigned ii)
{
	return ((linted_conf_section)hash) << 32U |
	       (linted_conf_section)ii;
}

static inline size_t section_id_hash(linted_conf_section id)
{
	return id >> 32U;
}

static inline size_t section_id_offset(linted_conf_section id)
{
	return id & UINT32_MAX;
}

static void free_settings(struct conf_setting *settings,
                          size_t settings_size);

static void free_sections(struct conf_section *sections,
                          size_t sections_size)
{
	for (size_t jj = 0U; jj < sections_size; ++jj) {
		struct conf_section const *section = &sections[jj];

		for (size_t kk = 0U; kk < SETTING_BUCKETS_SIZE; ++kk) {
			struct conf_setting_bucket const *
			    setting_bucket = &section->buckets[kk];
			size_t settings_size =
			    setting_bucket->settings_size;
			struct conf_setting *settings =
			    setting_bucket->settings;

			free_settings(settings, settings_size);

			linted_mem_free(settings);
		}
		linted_mem_free(section->name);
	}
}

linted_error linted_conf_add_section(struct linted_conf *conf,
                                     linted_conf_section *sectionp,
                                     char *section_name)
{
	linted_error err;

	struct conf_section_bucket *buckets = conf->buckets;

	size_t section_hash =
	    string_hash(section_name) % SECTION_BUCKETS_SIZE;

	struct conf_section_bucket *bucket = &buckets[section_hash];

	size_t sections_size = bucket->sections_size;
	struct conf_section *sections = bucket->sections;

	{
		bool have_found_field = false;

		size_t found_field;
		for (size_t ii = 0U; ii < sections_size; ++ii) {
			if (0 ==
			    strcmp(sections[ii].name, section_name)) {
				have_found_field = true;
				found_field = ii;
				break;
			}
		}

		if (have_found_field) {
			linted_mem_free(section_name);
			*sectionp = section_id_create(section_hash,
			                              found_field);
			return 0;
		}
	}

	size_t new_sections_size = sections_size + 1U;
	struct conf_section *new_sections;
	{
		void *xx;
		err = linted_mem_realloc_array(&xx, sections,
		                               new_sections_size,
		                               sizeof sections[0U]);
		if (err != 0)
			return err;
		new_sections = xx;
	}

	struct conf_section *new_section = &new_sections[sections_size];

	new_section->name = section_name;

	for (size_t ii = 0U; ii < SETTING_BUCKETS_SIZE; ++ii) {
		new_section->buckets[ii].settings_size = 0U;
		new_section->buckets[ii].settings = 0;
	}

	bucket->sections_size = new_sections_size;
	bucket->sections = new_sections;

	*sectionp = section_id_create(section_hash, sections_size);
	return 0;
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
		linted_mem_free(setting->field);

		for (char **value = setting->value; *value != 0;
		     ++value)
			linted_mem_free(*value);
		linted_mem_free(setting->value);
	}
}

char const *const *linted_conf_find(struct linted_conf *conf,
                                    char const *section_name,
                                    char const *field)
{
	struct conf_section *section;
	{
		struct conf_section_bucket *buckets = conf->buckets;
		struct conf_section_bucket *bucket =
		    &buckets[string_hash(section_name) %
		             SECTION_BUCKETS_SIZE];

		size_t sections_size = bucket->sections_size;
		struct conf_section *sections = bucket->sections;

		size_t section_index;
		bool have_found_section = false;
		for (size_t ii = 0U; ii < sections_size; ++ii) {
			if (0 ==
			    strcmp(sections[ii].name, section_name)) {
				have_found_section = true;
				section_index = ii;
				break;
			}
		}

		if (!have_found_section)
			return 0;

		section = &sections[section_index];
	}

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

linted_error
linted_conf_add_setting(struct linted_conf *conf,
                        linted_conf_section section, char *field,
                        char const *const *additional_values)
{
	linted_error err;

	struct conf_setting_bucket *buckets =
	    conf->buckets[section_id_hash(section)]
	        .sections[section_id_offset(section)]
	        .buckets;

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
			linted_mem_free(field);

			linted_mem_free(setting_field);

			for (size_t ii = 0U; setting_values[ii] != 0;
			     ++ii)
				linted_mem_free(setting_values[ii]);

			linted_mem_free(setting_values);

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
				err = linted_mem_alloc_array(
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
					err = linted_str_dup(
					    &xx, additional_values[ii]);
					if (err != 0) {
						for (; ii != 0; --ii)
							linted_mem_free(
							    new_value
							        [ii -
							         1U]);

						linted_mem_free(
						    new_value);
						return err;
					}
					copy = xx;
				}
				new_value[setting_values_len + ii] =
				    copy;
			}

			new_value[new_value_len] = 0;

			linted_mem_free(setting_field);
			linted_mem_free(setting_values);

			setting->field = field;
			setting->value = new_value;
		}
		return 0;
	}
have_not_found_field:
	;
	char **value_copy;
	{
		void *xx;
		err = linted_mem_alloc_array(
		    &xx, additional_values_len + 1U,
		    sizeof additional_values[0U]);
		if (err != 0)
			return err;
		value_copy = xx;
	}
	for (size_t ii = 0U; ii < additional_values_len; ++ii) {
		char *copy;
		{
			char *xx;
			err =
			    linted_str_dup(&xx, additional_values[ii]);
			if (err != 0) {
				for (size_t jj = 0U; jj < ii; ++jj)
					linted_mem_free(value_copy[jj]);
				linted_mem_free(value_copy);
				return err;
			}
			copy = xx;
		}
		value_copy[ii] = copy;
	}
	value_copy[additional_values_len] = 0;

	size_t new_settings_size = settings_size + 1U;
	struct conf_setting *new_settings;
	{
		void *xx;
		err = linted_mem_realloc_array(&xx, settings,
		                               new_settings_size,
		                               sizeof settings[0U]);
		if (err != 0) {
			for (size_t ii = 0U; value_copy[ii] != 0; ++ii)
				linted_mem_free(value_copy[ii]);
			linted_mem_free(value_copy);

			return err;
		}
		new_settings = xx;
	}
	new_settings[settings_size].field = field;
	new_settings[settings_size].value = value_copy;

	bucket->settings_size = new_settings_size;
	bucket->settings = new_settings;

	return 0;
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
