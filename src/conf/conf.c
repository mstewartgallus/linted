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
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct conf_section_bucket
{
	size_t sections_size;
	struct conf_section *sections;
};

#define SECTION_BUCKETS_SIZE 1024U

struct conf
{
	char *name;
	unsigned long refcount;
	struct conf_section_bucket buckets[SECTION_BUCKETS_SIZE];
};

struct conf_setting_buckets;

#define SETTING_BUCKETS_SIZE 1024U

struct conf_setting_bucket
{
	size_t settings_size;
	struct conf_setting *settings;
};

struct conf_section
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

static size_t string_hash(char const *str);

linted_error conf_create(struct conf **unitp, char const *name)
{
	linted_error errnum = 0;
	struct conf *unit;

	char *name_copy = strdup(name);
	if (NULL == name_copy) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	unit = malloc(sizeof *unit);
	if (NULL == unit) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_name_copy;
	}

	unit->name = name_copy;
	unit->refcount = 1;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct conf_section_bucket *bucket = &unit->buckets[ii];

		bucket->sections_size = 0U;
		bucket->sections = NULL;
	}

free_name_copy:
	if (errnum != 0) {
		free(name_copy);
		return errnum;
	}

	*unitp = unit;

	return 0;
}

void conf_put(struct conf *unit)
{
	if (--unit->refcount != 0)
		return;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct conf_section_bucket const *bucket = &unit->buckets[ii];

		for (size_t jj = 0U; jj < bucket->sections_size; ++jj) {
			struct conf_section const *section =
			    &bucket->sections[jj];

			for (size_t kk = 0U; kk < SETTING_BUCKETS_SIZE; ++kk) {
				struct conf_setting_bucket const *
				setting_bucket = &section->buckets[kk];

				for (size_t ww = 0U;
				     ww < setting_bucket->settings_size; ++ww) {
					free(
					    setting_bucket->settings[ww].field);
					free(
					    setting_bucket->settings[ww].value);
				}

				free(setting_bucket->settings);
			}
		}

		free(bucket->sections);
	}
	free(unit->name);
	free(unit);
}

char const *conf_peek_name(struct conf *unit)
{
	return unit->name;
}

linted_error conf_add_section(struct conf *unit, struct conf_section **sectionp,
                              char *section_name)
{
	linted_error errnum;

	struct conf_section_bucket *buckets = unit->buckets;

	struct conf_section_bucket *bucket =
	    &buckets[string_hash(section_name) % SECTION_BUCKETS_SIZE];

	size_t sections_size = bucket->sections_size;
	struct conf_section *sections = bucket->sections;

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
		free(section_name);
		*sectionp = &sections[found_field];
	} else {
		size_t new_sections_size = sections_size + 1U;
		struct conf_section *new_sections =
		    realloc(sections, new_sections_size * sizeof sections[0U]);
		if (NULL == new_sections) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		struct conf_section *new_section = &new_sections[sections_size];

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

char const *const *conf_find(struct conf *unit, char const *section,
                             char const *field)
{
	struct conf_section *found_section;

	{
		struct conf_section_bucket *buckets = unit->buckets;
		struct conf_section_bucket *bucket =
		    &buckets[string_hash(section) % SECTION_BUCKETS_SIZE];

		size_t sections_size = bucket->sections_size;
		struct conf_section *sections = bucket->sections;

		bool have_found_section = false;
		for (size_t ii = 0U; ii < sections_size; ++ii) {
			if (0 == strcmp(sections[ii].name, section)) {
				have_found_section = true;
				found_section = &sections[ii];
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
		}
	}

	if (!have_found_setting)
		return NULL;

	return (char const * const *)found_setting->value;
}

linted_error conf_add_setting(struct conf_section *section, char *field,
                              char const *const *value)
{
	linted_error errnum;

	struct conf_setting_bucket *buckets = section->buckets;

	struct conf_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct conf_setting *settings = bucket->settings;

	bool have_found_field = false;
	size_t found_field;
	for (size_t ii = 0U; ii < settings_size; ++ii) {
		if (0 == strcmp(settings[ii].field, field)) {
			have_found_field = true;
			found_field = ii;
			break;
		}
	}

	if (have_found_field) {
		if (NULL == value[0U]) {
			free(field);
			free(settings[found_field].field);

			char **values = settings[found_field].value;

			for (size_t ii = 0U; values[ii] != NULL; ++ii)
				free(values[ii]);

			free(values);

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

			char **new_value =
			    malloc((new_value_len + 1U) * sizeof value[0U]);
			if (NULL == new_value) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				return errnum;
			}

			for (size_t ii = 0U; ii < old_value_len; ++ii)
				new_value[ii] = old_value[ii];

			for (size_t ii = 0U; ii < value_len; ++ii) {
				char *copy = strdup(value[ii]);
				if (NULL == copy) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
					for (; ii != 0; --ii)
						free(new_value[ii - 1U]);

					free(new_value);
					return errnum;
				}
				new_value[old_value_len + ii] = copy;
			}

			new_value[new_value_len] = NULL;

			free(settings[found_field].field);
			free(old_value);

			settings[found_field].field = field;
			settings[found_field].value = new_value;
		}
	} else {
		if (NULL == value[0U]) {
			free(field);
		} else {
			size_t new_settings_size = settings_size + 1U;
			struct conf_setting *new_settings = realloc(
			    settings, new_settings_size * sizeof settings[0U]);
			if (NULL == new_settings) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				return errnum;
			}

			size_t value_len;
			for (size_t ii = 0U;; ++ii) {
				if (NULL == value[ii]) {
					value_len = ii;
					break;
				}
			}

			char **value_copy =
			    malloc((value_len + 1U) * sizeof value[0U]);
			for (size_t ii = 0U; ii < value_len; ++ii)
				value_copy[ii] = strdup(value[ii]);
			value_copy[value_len] = NULL;

			new_settings[settings_size].field = field;
			new_settings[settings_size].value = value_copy;

			bucket->settings_size = new_settings_size;
			bucket->settings = new_settings;
		}
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
