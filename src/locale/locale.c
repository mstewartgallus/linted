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
#include "config.h"

#include "linted/locale.h"

#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"

linted_error linted_locale_missing_process_name(linted_ko ko,
                                                char const *package_name)
{
	linted_error errnum;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = NULL;

	errnum =
	    linted_str_append_cstring(&buffer, &capacity, &size, package_name);
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
: missing process name\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_io_write_all(ko, NULL, buffer, size)) != 0) {
		goto free_buffer;
	}

free_buffer:
	linted_mem_free(buffer);
	return errnum;
}

linted_error linted_locale_on_bad_option(linted_ko ko, char const *process_name,
                                         char const *bad_option)
{
	linted_error errnum;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = NULL;

	errnum =
	    linted_str_append_cstring(&buffer, &capacity, &size, process_name);
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
: unrecognized option '"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum =
	    linted_str_append_cstring(&buffer, &capacity, &size, bad_option);
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum =
	    linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("'\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_io_write_all(ko, NULL, buffer, size)) != 0) {
		goto free_buffer;
	}

free_buffer:
	linted_mem_free(buffer);
	return errnum;
}

linted_error linted_locale_try_for_more_help(linted_ko ko,
                                             char const *process_name,
                                             struct linted_str help_option)
{
	linted_error errnum;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = NULL;

	if ((errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                    LINTED_STR("Try `"))) != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_str_append_cstring(&buffer, &capacity, &size,
	                                        process_name)) != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                    LINTED_STR(" "))) != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                    help_option)) != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
' for more information.\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_io_write_all(ko, NULL, buffer, size)) != 0) {
		goto free_buffer;
	}

free_buffer:
	linted_mem_free(buffer);
	return errnum;
}

linted_error linted_locale_version(linted_ko ko,
                                   struct linted_str package_string,
                                   struct linted_str copyright_year)
{
	linted_error errnum;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = NULL;

	if ((errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                    package_string)) != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                    LINTED_STR("\n\n"))) != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
Copyright (C) "));
	if (errnum != 0) {
		goto free_buffer;
	}
	if ((errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                    copyright_year)) != 0) {
		goto free_buffer;
	}
	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
 Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	if ((errnum = linted_io_write_all(ko, NULL, buffer, size)) != 0) {
		goto free_buffer;
	}

free_buffer:
	linted_mem_free(buffer);
	return errnum;
}
