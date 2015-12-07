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
#include "config.h"

#include "lntd/locale.h"

#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/str.h"

#include <stddef.h>

lntd_error lntd_locale_on_bad_option(lntd_ko ko,
                                     char const *process_name,
                                     char const *bad_option)
{
	lntd_error err;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = 0;

	err = lntd_str_append_cstring(&buffer, &capacity, &size,
	                              process_name);
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size, "\
: unrecognized option '");
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size,
	                              bad_option);
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size, "'\n");
	if (err != 0)
		goto free_buffer;

	err = lntd_io_write_all(ko, 0, buffer, size);
	if (err != 0)
		goto free_buffer;

free_buffer:
	lntd_mem_free(buffer);
	return err;
}

lntd_error lntd_locale_try_for_more_help(lntd_ko ko,
                                         char const *process_name,
                                         char const *help_option)
{
	lntd_error err;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = 0;

	err =
	    lntd_str_append_cstring(&buffer, &capacity, &size, "Try `");
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size,
	                              process_name);
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size, " ");
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size,
	                              help_option);
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size, "\
' for more information.\n");
	if (err != 0)
		goto free_buffer;

	err = lntd_io_write_all(ko, 0, buffer, size);
	if (err != 0)
		goto free_buffer;

free_buffer:
	lntd_mem_free(buffer);
	return err;
}

lntd_error lntd_locale_version(lntd_ko ko, char const *package_string,
                               char const *copyright_year)
{
	lntd_error err;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = 0;

	err = lntd_str_append_cstring(&buffer, &capacity, &size,
	                              package_string);
	if (err != 0)
		goto free_buffer;

	err =
	    lntd_str_append_cstring(&buffer, &capacity, &size, "\n\n");
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size, "\
Copyright (C) ");
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size,
	                              copyright_year);
	if (err != 0)
		goto free_buffer;

	err = lntd_str_append_cstring(&buffer, &capacity, &size, "\
 Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n");
	if (err != 0)
		goto free_buffer;

	err = lntd_io_write_all(ko, 0, buffer, size);
	if (err != 0)
		goto free_buffer;

free_buffer:
	lntd_mem_free(buffer);
	return err;
}
