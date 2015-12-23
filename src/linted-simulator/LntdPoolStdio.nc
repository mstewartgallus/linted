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
#include "async.h"
#include "error.h"
#include "ko.h"

#include <limits.h>
#include <stddef.h>

module LntdPoolStdio
{
	uses interface LntdAsyncCommand;
	provides interface LntdStdio;
}
implementation
{
	lntd_error my_read(lntd_ko ko, char *bytes, size_t size,
	                   size_t *bytes_read);

	lntd_error my_write(lntd_ko ko, char const *bytes, size_t size);

	command lntd_error
	    LntdStdio.read_line(lntd_ko ko, char **bufp, size_t *sizep)
	{
		size_t size = 80U;

		size_t bytes_read = 0U;
		lntd_error err;

		char *buf;

		if (ko > INT_MAX)
			return EINVAL;

		buf = malloc(size);
		if (0 == buf) {
			return errno;
		}

		for (;;) {
			char last_byte;

			size_t xx;
			err = my_read(ko, buf + bytes_read, 1U, &xx);
			if (err != 0) {
				free(buf);
				return errno;
			}
			if (0 == xx)
				break;
			last_byte = buf[bytes_read];
			++bytes_read;
			if ('\n' == last_byte) {
				--bytes_read;
				break;
			}
		}
		buf[bytes_read] = '\0';

		*bufp = buf;
		if (sizep != 0)
			*sizep = size;
		return 0;
	}

	command lntd_error
	    LntdStdio.write_line(lntd_ko ko, char const *str)
	{
		size_t strsize;
		char *newlinestr;
		lntd_error err;

		if (ko > INT_MAX)
			return EINVAL;

		strsize = strlen(str);
		newlinestr = malloc(strsize + 1U);
		if (0 == newlinestr) {
			return errno;
		}

		memcpy(newlinestr, str, strsize);

		newlinestr[strsize] = '\n';

		err = my_write(ko, newlinestr, strsize + 1U);

		free(newlinestr);

		return err;
	}

	command lntd_error LntdStdio.read(
	    lntd_ko ko, char *buf, size_t size, size_t *bytes_read)
	{
		return my_read(ko, buf, size, bytes_read);
	}

	command lntd_error
	    LntdStdio.write(lntd_ko ko, char const *bytes, size_t size)
	{
		return my_write(ko, bytes, size);
	}

	lntd_error my_read(lntd_ko ko, char *bytes, size_t size,
	                   size_t *bytes_read)
	{
		struct lntd_async_cmd_read cmd = {0};
		lntd_error err;

		if (ko > INT_MAX)
			return EINVAL;

		cmd.ko = ko;
		cmd.bytes = bytes;
		cmd.size = size;

		cmd.bytes_left = size;

		err = call LntdAsyncCommand.execute_sync(
		    LNTD_ASYNC_CMD_TYPE_READ, &cmd);

		*bytes_read = size - cmd.bytes_left;

		return err;
	}

	lntd_error my_write(lntd_ko ko, char const *bytes, size_t size)
	{
		struct lntd_async_cmd_write cmd = {0};

		if (ko > INT_MAX)
			return EINVAL;

		cmd.ko = ko;
		cmd.bytes = bytes;
		cmd.size = size;

		cmd.bytes_left = size;

		return call LntdAsyncCommand.execute_sync(
		    LNTD_ASYNC_CMD_TYPE_WRITE, &cmd);
	}

	event void LntdAsyncCommand.done(lntd_error the_err)
	{
		/* Never happens */
	}
}
