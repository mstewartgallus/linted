/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/fifo.h"

#include "linted/error.h"

#include <windows.h>

linted_error linted_fifo_pair(linted_fifo *readerp,
                              linted_fifo *writerp, unsigned long flags)
{
	if (flags != 0)
		return LINTED_ERROR_INVALID_PARAMETER;

	linted_ko reader;
	linted_ko writer;
	{
		HANDLE xx;
		HANDLE yy;
		if (!CreatePipe(&xx, &yy, 0, 4096U)) {
			linted_error err =
			    HRESULT_FROM_WIN32(GetLastError());
			LINTED_ASSUME(err != 0);
			return err;
		}
		reader = xx;
		writer = yy;
	}

	*readerp = reader;
	*writerp = writer;
	return 0;
}

linted_error linted_fifo_create(linted_fifo *kop, linted_ko dirko,
                                char const *pathname,
                                unsigned long flags, mode_t mode)
{
	return LINTED_ERROR_UNIMPLEMENTED;
}
