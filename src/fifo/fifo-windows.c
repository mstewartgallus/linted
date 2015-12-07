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

#include "config.h"

#include "lntd/fifo.h"

#include "lntd/error.h"

#include <windows.h>

lntd_error lntd_fifo_pair(lntd_fifo *readerp, lntd_fifo *writerp,
                          unsigned long flags)
{
	if (flags != 0)
		return LNTD_ERROR_INVALID_PARAMETER;

	lntd_ko reader;
	lntd_ko writer;
	{
		HANDLE xx;
		HANDLE yy;
		if (!CreatePipe(&xx, &yy, 0, 4096U)) {
			lntd_error err =
			    HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);
			return err;
		}
		reader = xx;
		writer = yy;
	}

	*readerp = reader;
	*writerp = writer;
	return 0;
}

lntd_error lntd_fifo_create(lntd_fifo *kop, lntd_ko dirko,
                            char const *pathname, unsigned long flags,
                            mode_t mode)
{
	return LNTD_ERROR_UNIMPLEMENTED;
}
