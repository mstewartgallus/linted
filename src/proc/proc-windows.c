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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "lntd/proc.h"

#include "lntd/async.h"
#include "lntd/mem.h"
#include "lntd/util.h"

#include <signal.h>
#include <sys/types.h>

#include <windows.h>

lntd_proc lntd_proc_get_pid(void)
{
	return GetCurrentProcessId();
}

/* MSVC's way of setting thread names is really weird and hacky */

#define MS_VC_EXCEPTION 0x406D1388

#pragma pack(push, 8)
typedef struct tagTHREADNAME_INFO {
	DWORD dwType;
	LPCSTR szName;
	DWORD dwThreadID;
	DWORD dwFlags;
} THREADNAME_INFO;
#pragma pack(pop)

static LONG CALLBACK exception_handler(EXCEPTION_POINTERS *infop);

/**
 * @todo Get Window's thread name setting to work on GCC which doesn't
 * support SEH.
 */
lntd_error lntd_proc_name(char const *name)
{

	THREADNAME_INFO info = {0};

	/* Must be 0x1000 */
	info.dwType = 0x1000;
	info.szName = name;

	/* Thread ID (-1=caller thread) */
	info.dwThreadID = -1;

	/* Reserved for the future */
	info.dwFlags = 0;

	void *handler =
	    AddVectoredExceptionHandler(1, exception_handler);
	RaiseException(MS_VC_EXCEPTION, 0,
	               sizeof info / sizeof(ULONG_PTR),
	               (ULONG_PTR *)&info);
	RemoveVectoredExceptionHandler(handler);

	return 0;
}

static LONG CALLBACK exception_handler(EXCEPTION_POINTERS *infop)
{
	return EXCEPTION_CONTINUE_EXECUTION;
}
