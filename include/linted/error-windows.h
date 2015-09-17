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
#ifndef LINTED_ERROR_H
#error this header should never be included directly
#endif

#include <stdint.h>

#include <ntdef.h>
#include <winerror.h>

/* HRESULTs are 32-bit signed integers */
typedef int_fast32_t linted_error;

#define LINTED_ERROR_AGAIN _HRESULT_TYPEDEF_(WSAEWOULDBLOCK)
#define LINTED_ERROR_CANCELLED                                         \
	_HRESULT_TYPEDEF_(ERROR_OPERATION_ABORTED)
#define LINTED_ERROR_INVALID_KO _HRESULT_TYPEDEF_(ERROR_INVALID_HANDLE)
#define LINTED_ERROR_INVALID_PARAMETER                                 \
	_HRESULT_TYPEDEF_(ERROR_INVALID_PARAMETER)
#define LINTED_ERROR_UNIMPLEMENTED                                     \
	_HRESULT_TYPEDEF_(ERROR_INVALID_PARAMETER)
#define LINTED_ERROR_OUT_OF_MEMORY                                     \
	_HRESULT_TYPEDEF_(ERROR_NOT_ENOUGH_MEMORY)
#define LINTED_ERROR_PERMISSION _HRESULT_TYPEDEF_(ERROR_ACCESS_DENIED)
