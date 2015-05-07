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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_ERROR_H
#error this header should never be included directly
#endif

#include <errno.h>

typedef unsigned linted_error;

#define LINTED_ERROR_AGAIN EAGAIN
#define LINTED_ERROR_CANCELLED ECANCELED
#define LINTED_ERROR_INVALID_KO EBADF
#define LINTED_ERROR_INVALID_PARAMETER EINVAL
#define LINTED_ERROR_UNIMPLEMENTED ENOSYS
#define LINTED_ERROR_OUT_OF_MEMORY ENOMEM
#define LINTED_ERROR_PERMISSION EPERM
