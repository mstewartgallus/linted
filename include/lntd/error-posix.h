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
#ifndef LNTD_ERROR_H
#error this header should never be included directly
#endif

/* IWYU pragma: private, include "lntd/error.h" */

#include <errno.h>

typedef unsigned lntd_error;

#define LNTD_ERROR_AGAIN EAGAIN
#define LNTD_ERROR_CANCELLED ECANCELED
#define LNTD_ERROR_INVALID_KO EBADF
#define LNTD_ERROR_INVALID_PARAMETER EINVAL
#define LNTD_ERROR_UNIMPLEMENTED ENOSYS
#define LNTD_ERROR_OUT_OF_MEMORY ENOMEM
#define LNTD_ERROR_PERMISSION EPERM
#define LNTD_ERROR_FILE_NOT_FOUND ENOENT
