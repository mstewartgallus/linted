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
#define LINTED_ERROR_H

#if defined HAVE_WINDOWS_API
#include "error-windows.h"
#elif defined HAVE_POSIX_API
#include "error-posix.h"
#else
#error no error code method for this platform has been implemented
#endif

/**
 * @file
 *
 * Abstracts over the system's basic error type.
 */

char const *linted_error_string(linted_error errnum);

void linted_error_string_free(char const *str);

#endif /* LINTED_ERROR_H */
