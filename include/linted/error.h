/*
 * Copyright 2014 Steven Stewart-Gallus
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
#ifndef LINTED_ERROR_H
#define LINTED_ERROR_H

#if defined __linux__
#include <errno.h>
#endif

#if defined _WIN32 || defined _WIN64
#include <windows.h>
#endif

/**
 * @file
 *
 * Abstracts over the system's basic error type.
 */

#if defined __linux__
typedef int linted_error;
#elif defined __WIN32 || defined _WIN64
typedef HRESULT linted_error;
#else
#error no known most primitive platform error type
#endif

char const *linted_error_string_alloc(linted_error errnum);
void linted_error_string_free(char const *error_string);

#endif /* LINTED_ERROR_H */
