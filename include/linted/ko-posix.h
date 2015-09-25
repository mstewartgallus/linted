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
#ifndef LINTED_KO_H
#error this header should never be included directly
#endif

/* IWYU pragma: private, include "linted/ko.h" */

typedef unsigned linted_ko;

#define LINTED_KO_CWD ((linted_ko)-1)

#define LINTED_KO_STDIN 0U
#define LINTED_KO_STDOUT 1U
#define LINTED_KO_STDERR 2U
