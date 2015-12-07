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
#ifndef LNTD_KO_H
#error this header should never be included directly
#endif

/* IWYU pragma: private, include "lntd/ko.h" */

typedef unsigned lntd_ko;

#define LNTD_KO_CWD ((lntd_ko)-1)

#define LNTD_KO_STDIN 0U
#define LNTD_KO_STDOUT 1U
#define LNTD_KO_STDERR 2U
