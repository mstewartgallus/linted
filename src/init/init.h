/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#ifndef LINTED_INIT_INIT_H
#define LINTED_INIT_INIT_H

#include "linted/ko.h"

#include <inttypes.h>

uint_fast8_t linted_init_init(linted_ko cwd, char const *display,
                              char const *chrootdir_path,
                              char const *fstab_path,
                              char const *simulator_path,
                              char const *gui_path);

#endif /* LINTED_INIT_INIT_H */
