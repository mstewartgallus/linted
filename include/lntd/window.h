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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_WINDOW_H
#define LNTD_WINDOW_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stdint.h>

/**
 * @file
 *
 * Communicates user input to another process.
 */

typedef lntd_ko lntd_window;
typedef lntd_ko lntd_window_notifier;

lntd_error lntd_window_read(lntd_window window, uint_fast32_t *outp);
lntd_error lntd_window_write(lntd_window window, uint_fast32_t in);

#endif /* LNTD_WINDOW_H */
