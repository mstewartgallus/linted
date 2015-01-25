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
#ifndef LINTED_LOG_H
#define LINTED_LOG_H

/**
 * @file
 *
 * Abstracts over the system logger.
 */

enum { LINTED_LOG_ERROR = 3 };

void linted_log_open(char const *ident);
void linted_log(unsigned log_level, char const *format, ...);

#endif /* LINTED_LOG_H */
