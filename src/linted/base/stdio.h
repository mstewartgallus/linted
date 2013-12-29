/*
 * Copyright 2013 Steven Stewart-Gallus
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
#ifndef LINTED_BASE_STDIO_H
#define LINTED_BASE_STDIO_H

#include <stdio.h>


void linted_fprintf(FILE * file, char const * format_string, ...);

void linted_fputs(char const * output, FILE * file);
void linted_puts(char const * output);

void linted_sprintf(char *str, char const * format, ...);

#endif /* LINTED_BASE_STDIO_H */
