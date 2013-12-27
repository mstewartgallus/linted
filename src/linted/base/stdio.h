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


void linted_fprintf(FILE * file, const char * format_string, ...);

void linted_fputs(const char * output, FILE * file);
void linted_puts(const char * output);

void linted_fflush(FILE * file);

void linted_fclose(FILE * const fp);

void linted_sprintf(char *str, const char *format, ...);

#endif /* LINTED_BASE_STDIO_H */
