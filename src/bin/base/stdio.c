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
#include "config.h"

#include "linted/base/stdio.h"

#include "linted/util.h"

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void linted_sprintf(char * str, const char * format_string, ...) {
    va_list arguments;
    va_start(arguments, format_string);

    if (vsprintf(str, format_string, arguments) < 0) {
        LINTED_ERROR("Could write format string %s\n", format_string);
    }

    va_end(arguments);
}

void linted_fprintf(FILE * const file, const char * format_string, ...) {
    va_list arguments;
    va_start(arguments, format_string);

    if (EOF == vfprintf(file, format_string, arguments)) {
        LINTED_ERROR("Could not write to file\n");
    }

    va_end(arguments);
}

void linted_fputs(const char * output, FILE * file) {
    if (fputs(output, file) != EOF) {
        return;
    }

    LINTED_ERROR("Could not write to file\n");
}

void linted_puts(const char * const output) {
    if (puts(output) != EOF) {
        return;
    }

    LINTED_ERROR("Could not write to standard output\n");
}
