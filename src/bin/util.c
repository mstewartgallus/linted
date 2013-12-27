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

#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void linted_error(const char * file,
                  const char * function,
                  unsigned line,
                  const char * format_string, ...) {
    va_list arguments;
    va_start(arguments, format_string);

    fprintf(stderr,
            "Error in file %s, function %s, and line %d:\n",
            file, function, line);
    vfprintf(stderr, format_string, arguments);

    exit(1);
}

int linted_open(const char *pathname, int flags) {
  retry:;
    const int fd = open(pathname, flags);
    if (-1 == fd) {
        if (errno == EINTR) {
            goto retry;
        }

        LINTED_ERROR("Could not open file descriptor to path name %s: %s\n",
                     pathname,
                     strerror(errno));
    }
    return fd;
}

void linted_close(int fd) {
  retry:;
    const int status = close(fd);
    if (-1 == status) {
        if (errno == EINTR) {
            goto retry;
        }

        LINTED_ERROR("Could not close file descriptor: %s\n",
                     strerror(errno));
    }
}

FILE * linted_fdopen(int fd, const char *mode) {
  retry:;
    FILE * const file = fdopen(fd, mode);
    if (NULL == file) {
        if (errno == EINTR) {
            goto retry;
        }
        LINTED_ERROR("Could not open file descriptor in mode %s: %s\n",
                     mode,
                     strerror(errno));
    }
    return file;
}
