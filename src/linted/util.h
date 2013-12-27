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
#ifndef LINTED_UTIL_H
#define LINTED_UTIL_H

#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

#define LINTED_ERROR(...)                                       \
    linted_error(__FILE__, __func__, __LINE__,  __VA_ARGS__)

void linted_error(const char * file,
                  const char * function,
                  unsigned line,
                  const char * format_string, ...);

int linted_open(const char *pathname, int flags);

void linted_close(int fd);

FILE * linted_fdopen(int fd, const char *mode);

void linted_pipe(int pipefd[2]);

#endif /* LINTED_UTIL_H */
