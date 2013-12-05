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
#include <sys/wait.h>
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

void linted_pipe(int pipefd[2]) {
    if (-1 == pipe(pipefd)) {
        LINTED_ERROR("Could not make pipe because of error: %s\n",
                     strerror(errno));
    }
}

pid_t linted_fork(void) {
    const pid_t child = fork();
    if (-1 == child) {
        LINTED_ERROR("Could not fork child because of error: %s\n",
                     strerror(errno));
    }
    return child;
}

void linted_execv(const char * const path, char *const argv[]) {
    execv(path, argv);
    LINTED_ERROR("Could not execute %s because of error: %s\n",
                 path,
                 strerror(errno));
}

pid_t linted_wait(int * status) {
retry:;
    const pid_t result = wait(status);
    if (-1 == result) {
        if (errno == EINTR) {
            goto retry;
        }
        LINTED_ERROR("Could not wait for a process to exit",
                     strerror(errno));
    }
    return result;
}
