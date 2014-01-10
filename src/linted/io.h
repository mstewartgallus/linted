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
#ifndef LINTED_IO_H
#define LINTED_IO_H

#include <string.h>
#include <sys/types.h>

/**
 * Like send but sends the file descriptor sent_fd as well.
 */
ssize_t linted_io_send_with_fd(int fd, void const *buf, size_t len, int sent_fd);

/**
 * Like recv but receives a file descriptor received_fd as well.
 */
ssize_t linted_io_recv_with_fd(int fd, void *buf, size_t len, int *received_fd);

#endif				/* LINTED_IO_H */
