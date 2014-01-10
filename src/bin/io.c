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
#include "config.h"

#include "linted/io.h"
#include "linted/util.h"

#include <sys/socket.h>
#include <sys/types.h>

ssize_t linted_io_send_with_fd(int const fd,
			       void const *const buf, size_t const len, int const sent_fd)
{
	struct iovec iovecs[] = {
		(struct iovec){
			       /* The iovec mechanism doesn't have const correctness for
			        * symmetry with recvmsg.
			        */
			       .iov_base = (void *)buf,
			       .iov_len = len}
	};

	struct msghdr message;
	memset(&message, 0, sizeof message);

	message.msg_iov = iovecs;
	message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

	int const sent_fildes[] = { sent_fd };
	char control_message[CMSG_SPACE(sizeof sent_fildes)];
	message.msg_control = control_message;
	message.msg_controllen = sizeof control_message;

	struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
	control_message_header->cmsg_level = SOL_SOCKET;
	control_message_header->cmsg_type = SCM_RIGHTS;
	control_message_header->cmsg_len = CMSG_LEN(sizeof sent_fildes);

	void *const control_message_data = CMSG_DATA(control_message_header);
	memcpy(control_message_data, sent_fildes, sizeof sent_fildes);

	return sendmsg(fd, &message, 0);
}

ssize_t linted_io_recv_with_fd(int const fd,
                               void * const buf, size_t const len,
                               int * const received_fd) {
    struct msghdr message;
    memset(&message, 0, sizeof message);

    struct iovec iov[] = {
        (struct iovec){
            .iov_base = buf,
            .iov_len = len}
    };
    message.msg_iov = iov;
    message.msg_iovlen = LINTED_ARRAY_SIZE(iov);

    int sent_fildes[1] = { -1 };
    char control_message[CMSG_SPACE(sizeof sent_fildes)];
    memset(control_message, 0, sizeof control_message);

    message.msg_control = control_message;
    message.msg_controllen = sizeof control_message;

    ssize_t const bytes_read = recvmsg(fd,
                                       &message,
                                       MSG_CMSG_CLOEXEC | MSG_WAITALL);

    if (bytes_read != -1 && bytes_read != 0) {
        struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
        void *const control_message_data = CMSG_DATA(control_message_header);

        memcpy(sent_fildes, control_message_data, sizeof sent_fildes);

        *received_fd = sent_fildes[0];
    }
    return bytes_read;
}
