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
#include "config.h"

#include "linted/io.h"
#include "linted/util.h"

#include <errno.h>
#include <limits.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>


mqd_t linted_io_anonymous_mq(struct mq_attr *attr, int oflag)
{
    char const template_name[] = "/anonymous-mq-lP5N7IJUZDZWzQ3hjV7-XXXXXXXXXX";
    size_t const xs_start = 35;

    char random_mq_name[sizeof template_name];
    memcpy(random_mq_name, template_name, sizeof template_name);

    mqd_t new_mq;
    do {
        /* TODO: Replace, rand is terrible */
        /* Seed with rand */
        int possible_seed;
        do {
            /* To get an even distribution of values throw out
             * results that don't fit.
             */
            possible_seed = rand();
        } while (possible_seed > CHAR_MAX);

        unsigned char state = possible_seed;

        for (size_t ii = xs_start; ii < sizeof template_name - 1; ++ii) {
            for (;;) {
                /* Use a fast linear congruential generator */
                state = 5 + 3 * state;
                unsigned const possible_value = state;

                /* Again, throw out results that don't fit for an even
                 * distribution of values.
                 */
                if ((possible_value >= 'a' && possible_value <= 'z')
                    || (possible_value >= 'A' && possible_value <= 'Z')
                    || (possible_value >= '0' && possible_value <= '9')) {
                    random_mq_name[ii] = possible_value;
                    break;
                }
            }
        }

        new_mq = mq_open(random_mq_name, oflag | O_RDWR | O_CREAT | O_EXCL,
                         S_IRUSR | S_IWUSR, attr);
    } while (-1 == new_mq && EEXIST == errno);

    if (new_mq != -1) {
        /* This could only happen via programmer error */
        if (-1 == mq_unlink(random_mq_name)) {
            LINTED_ERROR(
                    "Could not remove message queue with name %s because of error: %m",
                    random_mq_name, errno);
        }
    }

    return new_mq;
}

int linted_io_create_local_server(int sockets[2])
{
    return socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sockets);
}

int linted_io_connect_to_local_socket(int const local)
{
    int sockets[2];
    if (-1 == socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sockets)) {
        return -1;
    }

    int sent_end = sockets[0];
    int kept_end = sockets[1];

    {
        char dummy_data = 0;

        struct iovec iovecs[] = {
            (struct iovec){
                           .iov_base = &dummy_data,
                           .iov_len = sizeof dummy_data}
        };

        struct msghdr message;
        memset(&message, 0, sizeof message);

        message.msg_iov = iovecs;
        message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

        int const sent_fildes[] = { sent_end };
        char control_message[CMSG_SPACE(sizeof sent_fildes)];
        memset(control_message, 0, sizeof control_message);

        message.msg_control = control_message;
        message.msg_controllen = sizeof control_message;

        struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
        control_message_header->cmsg_level = SOL_SOCKET;
        control_message_header->cmsg_type = SCM_RIGHTS;
        control_message_header->cmsg_len = CMSG_LEN(sizeof sent_fildes);

        void *const control_message_data = CMSG_DATA(control_message_header);
        memcpy(control_message_data, sent_fildes, sizeof sent_fildes);

        ssize_t bytes_written;
        do {
            bytes_written = sendmsg(local, &message, 0);
        } while (-1 == bytes_written && EINTR == errno);
        if (-1 == bytes_written) {
            goto error_and_close_sockets;
        }
    }

    if (-1 == close(sent_end)) {
        goto error_and_close_socket;
    }

    return kept_end;

 error_and_close_sockets:
    close(sent_end);
 error_and_close_socket:
    close(kept_end);

    return -1;
}

int linted_io_recv_socket(int const inbox)
{
    char dummy_data = 0;

    struct iovec iovecs[] = {
        (struct iovec){
                       .iov_base = &dummy_data,
                       .iov_len = sizeof dummy_data}
    };

    struct msghdr message;
    memset(&message, 0, sizeof message);

    message.msg_iov = iovecs;
    message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

    int sent_fildes[1] = { -1 };
    char control_message[CMSG_SPACE(sizeof sent_fildes)];
    memset(control_message, 0, sizeof control_message);

    message.msg_control = control_message;
    message.msg_controllen = sizeof control_message;

    ssize_t bytes_read;
    do {
        bytes_read = recvmsg(inbox, &message, MSG_CMSG_CLOEXEC | MSG_WAITALL);
    } while (-1 == bytes_read && EINTR == errno);

    if (-1 == bytes_read) {
        return -1;
    }

    if (0 == bytes_read) {
        errno = 0;
        return -1;
    }

    struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
    void *const control_message_data = CMSG_DATA(control_message_header);

    memcpy(sent_fildes, control_message_data, sizeof sent_fildes);

    return sent_fildes[0];
}
