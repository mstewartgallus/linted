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
#include "linted/locale.h"
#include "linted/manager.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>


static int run_status(char const *program_name, int argc, char **argv);
static int run_stop(char const *program_name, int argc, char **argv);

static errno_t ctl_help(int fildes, char const *program_name,
                        struct linted_str package_name,
                        struct linted_str package_url,
                        struct linted_str package_bugreport);
static errno_t status_help(int fildes, char const *program_name,
                           struct linted_str package_name,
                           struct linted_str package_url,
                           struct linted_str package_bugreport);
static errno_t stop_help(int fildes, char const *program_name,
                         struct linted_str package_name,
                         struct linted_str package_url,
                         struct linted_str package_bugreport);
static errno_t failure(int fildes, char const *program_name,
                       struct linted_str message, errno_t errnum);

int main(int argc, char **argv)
{
    if (argc < 1) {
        linted_locale_missing_process_name(STDERR_FILENO,
                                           LINTED_STR(PACKAGE_TARNAME
                                                      "-lintedctl"));
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;
    char const *command = NULL;
    unsigned last_index = 1;
    for (; last_index < (unsigned)argc; ++last_index) {
        char const *argument = argv[last_index];

        if (0 == strncmp(argument, "--", strlen("--"))) {
            if (0 == strcmp(argument, "--help")) {
                need_help = true;
            } else if (0 == strcmp(argument, "--version")) {
                need_version = true;
            } else {
                bad_option = argument;
            }
        } else {
            command = argument;
            break;
        }
    }
    ++last_index;

    if (need_help) {
        ctl_help(STDOUT_FILENO,
                 program_name,
                 LINTED_STR(PACKAGE_NAME),
                 LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(STDERR_FILENO, program_name, bad_option);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    if (NULL == command) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing COMMAND\n", program_name);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    if (0 == strcmp("status", command)) {
        return run_status(program_name, argc - last_index + 1,
                          argv + last_index - 1);
    } else if (0 == strcmp("stop", command)) {
        return run_stop(program_name, argc - last_index + 1,
                        argv + last_index - 1);
    } else {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: unrecognized command '%s'\n",
                               program_name, command);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }
}

static int run_status(char const *program_name, int argc, char **argv)
{
    bool need_version = false;
    bool need_add_help = false;
    char const *bad_option = NULL;
    char const *bad_argument = NULL;
    unsigned last_index = 1;
    for (; last_index < (unsigned)argc; ++last_index) {
        char const *argument = argv[last_index];

        if (0 == strncmp(argument, "--", strlen("--"))) {
            if (0 == strcmp(argument, "--help")) {
                need_add_help = true;
            } else if (0 == strcmp(argument, "--version")) {
                need_version = true;
            } else {
                bad_option = argument;
            }
        } else {
            bad_argument = argument;
            break;
        }
    }

    if (need_add_help) {
        status_help(STDOUT_FILENO,
                    program_name,
                    LINTED_STR(PACKAGE_NAME),
                    LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(STDERR_FILENO, program_name, bad_option);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    if (bad_argument != NULL) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: too many arguments: '%s'\n",
                               program_name, bad_argument);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    char const *path = getenv("LINTED_SOCKET");
    if (NULL == path) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing LINTED_SOCKET\n", program_name);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    size_t path_len = strlen(path);
    if (path_len > LINTED_MANAGER_PATH_MAX - 1) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: LINTED_SOCKET is too long\n", program_name);
        return EXIT_FAILURE;
    }

    linted_manager linted;

    {
        linted_manager manager;
        char buf[LINTED_MANAGER_PATH_MAX];

        buf[0] = '\0';
        memcpy(buf + 1, path, path_len);

        errno_t errnum = linted_manager_connect(&manager, buf, path_len + 1);
        if (errnum != 0) {
            failure(STDERR_FILENO, program_name,
                    LINTED_STR("can not create socket"), errnum);
            return EXIT_FAILURE;
        }
        linted = manager;
    }

    {
        union linted_manager_request request;
        memset(&request, 0, sizeof request);

        request.type = LINTED_MANAGER_STATUS;
        request.status.service = LINTED_MANAGER_SERVICE_GUI;

        linted_io_write_format(STDOUT_FILENO, NULL,
                               "%s: sending the status request for the gui\n",
                               program_name);

        errno_t errnum = linted_manager_send_request(linted, &request);
        if (errnum != 0) {
            failure(STDERR_FILENO, program_name,
                    LINTED_STR("can not send request"), errnum);
            return EXIT_FAILURE;
        }
    }

    {
        union linted_manager_reply reply;
        size_t bytes_read;
        errno_t errnum = linted_manager_recv_reply(linted, &reply, &bytes_read);
        if (errnum != 0) {
            failure(STDERR_FILENO, program_name,
                    LINTED_STR("can not read reply"), errnum);
            return EXIT_FAILURE;
        }

        if (0 == bytes_read) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: socket hung up\n", program_name);
            return EXIT_FAILURE;
        }

        /* Sent malformed input */
        if (bytes_read != sizeof reply) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: reply was too small: %i\n",
                                   program_name, bytes_read);
            return EXIT_FAILURE;
        }

        if (reply.status.is_up) {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "%s: gui is up\n", program_name);
        } else {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "%s: the gui is down\n", program_name);
        }
    }

    return EXIT_SUCCESS;
}

static int run_stop(char const *program_name, int argc, char **argv)
{
    bool need_version = false;
    bool need_add_help = false;
    char const *bad_option = NULL;
    char const *bad_argument = NULL;
    unsigned last_index = 1;
    for (; last_index < (unsigned)argc; ++last_index) {
        char const *argument = argv[last_index];

        if (0 == strncmp(argument, "--", strlen("--"))) {
            if (0 == strcmp(argument, "--help")) {
                need_add_help = true;
            } else if (0 == strcmp(argument, "--version")) {
                need_version = true;
            } else {
                bad_option = argument;
            }
        } else {
            bad_argument = argument;
            break;
        }
    }

    if (need_add_help) {
        stop_help(STDOUT_FILENO,
                  program_name,
                  LINTED_STR(PACKAGE_NAME),
                  LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(STDERR_FILENO, program_name, bad_option);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    if (bad_argument != NULL) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: too many arguments: '%s'\n",
                               program_name, bad_argument);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    char const *path = getenv("LINTED_SOCKET");
    if (NULL == path) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing LINTED_SOCKET\n", program_name);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR("--help"));
        return EXIT_FAILURE;
    }

    size_t path_len = strlen(path);
    if (path_len > LINTED_MANAGER_PATH_MAX - 1) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: LINTED_SOCKET is too long\n", program_name);
        return EXIT_FAILURE;
    }

    linted_manager linted;

    {
        linted_manager manager;
        char buf[LINTED_MANAGER_PATH_MAX];

        buf[0] = '\0';
        memcpy(buf + 1, path, path_len);

        errno_t errnum = linted_manager_connect(&manager, buf, path_len + 1);
        if (errnum != 0) {
            failure(STDERR_FILENO, program_name,
                    LINTED_STR("can not create socket"), errnum);
            return EXIT_FAILURE;
        }
        linted = manager;
    }

    {
        union linted_manager_request request;
        memset(&request, 0, sizeof request);

        request.type = LINTED_MANAGER_STOP;
        request.stop.service = LINTED_MANAGER_SERVICE_GUI;

        linted_io_write_format(STDOUT_FILENO, NULL,
                               "%s: sending the stop request for the gui\n",
                               program_name);

        errno_t errnum = linted_manager_send_request(linted, &request);
        if (errnum != 0) {
            failure(STDERR_FILENO, program_name,
                    LINTED_STR("can send request"), errno);
            return EXIT_FAILURE;
        }
    }

    {
        union linted_manager_reply reply;
        size_t bytes_read;
        errno_t errnum = linted_manager_recv_reply(linted, &reply, &bytes_read);
        if (errnum != 0) {
            failure(STDERR_FILENO, program_name,
                    LINTED_STR("can not read reply"), errno);
            return EXIT_FAILURE;
        }

        if (0 == bytes_read) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: socket hung up\n", program_name);
            return EXIT_FAILURE;
        }

        if (reply.stop.was_up) {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "%s: gui was killed\n", program_name);
        } else {
            linted_io_write_format(STDOUT_FILENO, NULL,
                                   "%s: the gui was not killed\n",
                                   program_name);
        }
    }

    return EXIT_SUCCESS;
}

static errno_t ctl_help(int fildes, char const *program_name,
                        struct linted_str package_name,
                        struct linted_str package_url,
                        struct linted_str package_bugreport)
{
    errno_t errnum;

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" [OPTIONS]\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Run the manager program.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  LINTED_SOCKET       the socket of the linted game\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  status              request the status of the gui service\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t status_help(int fildes, char const *program_name,
                           struct linted_str package_name,
                           struct linted_str package_url,
                           struct linted_str package_bugreport)
{
    errno_t errnum;

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL,
                             LINTED_STR(" status [OPTIONS]\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report the status.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  LINTED_SOCKET       the socket of the linted game\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t stop_help(int fildes, char const *program_name,
                         struct linted_str package_name,
                         struct linted_str package_url,
                         struct linted_str package_bugreport)
{
    errno_t errnum;

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL,
                             LINTED_STR(" stop [OPTIONS]\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Stop a service.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  LINTED_SOCKET       the socket of the linted game\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t failure(int fildes, char const *program_name,
                       struct linted_str message, errno_t error)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, message)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    char const *error_string = linted_error_string_alloc(error);
    errnum = linted_io_write_string(fildes, NULL, error_string);
    linted_error_string_free(error_string);

    if (errnum != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    return 0;
}
