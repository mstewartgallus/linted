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
#include "linted/main_loop.h"
#include "linted/process_spawner.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>


#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " [OPTIONS] [SUBCOMMAND]\n"\
    "Play the " PACKAGE_NAME " game\n"\
    "\n"\
    "The following OPTIONS are accepted:\n"\
    "--version\t\tJust print version info and return\n"\
    "--help\t\tPrint this usage information\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

#define VERSION_TEXT \
    PACKAGE_STRING "\n"\
    "Copyright (C) 2013 Steven Stewart-Gallus\n"\
    PACKAGE_NAME " comes with ABSOLUTELY NO WARRANTY.\n"\
    "You may redistribute copies of Linted\n"\
    "under the terms of the Apache License.\n"\
    "For more information about these matters, see the file named COPYING.\n"

extern char **environ;

static int run_game(void);

int main(int argc, char **argv)
{
    /* First we check if we are run with proper security */
    uid_t const uid = getuid();
    uid_t const euid = geteuid();
    if (0 == euid || 0 == uid) {
        fputs("Bad administrator!\n" "It is insecure to run a game as root!\n",
              stderr);
        return EXIT_FAILURE;
    }

    openlog(PACKAGE_TARNAME, LOG_PERROR /* So the user can see this */
            | LOG_CONS          /* So we know there is an error */
            | LOG_PID           /* Because we fork several times */
            , LOG_USER          /* This is a game and is a user program */
        );

    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        LINTED_LAZY_DEV_ERROR("could not drop ability to raise privileges: %s",
                              linted_error_string_alloc(errno));
    }

    /* Sanitize open files */
    fd_set essential_fds;
    FD_ZERO(&essential_fds);
    FD_SET(STDERR_FILENO, &essential_fds);
    FD_SET(STDIN_FILENO, &essential_fds);
    FD_SET(STDOUT_FILENO, &essential_fds);
    if (-1 == linted_io_close_fds_except(&essential_fds)) {
        LINTED_LAZY_DEV_ERROR("could not close unneeded files: %s",
                              linted_error_string_alloc(errno));
    }

    /* Sanitize the environment */
    char **env = environ;
    for (; *env != NULL; ++env) {
        /* TODO: Only pass the display variable in for the gui task */
        size_t const env_length = strlen(*env);
        char const needle[] = "DISPLAY=";
        size_t const needle_length = sizeof needle - 1;
        if (env_length >= needle_length) {
            if (0 == memcmp(needle, *env, needle_length)) {
                continue;
            }
        }

        char *sensitive_data = strchr(*env, '=') + 1;

        memset(sensitive_data, '\0', env_length - (sensitive_data - *env));
    }

    bool should_run_game = false;
    int command_status = 0;
    switch (argc) {
    case 1:
        /* No arguments */
        should_run_game = true;
        break;

    case 2:
        if (0 == strcmp(argv[1], "--help")) {
            fputs(USAGE_TEXT, stdout);
        } else if (0 == strcmp(argv[1], "--version")) {
            fputs(VERSION_TEXT, stdout);
        } else {
            fprintf(stderr,
                    PACKAGE_TARNAME
                    " did not understand the command line input\n" USAGE_TEXT);
        }
        break;

    default:
        fprintf(stderr,
                PACKAGE_TARNAME " did not understand the command line input\n"
                USAGE_TEXT);
        break;
    }

    if (-1 == close(STDIN_FILENO)) {
        command_status = -1;
        char const *error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not close standard input: %s", error_string);
        linted_error_string_free(error_string);
    }

    if (-1 == close(STDOUT_FILENO)) {
        command_status = -1;
        char const *error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not close standard output: %s", error_string);
        linted_error_string_free(error_string);
    }

    if (should_run_game && command_status != -1) {
        if (-1 == run_game()) {
            command_status = -1;
            char const *error_string = linted_error_string_alloc(errno);
            syslog(LOG_ERR, "could not run the game: %s", error_string);
            linted_error_string_free(error_string);
        }
    }

    if (-1 == close(STDERR_FILENO)) {
        command_status = -1;
        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not close standard error: %s", error_string);
        linted_error_string_free(error_string);
    }

    closelog();

    return (-1 == command_status) ? EXIT_FAILURE : EXIT_SUCCESS;
}

static int run_game(void)
{
    int exit_status = -1;

    linted_spawner spawners[2];
    if (-1 == linted_spawner_pair(spawners)) {
        return -1;
    }

    pid_t spawner = fork();
    if (-1 == spawner) {
        goto close_spawner_handles;
    }

    if (0 == spawner) {
        close(spawners[1]);

        /* Fork off tasks from a known good state */
        int spawner_status = linted_process_spawner_run(spawners[0], NULL);

        if (-1 == spawner_status) {
            exit(errno);
        }
        exit(0);
    }

    if (-1 == linted_main_loop_run(spawners[1])) {
        goto close_spawner_handles;
    }

    exit_status = 0;

 close_spawner_handles:
    {
        int errnum = errno;
        int close_status = close(spawners[0]);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = close(spawners[1]);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    if (exit_status != -1) {
        waitpid(spawner, NULL, 0);
    }

    return exit_status;
}
