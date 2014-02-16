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

#include "linted/main_loop.h"
#include "linted/spawner.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <unistd.h>

#if defined(__linux__)
extern char **environ;
#else
#error There is no portable method for environment variable sanitization
#endif

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

static int main_loop_wrapper(linted_spawner_t spawner, int const fildes[]);

int main(int argc, char **argv)
{
    /* First we check if we are run with proper security */
#ifdef HAVE_UID_T
    uid_t const euid = geteuid();
    if (0 == euid) {
        fputs("Bad administrator!\n" "It is insecure to run a game as root!\n",
              stderr);
        return EXIT_FAILURE;
    }
#endif                          /* HAVE_UID_T */

    /* Prepare the system logger */
    openlog(PACKAGE_TARNAME, LOG_PERROR /* So the user can see this */
            | LOG_CONS          /* So we know there is an error */
            | LOG_PID           /* Because we fork several times */
            | LOG_NDELAY        /* Share the connection when we fork */
            , LOG_USER          /* This is a game and is a user program */
        );

    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        LINTED_LAZY_DEV_ERROR
            ("Could not run drop ability to raise privileges: %s",
             linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    /* Sanitize the environment */
    char ** env = environ;
    for (;*env != NULL; ++env) {
        /* TODO: Only pass the display variable in for the gui task */
        if (0 > strcmp("DISPLAY=", *env)) {
            continue;
        }

        char const needle[] = "=";
        char * sensitive_data = strstr(*env, needle) + (sizeof needle - 1);

        for (char * xx = sensitive_data; *xx != '\0'; ++xx) {
            *xx = '\0';
        }
    }

    int command_status = -1;
    switch (argc) {
    case 1:
        /* Fork off tasks from a known good state */
        if (-1 == linted_spawner_run(main_loop_wrapper, (int[]) {
                                     -1})) {
            char const *const error_string = linted_error_string_alloc(errno);
            syslog(LOG_ERR, "Could not run spawner: %s", error_string);
            linted_error_string_free(error_string);
            break;
        }
        command_status = 0;
        break;

    case 2:
        if (0 == strcmp(argv[1], "--help")) {
            fputs(USAGE_TEXT, stdout);
            command_status = 0;
        } else if (0 == strcmp(argv[1], "--version")) {
            fputs(VERSION_TEXT, stdout);
            command_status = 0;
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

    if (EOF == fclose(stdin)) {
        command_status = -1;
        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "Could not close standard input: %s", error_string);
        linted_error_string_free(error_string);
    }

    if (EOF == fclose(stdout)) {
        command_status = -1;
        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "Could not close standard output: %s", error_string);
        linted_error_string_free(error_string);
    }

    if (EOF == fclose(stderr)) {
        command_status = -1;
        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "Could not close standard error: %s", error_string);
        linted_error_string_free(error_string);
    }

    closelog();

    return (-1 == command_status) ? EXIT_FAILURE : EXIT_SUCCESS;
}

static int main_loop_wrapper(linted_spawner_t spawner, int const fds[])
{
    return linted_main_loop_run(spawner);
}
