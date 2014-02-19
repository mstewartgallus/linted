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
#include "linted/syslog.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/wait.h>
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

static int main_loop_wrapper(int const fildes[]);
static void close_in_out(void);
static int main_loop(void);

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
    linted_syslog_open();

    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        LINTED_LAZY_DEV_ERROR
            ("Could not run drop ability to raise privileges: %s",
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

    int command_status = -1;
    switch (argc) {
    case 1:{
        close_in_out();

        /* close the the logger so it isn't shared */
        closelog();

        int main_loop_status = main_loop();

        /* open the logger again */
        linted_syslog_open();

        if (-1 == main_loop_status) {
            char const * error_string = linted_error_string_alloc(errno);
            syslog(LOG_ERR, "could not run main loop: %s", error_string);
            linted_error_string_free(error_string);
            break;
        }
        command_status = 0;
        break;
    }

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
        close_in_out();
        break;

    default:
        fprintf(stderr,
                PACKAGE_TARNAME " did not understand the command line input\n"
                USAGE_TEXT);
        close_in_out();
        break;
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

static int main_loop(void)
{
    int exit_status = -1;
    linted_spawner spawners[2];
    if (-1 == linted_spawner_pair(spawners)) {
        return -1;
    }

    /* Fork off tasks from a known good state */
    pid_t child = fork();
    if (0 == child) {
        if (-1 == linted_spawner_close(spawners[1])) {
            exit(errno);
        }

        int preserved[] = { STDERR_FILENO };
        if (-1 == linted_process_spawner_run(spawners[0],
                                             preserved,
                                             LINTED_ARRAY_SIZE(preserved))) {
            exit(errno);
        }

        exit(EXIT_SUCCESS);
    }

    if (-1 == child) {
        goto close_spawners;
    }

    /* TODO: main loop code isn't needed any more */

    int main_loop_fildes[] = { spawners[1] };
    if (-1 == linted_spawner_spawn(spawners[1], main_loop_wrapper,
                                   main_loop_fildes,
                                   LINTED_ARRAY_SIZE(main_loop_fildes))) {
        goto close_spawners;
    }

    exit_status = 0;

close_spawners:
    {
        int errnum = errno;
        int close_status = linted_spawner_close(spawners[0]);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_spawner_close(spawners[1]);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    if (-1 == exit_status) {
        return -1;
    }

    /* TODO: Use the spawenrs exit status */
    int wait_status;
    do {
        wait_status = waitpid(child, NULL, 0);
    } while (-1 == wait_status && EINTR == errno);
    if (-1 == wait_status) {
        return -1;
    }

    return 0;
}

static int main_loop_wrapper(int const fds[])
{
    return linted_main_loop_run(fds[0]);
}

static void close_in_out(void)
{
    if (EOF == fclose(stdin)) {
        LINTED_LAZY_DEV_ERROR("Could not close standard input: %s",
                              linted_error_string_alloc(errno));
    }
    stdin = NULL;

    if (EOF == fclose(stdout)) {
        LINTED_LAZY_DEV_ERROR("Could not close standard output: %s",
                              linted_error_string_alloc(errno));
    }
    stdout = NULL;
}
