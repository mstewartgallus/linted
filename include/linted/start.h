/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LINTED_START_H
#define LINTED_START_H

#include <stddef.h>

#if defined HAVE_WINDOWS_API
#include <windows.h>
#endif

/**
 * @file
 *
 * Implements common startup functionality.
 */

struct linted_start_config {
	char const *canonical_process_name;
	unsigned char (*start)(char const *process_name, size_t argc,
	                       char const *const argv[]);
	_Bool dont_init_signals : 1U;
	_Bool dont_init_logging : 1U;
};

static struct linted_start_config const linted_start_config;

#if defined HAVE_WINDOWS_API
int linted_start_show_command(void);

int linted_start__main(struct linted_start_config const *config);
#else
int linted_start__main(struct linted_start_config const *config,
                       int argc, char *argv[]);
#endif

/* This is an awful hack to get linking to work right */
#ifndef LINTED_START__NO_MAIN
#if defined HAVE_WINDOWS_API
/* Hack around a bug in wclang by defining both of these. */
int WinMain(HINSTANCE program_instance, HINSTANCE prev_instance_unused,
            char *command_line_unused, int show_command_arg)
{
	return linted_start__main(&linted_start_config);
}

int wWinMain(HINSTANCE program_instance, HINSTANCE prev_instance_unused,
             wchar_t *command_line_unused, int show_command_arg)
{
	return linted_start__main(&linted_start_config);
}
#else
int main(int argc, char *argv[])
{
	return linted_start__main(&linted_start_config, argc, argv);
}
#endif
#endif

#endif /* LINTED_START_H */
