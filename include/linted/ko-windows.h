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
#ifndef LINTED_KO_H
#error this header should never be included directly
#endif

/* Use a fake type to give a little more type safety. */
struct linted_ko_;

typedef struct linted_ko_ *linted_ko;

#define LINTED_KO_CWD ((linted_ko)-1)

#define LINTED_KO_STDIN linted_ko__get_stdin()
#define LINTED_KO_STDOUT linted_ko__get_stdout()
#define LINTED_KO_STDERR linted_ko__get_stderr()

linted_ko linted_ko__get_stdin(void);
linted_ko linted_ko__get_stdout(void);
linted_ko linted_ko__get_stderr(void);
