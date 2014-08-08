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
#ifndef LINTED_DIR_H
#define LINTED_DIR_H

#include "linted/error.h"
#include "linted/ko.h"

#include <sys/types.h>

/**
 * @file
 *
 * Abstracts over the concept of a filesystem directory.
 *
 * @warning Directories are not atomically created and opened. If a
 *          directory is created somewhere that an attacker can
 *          manipulate a privileged program to delete directories in
 *          (perhaps a tmpcleaner program) then an attacker create his
 *          own directory in the spot and have one open his directory.
 */

typedef linted_ko linted_dir;

#define LINTED_DIR_EXCL (1UL << 0U)

linted_error linted_dir_create(linted_dir *dirp, linted_ko dirko,
                               char const *pathname, unsigned long flags,
                               mode_t mode);

#endif /* LINTED_DIR_H */
