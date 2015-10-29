/*
 * Copyright 2015 Steven Stewart-Gallus
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
#ifndef LINTED_PATH_H
#define LINTED_PATH_H

#include "linted/error.h"

/**
 * @file
 *
 * Provides standard filesystem paths
 */

linted_error linted_path_package_runtime_dir(char **packagep);
linted_error linted_path_package_data_home(char **packagep);

linted_error linted_path_base(char **basep, char const *str);
linted_error linted_path_dir(char **dirp, char const *str);

#endif /* LINTED_PATH_H */
