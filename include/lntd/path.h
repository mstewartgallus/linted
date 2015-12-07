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
#ifndef LNTD_PATH_H
#define LNTD_PATH_H

#include "lntd/error.h"

/**
 * @file
 *
 * Provides standard filesystem paths
 */

lntd_error lntd_path_package_runtime_dir(char **packagep);
lntd_error lntd_path_package_data_home(char **packagep);

lntd_error lntd_path_base(char **basep, char const *str);
lntd_error lntd_path_dir(char **dirp, char const *str);

#endif /* LNTD_PATH_H */
