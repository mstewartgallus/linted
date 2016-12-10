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
#include "config.h"

#include "lntd/error.h"
#include "lntd/ko.h"

interface LntdStdio
{
	command lntd_error write(lntd_ko ko, char const *bytes,
	                         size_t size);
	command lntd_error write_line(lntd_ko ko, char const *str);

	command lntd_error read(lntd_ko ko, char *buf, size_t size,
	                        size_t *bytes_read);
	command lntd_error read_line(lntd_ko ko, char **bufp,
	                             size_t *sizep);
}
