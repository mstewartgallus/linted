/*
 * Copyright 2015,2016 Steven Stewart-Gallus
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
#ifndef LNTD_UPDATE_H
#define LNTD_UPDATE_H

nx_struct lntd_update_input
{
	nx_int32_t x_position;
	nx_int32_t y_position;
	nx_int32_t z_position;

	nx_int32_t mx_position;
	nx_int32_t my_position;
	nx_int32_t mz_position;

	nx_uint32_t z_rotation;
	nx_uint32_t x_rotation;
};

#endif /* LNTD_UPDATE_H */
