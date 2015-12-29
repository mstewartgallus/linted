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
#ifndef LNTD_CONTROLLER_H
#define LNTD_CONTROLLER_H

nx_struct lntd_controller_input
{
	nx_int32_t z_tilt;
	nx_int32_t x_tilt;

	nx_uint8_t left : 1U;
	nx_uint8_t right : 1U;
	nx_uint8_t forward : 1U;
	nx_uint8_t back : 1U;

	nx_uint8_t jumping : 1U;
};

#endif /* LNTD_CONTROLLER_H */
