/*
 * Copyright 2017 Steven Stewart-Gallus
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
#include <stdbool.h>
#include <stdint.h>

void atomic_store(uint64_t *atomic, uint64_t new)
{
	__atomic_store_n(atomic, new, __ATOMIC_SEQ_CST);
}

uint64_t atomic_load(uint64_t *atomic)
{
	return __atomic_load_n(atomic, __ATOMIC_SEQ_CST);
}

int atomic_compare_exchange(uint64_t *atomic, uint64_t expected,
                            uint64_t desired)
{
	return __atomic_compare_exchange_n(atomic, &expected, desired,
	                                   false, __ATOMIC_SEQ_CST,
	                                   __ATOMIC_SEQ_CST);
}

uint64_t atomic_exchange(uint64_t *atomic, uint64_t new)
{
	return __atomic_exchange_n(atomic, new, __ATOMIC_SEQ_CST);
}
