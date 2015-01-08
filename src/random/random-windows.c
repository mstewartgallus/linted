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
#define UNICODE
#define _UNICODE

#include "config.h"

#include "linted/random.h"

#include <assert.h>
#include <windows.h>

static HCRYPTPROV crypto_provider;

void linted_random_seed_generator(unsigned seed)
{
	if (!CryptAcquireContext(&crypto_provider, 0, 0, PROV_RSA_FULL,
	                         CRYPT_SILENT)) {
		assert(0);
	}
}

unsigned long linted_random_fast(void)
{
	BYTE bytes[sizeof(unsigned long)];
	if (!CryptGenRandom(crypto_provider, sizeof bytes, bytes)) {
		assert(0);
	}
	unsigned long number;
	memcpy(&number, bytes, sizeof number);
	return number;
}
