/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#ifndef LINTED_RPC_H
#define LINTED_RPC_H

#include "linted/util.h"

#include <stdint.h>
#include <string.h>

#define INT_MIN (-(intmax_t) (UINTMAX_C(1) << 31u))

struct linted_rpc_int32 {
    char bytes[4];
};

static inline struct linted_rpc_int32 linted_rpc_pack(int_fast32_t fast)
{
    /*
     * Unlike with the code in unpack converting from a signed to an
     * unsigned value is not implementation defined.
     */
    uint_fast32_t positive = fast;

    unsigned char bytes[LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes)] = {
        ((uintmax_t) positive) & 0xFFu,
        (((uintmax_t) positive) >> 8u) & 0xFFu,
        (((uintmax_t) positive) >> 16u) & 0xFFu,
        (((uintmax_t) positive) >> 24u) & 0xFFu
    };

    struct linted_rpc_int32 raw;
    memcpy(raw.bytes, &bytes, sizeof raw.bytes);
    return raw;
}

static inline int_fast32_t linted_rpc_unpack(struct linted_rpc_int32 raw)
{
    unsigned char pos_bytes[sizeof raw.bytes];
    memcpy(&pos_bytes, raw.bytes, sizeof raw.bytes);

    uint_fast32_t positive = ((uintmax_t) pos_bytes[0])
        | (((uintmax_t) pos_bytes[1]) << 8u)
        | (((uintmax_t) pos_bytes[2]) << 16u)
        | (((uintmax_t) pos_bytes[3]) << 24u);

    /*
     * Section 6.3.1.2 "Signed and unsigned integers" of the C99
     * standard specifies that the behaviour is implementation-defined
     * (or that a signal could be raised) if the new type is signed
     * and the value can't be represented in it so we do this.
     */
    if (positive > (int_fast64_t) INT32_MAX) {
        return -(uint_fast32_t) ((UINT32_MAX - (int_fast64_t) positive) + 1u);
    }

    return positive;
}

#endif                          /* LINTED_RPC_H */
