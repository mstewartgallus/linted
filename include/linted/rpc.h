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

/**
 * @file
 *
 * Converts native numbers to lists of bytes.
 */

struct linted_rpc_int32 {
    char bytes[4u];
};

struct linted_rpc_uint32 {
    char bytes[4u];
};

static inline struct linted_rpc_uint32 linted_rpc_pack_uint32(uint_fast32_t fast)
{
    /*
     * Unlike with the code in unpack converting from a signed to an
     * unsigned value is not implementation defined.
     */
    uint_fast32_t positive = fast;

    unsigned char bytes[LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes)] = {
        ((uintmax_t) positive) & 0xFFu,
        (((uintmax_t) positive) >> 8u) & 0xFFu,
        (((uintmax_t) positive) >> 16u) & 0xFFu,
        (((uintmax_t) positive) >> 24u) & 0xFFu
    };

    struct linted_rpc_uint32 raw;
    memcpy(raw.bytes, bytes, sizeof raw.bytes);
    return raw;
}

static inline uint_fast32_t linted_rpc_unpack_uint32(struct linted_rpc_uint32 raw)
{
    unsigned char pos_bytes[sizeof raw.bytes];
    memcpy(pos_bytes, raw.bytes, sizeof raw.bytes);

    uint_fast32_t positive = ((uintmax_t) pos_bytes[0])
        | (((uintmax_t) pos_bytes[1]) << 8u)
        | (((uintmax_t) pos_bytes[2]) << 16u)
        | (((uintmax_t) pos_bytes[3]) << 24u);

    return positive;
}

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
    memcpy(raw.bytes, bytes, sizeof raw.bytes);
    return raw;
}

static inline int_fast32_t linted_rpc_unpack(struct linted_rpc_int32 raw)
{
    unsigned char pos_bytes[sizeof raw.bytes];
    memcpy(pos_bytes, raw.bytes, sizeof raw.bytes);

    uint_fast32_t positive = ((uintmax_t) pos_bytes[0])
        | (((uintmax_t) pos_bytes[1]) << 8u)
        | (((uintmax_t) pos_bytes[2]) << 16u)
        | (((uintmax_t) pos_bytes[3]) << 24u);

    return linted_uint32_to_int32(positive);
}

#endif                          /* LINTED_RPC_H */
