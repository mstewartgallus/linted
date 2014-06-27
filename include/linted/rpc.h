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

#define LINTED_RPC_UINT32_SIZE 4u
#define LINTED_RPC_INT32_SIZE 4u

static inline void linted_rpc_pack_uint32(uint_fast32_t fast, char * buf)
{

    uint_fast16_t low = ((uintmax_t) fast) & 0xFFFFu;
    uint_fast16_t high = (((uintmax_t) fast) >> 16u) & 0xFFFFu;

    unsigned char bytes[LINTED_RPC_UINT32_SIZE] = {
        ((uintmax_t) low) & 0xFFu,
        (((uintmax_t) low) >> 8u) & 0xFFu,
        ((uintmax_t) high) & 0xFFu,
        (((uintmax_t) high) >> 8u) & 0xFFu
    };

    memcpy(buf, bytes, sizeof bytes);
}

static inline uint_fast32_t linted_rpc_unpack_uint32(char const * buf)
{
    unsigned char pos_bytes[LINTED_RPC_UINT32_SIZE];
    memcpy(pos_bytes, buf, sizeof pos_bytes);

    uint_fast16_t low = ((uintmax_t) pos_bytes[0u])
        | (((uintmax_t) pos_bytes[1u]) << 8u);

    uint_fast16_t high = ((uintmax_t) pos_bytes[2u])
        | (((uintmax_t) pos_bytes[3u]) << 8u);

    uint_fast32_t positive = ((uintmax_t) low) | (((uintmax_t) high) << 16u);

    return positive;
}

static inline void linted_rpc_pack(int_fast32_t fast, char *buf)
{
    /*
     * Unlike with the code in unpack converting from a signed to an
     * unsigned value is not implementation defined.
     */
    uint_fast32_t positive = fast;

    uint_fast16_t low = ((uintmax_t) positive) & 0xFFFFu;
    uint_fast16_t high = (((uintmax_t) positive) >> 16u) & 0xFFFFu;

    unsigned char bytes[LINTED_RPC_INT32_SIZE] = {
        ((uintmax_t) low) & 0xFFu,
        (((uintmax_t) low) >> 8u) & 0xFFu,
        ((uintmax_t) high) & 0xFFu,
        (((uintmax_t) high) >> 8u) & 0xFFu
    };

    memcpy(buf, bytes, sizeof bytes);
}

static inline int_fast32_t linted_rpc_unpack(char const *buf)
{
    unsigned char pos_bytes[LINTED_RPC_INT32_SIZE];
    memcpy(pos_bytes, buf, sizeof pos_bytes);

    uint_fast16_t low = ((uintmax_t) pos_bytes[0u])
        | (((uintmax_t) pos_bytes[1u]) << 8u);

    uint_fast16_t high = ((uintmax_t) pos_bytes[2u])
        | (((uintmax_t) pos_bytes[3u]) << 8u);

    uint_fast32_t positive = ((uintmax_t) low) | (((uintmax_t) high) << 16u);

    return linted_uint32_to_int32(positive);
}

#endif                          /* LINTED_RPC_H */
