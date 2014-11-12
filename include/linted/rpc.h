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

#define LINTED_RPC_UINT32_SIZE 4U
#define LINTED_RPC_INT32_SIZE 4U

static inline int_fast32_t linted_rpc_uint32_to_int32(uint_fast32_t positive)
{
    /*
   * Section 6.3.1.2 "Signed and unsigned integers" of the C99
   * standard specifies that the behaviour is implementation-defined
   * (or that a signal could be raised) if the new type is signed
   * and the value can't be represented in it so we do this.
   */
    if (positive > (int_fast64_t)INT32_MAX) {
        return -(uint_fast32_t)((UINT32_MAX - (int_fast64_t)positive) + 1U);
    }

    return positive;
}

static inline void linted_rpc_pack_uint32(uint_fast32_t fast, char * buf)
{

    uint_fast16_t low = ((uintmax_t) fast) & 0xFFFFU;
    uint_fast16_t high = (((uintmax_t) fast) >> 16U) & 0xFFFFU;

    unsigned char bytes[LINTED_RPC_UINT32_SIZE] = {
        ((uintmax_t) low) & 0xFFU,
        (((uintmax_t) low) >> 8U) & 0xFFU,
        ((uintmax_t) high) & 0xFFU,
        (((uintmax_t) high) >> 8U) & 0xFFU
    };

    memcpy(buf, bytes, sizeof bytes);
}

static inline uint_fast32_t linted_rpc_unpack_uint32(char const * buf)
{
    unsigned char pos_bytes[LINTED_RPC_UINT32_SIZE];
    memcpy(pos_bytes, buf, sizeof pos_bytes);

    uint_fast16_t low = ((uintmax_t) pos_bytes[0U])
        | (((uintmax_t) pos_bytes[1U]) << 8U);

    uint_fast16_t high = ((uintmax_t) pos_bytes[2U])
        | (((uintmax_t) pos_bytes[3U]) << 8U);

    uint_fast32_t positive = ((uintmax_t) low) | (((uintmax_t) high) << 16U);

    return positive;
}

static inline void linted_rpc_pack(int_fast32_t fast, char * buf)
{
    /*
     * Unlike with the code in unpack converting from a signed to an
     * unsigned value is not implementation defined.
     */
    uint_fast32_t positive = fast;

    uint_fast16_t low = ((uintmax_t) positive) & 0xFFFFU;
    uint_fast16_t high = (((uintmax_t) positive) >> 16U) & 0xFFFFU;

    unsigned char bytes[LINTED_RPC_INT32_SIZE] = {
        ((uintmax_t) low) & 0xFFU,
        (((uintmax_t) low) >> 8U) & 0xFFU,
        ((uintmax_t) high) & 0xFFU,
        (((uintmax_t) high) >> 8U) & 0xFFU
    };

    memcpy(buf, bytes, sizeof bytes);
}

static inline int_fast32_t linted_rpc_unpack(char const * buf)
{
    unsigned char pos_bytes[LINTED_RPC_INT32_SIZE];
    memcpy(pos_bytes, buf, sizeof pos_bytes);

    uint_fast16_t low = ((uintmax_t) pos_bytes[0U])
        | (((uintmax_t) pos_bytes[1U]) << 8U);

    uint_fast16_t high = ((uintmax_t) pos_bytes[2U])
        | (((uintmax_t) pos_bytes[3U]) << 8U);

    uint_fast32_t positive = ((uintmax_t) low) | (((uintmax_t) high) << 16U);

    return linted_rpc_uint32_to_int32(positive);
}

#endif                          /* LINTED_RPC_H */
