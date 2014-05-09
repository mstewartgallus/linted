/*
 * Copyright 2013 Steven Stewart-Gallus
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
#ifndef LINTED_CHECK_H
#define LINTED_CHECK_H

#include <limits.h>
#include <float.h>

/**
 * @file
 *
 * Utility module for checking for overflow of constants.
 */

#define LINTED_CHECK_UNSIGNED(T, X) (0u / ((X) <= ((T) -1)) + X)

#define LINTED_CHECK_SIGNED_MAX_(T) (                   \
        sizeof(T) == sizeof(signed char) ? SCHAR_MAX :  \
        sizeof(T) == sizeof(short)       ? SHRT_MAX  :  \
        sizeof(T) == sizeof(int)         ? INT_MAX   :  \
        sizeof(T) == sizeof(long)        ? LONG_MAX  :  \
        sizeof(T) == sizeof(long long)   ? LLONG_MAX :  \
        0u / 0u)

#define LINTED_CHECK_SIGNED_MIN_(T) (                   \
        sizeof(T) == sizeof(signed char) ? SCHAR_MIN :  \
        sizeof(T) == sizeof(short)       ? SHRT_MIN  :  \
        sizeof(T) == sizeof(int)         ? INT_MIN   :  \
        sizeof(T) == sizeof(long)        ? LONG_MIN  :  \
        sizeof(T) == sizeof(long long)   ? LLONG_MIN :  \
        0u / 0u)

#define LINTED_CHECK_SIGNED(T, X) (                 \
        0 / ((X) >= LINTED_CHECK_SIGNED_MIN_(T))    \
        + 0 / ((X) <= LINTED_CHECK_SIGNED_MAX_(T))  \
        + X)

#define LINTED_CHECK_FLOAT_MAX_(T) (                \
        sizeof(T) == sizeof(float) ? FLT_MAX :      \
        sizeof(T) == sizeof(double) ? DBL_MAX  :    \
        0u / 0u)

#define LINTED_CHECK_FLOAT_MIN_(T) (                \
        sizeof(T) == sizeof(float) ? -FLT_MAX :     \
        sizeof(T) == sizeof(double) ? -DBL_MAX  :   \
        0u / 0u)

#define LINTED_CHECK_FLOAT(T, X) (                     \
        0 / ((X) >= LINTED_CHECK_FLOAT_MIN_(T))        \
        + 0 / ((X) <= LINTED_CHECK_FLOAT_MAX_(T))      \
        + X)

#endif                          /* LINTED_CHECK_H */
