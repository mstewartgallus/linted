-- Copyright 2015 Steven Stewart-Gallus
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
with Interfaces.C; use Interfaces.C;

package Libc.Stdint is
   pragma Pure;

   subtype int8_t is signed_char;  -- /usr/include/stdint.h:36
   subtype int16_t is short;  -- /usr/include/stdint.h:37
   subtype int32_t is int;  -- /usr/include/stdint.h:38
   subtype int64_t is long;  -- /usr/include/stdint.h:40

   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:48
   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:49
   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:51
   subtype uint64_t is unsigned_long;  -- /usr/include/stdint.h:55

   subtype int_least8_t is signed_char;  -- /usr/include/stdint.h:65
   subtype int_least16_t is short;  -- /usr/include/stdint.h:66
   subtype int_least32_t is int;  -- /usr/include/stdint.h:67
   subtype int_least64_t is long;  -- /usr/include/stdint.h:69

   subtype uint_least8_t is unsigned_char;  -- /usr/include/stdint.h:76
   subtype uint_least16_t is unsigned_short;  -- /usr/include/stdint.h:77
   subtype uint_least32_t is unsigned;  -- /usr/include/stdint.h:78
   subtype uint_least64_t is unsigned_long;  -- /usr/include/stdint.h:80

   subtype int_fast8_t is signed_char;  -- /usr/include/stdint.h:90
   subtype int_fast16_t is long;  -- /usr/include/stdint.h:92
   subtype int_fast32_t is long;  -- /usr/include/stdint.h:93
   subtype int_fast64_t is long;  -- /usr/include/stdint.h:94

   subtype uint_fast8_t is unsigned_char;  -- /usr/include/stdint.h:103
   subtype uint_fast16_t is unsigned_long;  -- /usr/include/stdint.h:105
   subtype uint_fast32_t is unsigned_long;  -- /usr/include/stdint.h:106
   subtype uint_fast64_t is unsigned_long;  -- /usr/include/stdint.h:107

   subtype intptr_t is long;  -- /usr/include/stdint.h:119
   subtype uintptr_t is unsigned_long;  -- /usr/include/stdint.h:122

   subtype intmax_t is long;  -- /usr/include/stdint.h:134
   subtype uintmax_t is unsigned_long;  -- /usr/include/stdint.h:135

   INT8_MIN : constant := int8_t'First;
   INT16_MIN : constant := int16_t'First;
   INT32_MIN : constant := int32_t'First;
   INT64_MIN : constant := int64_t'First;

   INT8_MAX : constant := int8_t'Last;
   INT16_MAX : constant := int16_t'Last;
   INT32_MAX : constant := int32_t'Last;
   INT64_MAX : constant := int64_t'Last;

   UINT8_MAX : constant := uint8_t'Last;
   UINT16_MAX : constant := uint16_t'Last;
   UINT32_MAX : constant := uint32_t'Last;
   UINT64_MAX : constant := uint64_t'Last;

   INT_LEAST8_MIN : constant := int_least8_t'First;
   INT_LEAST16_MIN : constant := int_least16_t'First;
   INT_LEAST32_MIN : constant := int_least32_t'First;
   INT_LEAST64_MIN : constant := int_least64_t'First;

   INT_LEAST8_MAX : constant := int_least8_t'Last;
   INT_LEAST16_MAX : constant := int_least16_t'Last;
   INT_LEAST32_MAX : constant := int_least32_t'Last;
   INT_LEAST64_MAX : constant := int_least64_t'Last;

   UINT_LEAST8_MAX : constant := uint_least8_t'Last;
   UINT_LEAST16_MAX : constant := uint_least16_t'Last;
   UINT_LEAST32_MAX : constant := uint_least32_t'Last;
   UINT_LEAST64_MAX : constant := uint_least64_t'Last;

   INT_FAST8_MIN : constant := int_fast8_t'First;
   INT_FAST16_MIN : constant := int_fast16_t'First;
   INT_FAST32_MIN : constant := int_fast32_t'First;
   INT_FAST64_MIN : constant := int_fast64_t'First;

   INT_FAST8_MAX : constant := int_fast8_t'Last;
   INT_FAST16_MAX : constant := int_fast16_t'Last;
   INT_FAST32_MAX : constant := int_fast32_t'Last;
   INT_FAST64_MAX : constant := int_fast64_t'Last;

   UINT_FAST8_MAX : constant := uint_fast8_t'Last;
   UINT_FAST16_MAX : constant := uint_fast16_t'Last;
   UINT_FAST32_MAX : constant := uint_fast32_t'Last;
   UINT_FAST64_MAX : constant := uint_fast64_t'Last;

   INTPTR_MIN : constant := intptr_t'First;
   INTPTR_MAX : constant := intptr_t'Last;

   UINTPTR_MAX : constant := uintptr_t'Last;

   INTMAX_MIN : constant := intmax_t'First;
   INTMAX_MAX : constant := intmax_t'Last;

   UINTMAX_MAX : constant := uintmax_t'Last;

   PTRDIFF_MIN : constant := ptrdiff_t'First;
   PTRDIFF_MAX : constant := ptrdiff_t'Last;

   SIG_ATOMIC_MIN : constant := -2147483647 - 1;
   SIG_ATOMIC_MAX : constant := 2147483647;

   SIZE_MAX : constant := size_t'Last;

   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX
   WINT_MIN : constant := 0;
   WINT_MAX : constant := 4294967295;
end Libc.Stdint;
