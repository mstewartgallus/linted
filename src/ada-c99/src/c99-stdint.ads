pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package C99.Stdint is
   pragma Pure;

   INT8_MIN : constant := -128;
   INT16_MIN : constant := -32767-1;
   INT32_MIN : constant := -2147483647-1;
   INT64_MIN : constant := -9223372036854775807-1;
   INT8_MAX : constant := 127;
   INT16_MAX : constant := 32767;
   INT32_MAX : constant := 2147483647;
   INT64_MAX : constant := 9223372036854775807;
   UINT8_MAX : constant := 255;
   UINT16_MAX : constant := 65535;
   UINT32_MAX : constant := 4294967295;
   UINT64_MAX : constant := 18446744073709551615;
   INT_LEAST8_MIN : constant := -128;
   INT_LEAST16_MIN : constant := -32767-1;
   INT_LEAST32_MIN : constant := -2147483647-1;
   INT_LEAST64_MIN : constant := -9223372036854775807-1;
   INT_LEAST8_MAX : constant := 127;
   INT_LEAST16_MAX : constant := 32767;
   INT_LEAST32_MAX : constant := 2147483647;
   INT_LEAST64_MAX : constant := 9223372036854775807;
   UINT_LEAST8_MAX : constant := 255;
   UINT_LEAST16_MAX : constant := 65535;
   UINT_LEAST32_MAX : constant := 4294967295;
   UINT_LEAST64_MAX : constant := 18446744073709551615;
   INT_FAST8_MIN : constant := -128;
   INT_FAST16_MIN : constant := -9223372036854775807-1;
   INT_FAST32_MIN : constant := -9223372036854775807-1;
   INT_FAST64_MIN : constant := -9223372036854775807-1;
   INT_FAST8_MAX : constant := 127;
   INT_FAST16_MAX : constant := 9223372036854775807;
   INT_FAST32_MAX : constant := 9223372036854775807;
   INT_FAST64_MAX : constant := 9223372036854775807;
   UINT_FAST8_MAX : constant := 255;
   UINT_FAST16_MAX : constant := 18446744073709551615;
   UINT_FAST32_MAX : constant := 18446744073709551615;
   UINT_FAST64_MAX : constant := 18446744073709551615;
   INTPTR_MIN : constant := -9223372036854775807-1;
   INTPTR_MAX : constant := 9223372036854775807;
   UINTPTR_MAX : constant := 18446744073709551615;
   INTMAX_MIN : constant := -9223372036854775807;
   INTMAX_MAX : constant := 9223372036854775807;
   UINTMAX_MAX : constant := 18446744073709551615;
   PTRDIFF_MIN : constant := -9223372036854775807;
   PTRDIFF_MAX : constant := 9223372036854775807;
   SIG_ATOMIC_MIN : constant := -2147483647 -1;
   SIG_ATOMIC_MAX : constant := 2147483647;
   SIZE_MAX : constant := 18446744073709551615;

   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX
   WINT_MIN : constant := 0;
   WINT_MAX : constant := 4294967295;

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
end C99.Stdint;
