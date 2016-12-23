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
with Interfaces.C.Strings;

package Libc.Locale with
     Spark_Mode => Off is
   pragma Preelaborate;

   --  unsupported macro: LC_ALL __LC_ALL
   --  unsupported macro: LC_CTYPE __LC_CTYPE
   --  unsupported macro: LC_NUMERIC __LC_NUMERIC
   --  unsupported macro: LC_COLLATE __LC_COLLATE
   --  unsupported macro: LC_MONETARY __LC_MONETARY
   --  unsupported macro: LC_TIME __LC_TIME

   type lconv is record
      decimal_point : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:57
      thousands_sep : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:58
      grouping : Interfaces.C.Strings.chars_ptr;  -- /usr/include/locale.h:64
      int_curr_symbol : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:70
      currency_symbol : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:71
      mon_decimal_point : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:72
      mon_thousands_sep : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:73
      mon_grouping : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:74
      positive_sign : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:75
      negative_sign : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/locale.h:76
      int_frac_digits : aliased char;  -- /usr/include/locale.h:77
      frac_digits : aliased char;  -- /usr/include/locale.h:78
      p_cs_precedes : aliased char;  -- /usr/include/locale.h:80
      p_sep_by_space : aliased char;  -- /usr/include/locale.h:82
      n_cs_precedes : aliased char;  -- /usr/include/locale.h:84
      n_sep_by_space : aliased char;  -- /usr/include/locale.h:86
      p_sign_posn : aliased char;  -- /usr/include/locale.h:93
      n_sign_posn : aliased char;  -- /usr/include/locale.h:94
      int_p_cs_precedes : aliased char;  -- /usr/include/locale.h:97
      int_p_sep_by_space : aliased char;  -- /usr/include/locale.h:99
      int_n_cs_precedes : aliased char;  -- /usr/include/locale.h:101
      int_n_sep_by_space : aliased char;  -- /usr/include/locale.h:103
      int_p_sign_posn : aliased char;  -- /usr/include/locale.h:110
      int_n_sign_posn : aliased char;  -- /usr/include/locale.h:111
   end record;
   pragma Convention (C_Pass_By_Copy, lconv);  -- /usr/include/locale.h:53

   function setlocale
     (category : int;
      locale : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/locale.h:124
   pragma Import (C, setlocale, "setlocale");

   function localeconv return access lconv;  -- /usr/include/locale.h:127
   pragma Import (C, localeconv, "localeconv");
end Libc.Locale;
