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
with System;

package Libc.Locale.GNU is
   pragma Preelaborate;

   type uu_locale_struct_uu_locales_array is array (0 .. 12) of System.Address;
   type uu_locale_struct_uu_names_array is array (0 .. 12) of Interfaces.C.Strings.chars_ptr;
   type uu_locale_struct is record
      uu_locales : aliased uu_locale_struct_uu_locales_array;  -- /usr/include/xlocale.h:30
      uu_ctype_b : access unsigned_short;  -- /usr/include/xlocale.h:33
      uu_ctype_tolower : access int;  -- /usr/include/xlocale.h:34
      uu_ctype_toupper : access int;  -- /usr/include/xlocale.h:35
      uu_names : aliased uu_locale_struct_uu_names_array;  -- /usr/include/xlocale.h:38
   end record;
   pragma Convention (C_Pass_By_Copy, uu_locale_struct);  -- /usr/include/xlocale.h:27

   --  skipped empty struct uu_locale_data

   type uu_locale_t is access all uu_locale_struct;  -- /usr/include/xlocale.h:39

   subtype locale_t is uu_locale_t;  -- /usr/include/xlocale.h:42

   --  unsupported macro: LC_MESSAGES __LC_MESSAGES
   --  unsupported macro: LC_PAPER __LC_PAPER
   --  unsupported macro: LC_NAME __LC_NAME
   --  unsupported macro: LC_ADDRESS __LC_ADDRESS
   --  unsupported macro: LC_TELEPHONE __LC_TELEPHONE
   --  unsupported macro: LC_MEASUREMENT __LC_MEASUREMENT
   --  unsupported macro: LC_IDENTIFICATION __LC_IDENTIFICATION
   --  unsupported macro: LC_CTYPE_MASK (1 << __LC_CTYPE)
   --  unsupported macro: LC_NUMERIC_MASK (1 << __LC_NUMERIC)
   --  unsupported macro: LC_TIME_MASK (1 << __LC_TIME)
   --  unsupported macro: LC_COLLATE_MASK (1 << __LC_COLLATE)
   --  unsupported macro: LC_MONETARY_MASK (1 << __LC_MONETARY)
   --  unsupported macro: LC_MESSAGES_MASK (1 << __LC_MESSAGES)
   --  unsupported macro: LC_PAPER_MASK (1 << __LC_PAPER)
   --  unsupported macro: LC_NAME_MASK (1 << __LC_NAME)
   --  unsupported macro: LC_ADDRESS_MASK (1 << __LC_ADDRESS)
   --  unsupported macro: LC_TELEPHONE_MASK (1 << __LC_TELEPHONE)
   --  unsupported macro: LC_MEASUREMENT_MASK (1 << __LC_MEASUREMENT)
   --  unsupported macro: LC_IDENTIFICATION_MASK (1 << __LC_IDENTIFICATION)
   --  unsupported macro: LC_ALL_MASK (LC_CTYPE_MASK | LC_NUMERIC_MASK | LC_TIME_MASK | LC_COLLATE_MASK | LC_MONETARY_MASK | LC_MESSAGES_MASK | LC_PAPER_MASK | LC_NAME_MASK | LC_ADDRESS_MASK | LC_TELEPHONE_MASK | LC_MEASUREMENT_MASK | LC_IDENTIFICATION_MASK )
   --  unsupported macro: LC_GLOBAL_LOCALE ((__locale_t) -1L)

   function newlocale
     (category_mask : int;
      locale : Interfaces.C.Strings.chars_ptr;
      base : locale_t) return locale_t;  -- /usr/include/locale.h:151
   pragma Import (C, newlocale, "newlocale");

   function duplocale (dataset : locale_t) return locale_t;  -- /usr/include/locale.h:186
   pragma Import (C, duplocale, "duplocale");

   procedure freelocale (dataset : locale_t);  -- /usr/include/locale.h:190
   pragma Import (C, freelocale, "freelocale");

   function uselocale (dataset : locale_t) return locale_t;  -- /usr/include/locale.h:197
   pragma Import (C, uselocale, "uselocale");

end Libc.Locale.GNU;
