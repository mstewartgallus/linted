-- Copyright 2016 Steven Stewart-Gallus
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

package Pulse.Utf8 is

   function pa_utf8_valid
     (str : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/utf8.h:37
   pragma Import (C, pa_utf8_valid, "pa_utf8_valid");

   function pa_ascii_valid
     (str : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/utf8.h:40
   pragma Import (C, pa_ascii_valid, "pa_ascii_valid");

   function pa_utf8_filter
     (str : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/utf8.h:43
   pragma Import (C, pa_utf8_filter, "pa_utf8_filter");

   function pa_ascii_filter
     (str : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/utf8.h:46
   pragma Import (C, pa_ascii_filter, "pa_ascii_filter");

   function pa_utf8_to_locale
     (str : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/utf8.h:49
   pragma Import (C, pa_utf8_to_locale, "pa_utf8_to_locale");

   function pa_locale_to_utf8
     (str : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/utf8.h:52
   pragma Import (C, pa_locale_to_utf8, "pa_locale_to_utf8");

end Pulse.Utf8;
