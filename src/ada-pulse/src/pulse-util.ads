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

with Libc.Stddef;

package Pulse.Util is

   function pa_get_user_name (s : Interfaces.C.Strings.chars_ptr; l : Libc.Stddef.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/util.h:37
   pragma Import (C, pa_get_user_name, "pa_get_user_name");

   function pa_get_host_name (s : Interfaces.C.Strings.chars_ptr; l : Libc.Stddef.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/util.h:40
   pragma Import (C, pa_get_host_name, "pa_get_host_name");

   function pa_get_fqdn (s : Interfaces.C.Strings.chars_ptr; l : Libc.Stddef.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/util.h:43
   pragma Import (C, pa_get_fqdn, "pa_get_fqdn");

   function pa_get_home_dir (s : Interfaces.C.Strings.chars_ptr; l : Libc.Stddef.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/util.h:46
   pragma Import (C, pa_get_home_dir, "pa_get_home_dir");

   function pa_get_binary_name (s : Interfaces.C.Strings.chars_ptr; l : Libc.Stddef.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/util.h:50
   pragma Import (C, pa_get_binary_name, "pa_get_binary_name");

   function pa_path_get_filename (p : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/util.h:54
   pragma Import (C, pa_path_get_filename, "pa_path_get_filename");

   function pa_msleep (t : unsigned_long) return int;  -- /usr/include/pulse/util.h:57
   pragma Import (C, pa_msleep, "pa_msleep");

end Pulse.Util;
