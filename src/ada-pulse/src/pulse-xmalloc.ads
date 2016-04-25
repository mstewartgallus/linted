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
with Libc.Stddef;
with System;
with Interfaces.C.Strings;

package Pulse.XMalloc is

   --  arg-macro: function pa_xnew (type, n)
   --    return (type*) _pa_xnew_internal((n), sizeof(type));
   --  arg-macro: function pa_xnew0 (type, n)
   --    return (type*) _pa_xnew0_internal((n), sizeof(type));
   --  arg-macro: function pa_xnewdup (type, p, n)
   --    return (type*) _pa_xnewdup_internal((p), (n), sizeof(type));
   --  arg-macro: function pa_xrenew (type, p, n)
   --    return (type*) _pa_xrenew_internal(p, (n), sizeof(type));
   function pa_xmalloc (l : Libc.Stddef.size_t) return System.Address;  -- /usr/include/pulse/xmalloc.h:41
   pragma Import (C, pa_xmalloc, "pa_xmalloc");

   function pa_xmalloc0 (l : Libc.Stddef.size_t) return System.Address;  -- /usr/include/pulse/xmalloc.h:44
   pragma Import (C, pa_xmalloc0, "pa_xmalloc0");

   function pa_xrealloc (ptr : System.Address; size : Libc.Stddef.size_t) return System.Address;  -- /usr/include/pulse/xmalloc.h:47
   pragma Import (C, pa_xrealloc, "pa_xrealloc");

   procedure pa_xfree (p : System.Address);  -- /usr/include/pulse/xmalloc.h:50
   pragma Import (C, pa_xfree, "pa_xfree");

   function pa_xstrdup (s : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/xmalloc.h:53
   pragma Import (C, pa_xstrdup, "pa_xstrdup");

   function pa_xstrndup (s : Interfaces.C.Strings.chars_ptr; l : Libc.Stddef.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/xmalloc.h:56
   pragma Import (C, pa_xstrndup, "pa_xstrndup");

   function pa_xmemdup (p : System.Address; l : Libc.Stddef.size_t) return System.Address;  -- /usr/include/pulse/xmalloc.h:59
   pragma Import (C, pa_xmemdup, "pa_xmemdup");

   --  skipped func _pa_xnew_internal

   --  skipped func _pa_xnew0_internal

   --  skipped func _pa_xnewdup_internal

   --  skipped func _pa_xrenew_internal

end Pulse.XMalloc;
