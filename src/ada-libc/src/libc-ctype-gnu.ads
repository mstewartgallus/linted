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
with Libc.xlocale_h;

package Libc.Ctype is
   pragma Pure;

   --  arg-macro: procedure isascii (c)
   --    __isascii (c)
   --  arg-macro: procedure toascii (c)
   --    __toascii (c)
   --  arg-macro: procedure isalnum_l (c, l)
   --    __isalnum_l ((c), (l))
   --  arg-macro: procedure isalpha_l (c, l)
   --    __isalpha_l ((c), (l))
   --  arg-macro: procedure iscntrl_l (c, l)
   --    __iscntrl_l ((c), (l))
   --  arg-macro: procedure isdigit_l (c, l)
   --    __isdigit_l ((c), (l))
   --  arg-macro: procedure islower_l (c, l)
   --    __islower_l ((c), (l))
   --  arg-macro: procedure isgraph_l (c, l)
   --    __isgraph_l ((c), (l))
   --  arg-macro: procedure isprint_l (c, l)
   --    __isprint_l ((c), (l))
   --  arg-macro: procedure ispunct_l (c, l)
   --    __ispunct_l ((c), (l))
   --  arg-macro: procedure isspace_l (c, l)
   --    __isspace_l ((c), (l))
   --  arg-macro: procedure isupper_l (c, l)
   --    __isupper_l ((c), (l))
   --  arg-macro: procedure isxdigit_l (c, l)
   --    __isxdigit_l ((c), (l))
   --  arg-macro: procedure isblank_l (c, l)
   --    __isblank_l ((c), (l))
   --  arg-macro: procedure isascii_l (c, l)
   --    __isascii_l ((c), (l))
   --  arg-macro: procedure toascii_l (c, l)
   --    __toascii_l ((c), (l))


   function isctype (c : int; mask : int) return int;  -- /usr/include/ctype.h:143
   pragma Import (C, isctype, "isctype");

   function isascii (c : int) return int;  -- /usr/include/ctype.h:150
   pragma Import (C, isascii, "isascii");

   function toascii (c : int) return int;  -- /usr/include/ctype.h:154
   pragma Import (C, toascii, "toascii");

   --  skipped func _toupper

   --  skipped func _tolower

   function isalnum_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:271
   pragma Import (C, isalnum_l, "isalnum_l");

   function isalpha_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:272
   pragma Import (C, isalpha_l, "isalpha_l");

   function iscntrl_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:273
   pragma Import (C, iscntrl_l, "iscntrl_l");

   function isdigit_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:274
   pragma Import (C, isdigit_l, "isdigit_l");

   function islower_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:275
   pragma Import (C, islower_l, "islower_l");

   function isgraph_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:276
   pragma Import (C, isgraph_l, "isgraph_l");

   function isprint_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:277
   pragma Import (C, isprint_l, "isprint_l");

   function ispunct_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:278
   pragma Import (C, ispunct_l, "ispunct_l");

   function isspace_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:279
   pragma Import (C, isspace_l, "isspace_l");

   function isupper_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:280
   pragma Import (C, isupper_l, "isupper_l");

   function isxdigit_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:281
   pragma Import (C, isxdigit_l, "isxdigit_l");

   function isblank_l (arg1 : int; arg2 : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:283
   pragma Import (C, isblank_l, "isblank_l");

   function tolower_l (c : int; l : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:288
   pragma Import (C, tolower_l, "tolower_l");

   function toupper_l (c : int; l : Libc.xlocale_h.locale_t) return int;  -- /usr/include/ctype.h:292
   pragma Import (C, toupper_l, "toupper_l");
end Libc.Ctype;
