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
   function isalnum (arg1 : int) return int;  -- /usr/include/ctype.h:110
   pragma Import (C, isalnum, "isalnum");

   function isalpha (arg1 : int) return int;  -- /usr/include/ctype.h:111
   pragma Import (C, isalpha, "isalpha");

   function isblank (arg1 : int) return int;  -- /usr/include/ctype.h:136
   pragma Import (C, isblank, "isblank");

   function iscntrl (arg1 : int) return int;  -- /usr/include/ctype.h:112
   pragma Import (C, iscntrl, "iscntrl");

   function isdigit (arg1 : int) return int;  -- /usr/include/ctype.h:113
   pragma Import (C, isdigit, "isdigit");

   function isgraph (arg1 : int) return int;  -- /usr/include/ctype.h:115
   pragma Import (C, isgraph, "isgraph");

   function islower (arg1 : int) return int;  -- /usr/include/ctype.h:114
   pragma Import (C, islower, "islower");

   function isprint (arg1 : int) return int;  -- /usr/include/ctype.h:116
   pragma Import (C, isprint, "isprint");

   function ispunct (arg1 : int) return int;  -- /usr/include/ctype.h:117
   pragma Import (C, ispunct, "ispunct");

   function isspace (arg1 : int) return int;  -- /usr/include/ctype.h:118
   pragma Import (C, isspace, "isspace");

   function isupper (arg1 : int) return int;  -- /usr/include/ctype.h:119
   pragma Import (C, isupper, "isupper");

   function isxdigit (arg1 : int) return int;  -- /usr/include/ctype.h:120
   pragma Import (C, isxdigit, "isxdigit");

   function tolower (c : int) return int;  -- /usr/include/ctype.h:124
   pragma Import (C, tolower, "tolower");

   function toupper (c : int) return int;  -- /usr/include/ctype.h:127
   pragma Import (C, toupper, "toupper");
end Libc.Ctype;
