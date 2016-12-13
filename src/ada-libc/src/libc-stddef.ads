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
with Interfaces.C;

package Libc.Stddef is
   pragma Pure;

   --  unsupported macro: NULL __null
   --  arg-macro: procedure offsetof (TYPE, MEMBER)
   --    __builtin_offsetof (TYPE, MEMBER)
   subtype ptrdiff_t is Interfaces.C.ptrdiff_t;  -- /usr/lib/gcc/x86_64-linux-gnu/4.6/include/stddef.h:150

   subtype size_t is Interfaces.C.size_t;  -- /usr/lib/gcc/x86_64-linux-gnu/4.6/include/stddef.h:212

   --  subtype wchar_t is Interfaces.C.wchar_t;
   --  subtype wint_t is Interfaces.C.unsigned;
end Libc.Stddef;
