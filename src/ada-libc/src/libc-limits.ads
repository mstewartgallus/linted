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

package Libc.Limits is
   --  unsupported macro: MB_LEN_MAX 16
   --  unsupported macro: LLONG_MIN (-LLONG_MAX-1)
   --  unsupported macro: LLONG_MAX __LONG_LONG_MAX__
   --  unsupported macro: ULLONG_MAX (LLONG_MAX * 2ULL + 1)
end Libc.Limits;