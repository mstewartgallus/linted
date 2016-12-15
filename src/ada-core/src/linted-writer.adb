-- Copyright 2015,2016 Steven Stewart-Gallus
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
package body Linted.Writer is
   package C renames Interfaces.C;

   package body Worker is
      package IO_Worker is new IO_Pool.Writer_Worker;

      procedure Write (Object : Linted.KOS.KO; Buf : System.Address; Count : C.size_t) renames IO_Worker.Write;
      function Poll return Option_Events.Option renames IO_Worker.Poll;
      procedure Wait renames IO_Worker.Wait;
   end Worker;
end Linted.Writer;
