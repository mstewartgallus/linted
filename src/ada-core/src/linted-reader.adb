-- Copyright 2015,2016,2017 Steven Stewart-Gallus
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
package body Linted.Reader is
   package C renames Interfaces.C;

   function Future_Is_Live
     (F : Future) return Boolean renames
     IO_Pool.Read_Future_Is_Live;

   procedure Read
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      F : out Future) renames
     IO_Pool.Read;

   procedure Read_Wait
     (F : in out Future;
      E : out Event) renames
     IO_Pool.Read_Wait;

   procedure Read_Poll
     (F : in out Future;
      E : out Event;
      Init : out Boolean) renames
     IO_Pool.Read_Poll;

   package body Worker is
      package IO_Worker is new IO_Pool.Reader_Worker (On_Event);

      procedure Read
        (Object : Linted.KOs.KO;
         Buf : System.Address;
         Count : C.size_t) renames
        IO_Worker.Read;
   end Worker;
end Linted.Reader;
