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
with Interfaces.C;
with System;

with Linted.IO_Pool;
with Linted.KOs;
with Linted.Triggers;

package Linted.Reader is
   pragma Elaborate_Body;

   subtype Event is Linted.IO_Pool.Reader_Event;
   subtype Future is Linted.IO_Pool.Read_Future;

   function Future_Is_Live (F : Future) return Boolean with
      Ghost;

   procedure Read
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      F : out Future) with
      Post => Future_Is_Live (F);

   procedure Read_Wait (F : in out Future; E : out Event) with
      Pre => Future_Is_Live (F),
      Post => not Future_Is_Live (F);

   procedure Read_Poll
     (F : in out Future;
      E : out Event;
      Init : out Boolean) with
      Pre => Future_Is_Live (F),
      Post => (if Init then not Future_Is_Live (F) else Future_Is_Live (F));

   generic
      with procedure On_Event (E : Event);
   package Worker is
      procedure Read
        (Object : Linted.KOs.KO;
         Buf : System.Address;
         Count : Interfaces.C.size_t);
   end Worker;
end Linted.Reader;
