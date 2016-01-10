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
with System;

with Linted.IO_Pool;
with Linted.KOs;
with Linted.Triggers;

package Linted.Reader is
   pragma Elaborate_Body;

   use Linted.KOs;
   use Linted.Triggers;

   subtype Event is Linted.IO_Pool.Reader_Event;

   package Option_Events renames Linted.IO_Pool.Option_Reader_Events;

   generic
      Event_Trigger : not null access Trigger;
   package Worker is
      procedure Read (Object : KO; Buf : System.Address; Count : Interfaces.C.size_t);
      function Poll return Option_Events.Option;
   end Worker;
end Linted.Reader;
