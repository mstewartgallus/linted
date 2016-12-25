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
with Linted.Errors;
with Linted.KOs;
with Linted.Update;

package Linted.Update_Reader with
     Spark_Mode => Off is
   pragma Elaborate_Body;

   type Event is record
      Data : Update.Packet;
      Err : Errors.Error := 0;
   end record;

   generic
   package Worker is
      procedure Start (Object : KOs.KO);
      procedure Wait (E : out Event);
   end Worker;
end Linted.Update_Reader;