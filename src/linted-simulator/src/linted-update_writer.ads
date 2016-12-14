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
with Ada.Synchronous_Task_Control;

with Linted.Errors;
with Linted.KOs;
with Linted.Options;

package Linted.Update_Writer is
   pragma Elaborate_Body;

   package Option_Events is new Linted.Options (Linted.Errors.Error);

   type Update_Int is range -2 ** (32 - 1) ..  2 ** (32 - 1) - 1;
   type Update_Nat is mod 2 ** 32;

   type Update is record
      X_Position : Update_Int := 0;
      Y_Position : Update_Int := 0;
      Z_Position : Update_Int := 0;

      Z_Rotation : Update_Nat := 0;
      X_Rotation : Update_Nat := 0;
   end record;

   generic
   package Worker with SPARK_Mode => Off is
      procedure Write (Object : Linted.KOs.KO; Data : Update);
      function Poll return Option_Events.Option;

      procedure Wait;
   end Worker;
end Linted.Update_Writer;
