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
with Linted.Errors;
with Linted.KOs;
with Linted.Options;
with Linted.Triggers;

package Linted.Controls_Writer is
   pragma Elaborate_Body;

   package Option_Events is new Linted.Options (Linted.Errors.Error);

   type Controls_Int is range -2 ** (32 - 1) ..  2 ** (32 - 1) - 1;

   type Controls is record
      Z_Tilt : Controls_Int;
      X_Tilt : Controls_Int;

      Left : Boolean;
      Right : Boolean;
      Forward : Boolean;
      Back : Boolean;

      Jumping : Boolean;
   end record;

   generic
      Event_Trigger : not null access Linted.Triggers.Trigger;
   package Worker is
      procedure Write (Object : Linted.KOs.KO; Data : Controls);
      function Poll return Option_Events.Option;
   end Worker;
end Linted.Controls_Writer;
