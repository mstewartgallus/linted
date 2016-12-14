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
with Linted.Errors;
with Linted.KOs;
with Linted.Options;

package Linted.Controls_Reader is
   pragma Elaborate_Body;

   type Controls_Int is range -2 ** (32 - 1) .. 2 ** (32 - 1) - 1;

   type Controls is record
      Z_Tilt : Controls_Int := 0;
      X_Tilt : Controls_Int := 0;

      Left : Boolean := False;
      Right : Boolean := False;
      Forward : Boolean := False;
      Back : Boolean := False;

      Jumping : Boolean := False;
   end record;

   type Event is record
      Data : Controls;
      Err : Errors.Error := 0;
   end record;

   package Option_Events is new Linted.Options (Event);

   generic
   package Worker with SPARK_Mode => Off is
      procedure Start (Object : KOs.KO);
      function Poll return Option_Events.Option;

      procedure Wait;
   end Worker;
end Linted.Controls_Reader;
