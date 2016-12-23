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
with Linted.Types;

package Linted.Simulate with
     Abstract_State => null is
   pragma Elaborate_Body;

   type Object_State is array (0 .. 1) of Types.Varying_Positions;

   type State is record
      Controls : Types.Controls;

      Objects : Object_State := (0 .. 1 => (Types.X .. Types.Z => (0, 0)));

      Z_Rotation : Types.Sim_Angle;
      X_Rotation : Types.Sim_Angle;

      Counter : Types.Nat := 0;
   end record;

   procedure Tick (This : in out State) with
      Global => null,
      Depends => (This => This);
end Linted.Simulate;
