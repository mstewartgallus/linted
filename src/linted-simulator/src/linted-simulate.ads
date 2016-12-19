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
with Linted.Controls_Reader;
with Linted.Types;

package Linted.Simulate with Abstract_State => null is
   pragma Elaborate_Body;

   type State is record
      Controls : Linted.Controls_Reader.Controls;

      Positions : Types.Varying_Positions := (Types.X => (Value => 0, Old => 0),
					Types.Y => (Value => 0, Old => 0),
					Types.Z => (Value => 0, Old => 0));

      MX_Position : Types.Int := 0;
      MY_Position : Types.Int := 0;
      MZ_Position : Types.Int := 0;

      Z_Rotation : Types.Sim_Angle;
      X_Rotation : Types.Sim_Angle;
   end record;

   procedure Tick (This : in out State) with
     Global => null,
     Depends => (This => This);
end Linted.Simulate;
