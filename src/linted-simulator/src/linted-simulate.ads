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
with Linted.Angles;
with Linted.Controls_Reader;

package Linted.Simulate is
   pragma Elaborate_Body;

   type Large is range -2 ** (64 - 1) ..  2 ** (64 - 1) -1;

   type Nat is mod 2 ** 32;
   type Int is range -2 ** (32 - 1) ..  2 ** (32 - 1) - 1;

   package Sim_Angles is new Linted.Angles (Nat);

   subtype Sim_Angle is Sim_Angles.Angle;
   use type Sim_Angle;

   type Differentiable is record
      Value : Int;
      Old : Int;
   end record;

   type Position is (X, Y, Z);
   type Varying_Positions is array (Position) of Differentiable;

   type State is record
      Controls : Linted.Controls_Reader.Controls;

      Positions : Varying_Positions := (X => (Value => 0, Old => 0),
					Y => (Value => 0, Old => 0),
					Z => (Value => 0, Old => 0));

      Z_Rotation : Sim_Angle;
      X_Rotation : Sim_Angle;
   end record;

   procedure Tick (This : in out State);
end Linted.Simulate;