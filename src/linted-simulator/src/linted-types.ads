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

package Linted.Types is
   pragma Pure;

   type Large is range -2**(64 - 1) .. 2**(64 - 1) - 1;

   type Nat is mod 2**32;
   type Int is range -2**(32 - 1) .. 2**(32 - 1) - 1;
   type Fixed is delta 1.0 / 2.0**30 range -2.0**32 .. 2.0**32 + 1.0;

   package Sim_Angles is new Linted.Angles (Nat);

   subtype Sim_Angle is Sim_Angles.Angle;

   type Differentiable is record
      Value : Int;
      Old : Int;
   end record;

   type Position is (X, Y, Z);
   type Varying_Positions is array (Position) of Differentiable;

   type Object_State is array (0 .. 1) of Types.Varying_Positions;

   function Absolute (X : Types.Int) return Types.Nat with
      Post => Absolute'Result <= Types.Nat (Types.Int'Last) + 1,
      Global => null,
      Depends => (Absolute'Result => X);

   function Tilt_Rotation
     (Rotation : Types.Sim_Angle;
      Tilt : Types.Int) return Types.Sim_Angle with
      Global => null,
      Depends => (Tilt_Rotation'Result => (Rotation, Tilt));
   function Tilt_Clamped_Rotation
     (Rotation : Types.Sim_Angle;
      Tilt : Types.Int) return Types.Sim_Angle with
      Global => null,
      Depends => (Tilt_Clamped_Rotation'Result => (Rotation, Tilt));

   function Min_Int (X : Types.Int; Y : Types.Int) return Types.Int;

   function Find_Sign (X : Types.Int) return Types.Int;

   function Saturate (X : Types.Large) return Types.Int;

   function Sim_Isatadd (X : Types.Int; Y : Types.Int) return Types.Int;

   function Downscale (X : Types.Int; Y : Types.Int) return Types.Int;

   function Sim_Sin is new Types.Sim_Angles.Sin (Fixed);
   function Sim_Cos is new Types.Sim_Angles.Cos (Fixed);
end Linted.Types;
