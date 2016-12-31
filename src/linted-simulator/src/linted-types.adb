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
package body Linted.Types is
   use type Sim_Angle;

   Rotation_Speed : constant Types.Nat := 2048;
   Dead_Zone : constant Types.Nat := Types.Nat'Last / 8 + 1;

   function Min_Int (X : Types.Int; Y : Types.Int) return Types.Int is
      Result : Types.Int;
   begin
      if X < Y then
         Result := X;
      else
         Result := Y;
      end if;
      return Result;
   end Min_Int;

   function Increment return Sim_Angle with
      Global => null,
      Depends => (Increment'Result => null);

   function Increment return Sim_Angle is
   begin
      return Sim_Angles.To_Angle (1, Rotation_Speed);
   end Increment;

   function Absolute (X : Types.Int) return Types.Nat is
      Inc : Types.Int;
      N : Types.Nat;
      Result : Types.Nat;
   begin
      -- Avoid tricky arithmetic overflow possibilities

      if X < 0 then
         Inc := X + 1;
         pragma Assert (Inc > Types.Int'First);
         N := Types.Nat (-Inc);
         pragma Assert (N <= Types.Nat (-(Types.Int'First + 1)));
         Result := N + 1;
         pragma Assert (Result <= Types.Nat (-(Types.Int'First + 1)) + 1);
         pragma Assert (Result <= Types.Nat (Types.Int'Last) + 1);
      else
         Result := Types.Nat (X);
         pragma Assert (Result <= Types.Nat (Types.Int'Last));
      end if;
      return Result;
   end Absolute;

   function Tilt_Rotation
     (Rotation : Types.Sim_Angle;
      Tilt : Types.Int) return Types.Sim_Angle
   is
      Result : Types.Sim_Angle;
   begin
      if Absolute (Tilt) <= Dead_Zone or 0 = Tilt then
         Result := Rotation;
      elsif Tilt > 0 then
         Result := Rotation + Types.Increment;
      else
         Result := Rotation - Types.Increment;
      end if;

      return Result;
   end Tilt_Rotation;

   function Tilt_Clamped_Rotation
     (Rotation : Types.Sim_Angle;
      Tilt : Types.Int) return Types.Sim_Angle
   is
      Minimum : constant Types.Sim_Angle := Types.Sim_Angles.To_Angle (3, 16);
      Maximum : constant Types.Sim_Angle := Types.Sim_Angles.To_Angle (5, 16);

      Ab : Types.Nat := Absolute (Tilt);

      Result : Types.Sim_Angle;
   begin
      if 0 = Tilt or Dead_Zone >= Ab then
         Result := Rotation;
      elsif Tilt > 0 then
         Result :=
           Types.Sim_Angles.Add_Clamped
             (Minimum,
              Maximum,
              Rotation,
              Types.Increment);
      else
         Result :=
           Types.Sim_Angles.Subtract_Clamped
             (Minimum,
              Maximum,
              Rotation,
              Types.Increment);
      end if;

      return Result;
   end Tilt_Clamped_Rotation;

   function Find_Sign (X : Types.Int) return Types.Int is
      Result : Types.Int;
   begin
      if X > 0 then
         Result := 1;
      elsif 0 = X then
         Result := 0;
      else
         Result := -1;
      end if;
      return Result;
   end Find_Sign;

   function Saturate (X : Types.Large) return Types.Int is
      Result : Types.Int;
   begin
      if X > Types.Large (Types.Int'Last) then
         Result := Types.Int'Last;
      elsif X < Types.Large (Types.Int'First) then
         Result := Types.Int'First;
      else
         Result := Types.Int (X);
      end if;
      return Result;
   end Saturate;

   function Sim_Isatadd (X : Types.Int; Y : Types.Int) return Types.Int is
   begin
      return Saturate (Types.Large (X) + Types.Large (Y));
   end Sim_Isatadd;

   function Downscale (X : Types.Int; Y : Types.Int) return Types.Int is
   begin
      return Types.Int
          ((Types.Large (Y) * Types.Large (X)) / Types.Large (Types.Int'Last));
   end Downscale;

end Linted.Types;
