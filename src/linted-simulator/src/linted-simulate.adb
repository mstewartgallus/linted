-- Copyright 2015,2016 Steven Stewart-Gallus
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http ://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
private with Linted.Angles;
private with Linted.Controls_Reader;
private with Linted.Errors;

package body Linted.Simulate is
   use type Errors.Error;

   Rotation_Speed : constant Nat := 2048;
   Dead_Zone : constant Nat := Nat'Last / 8;

   function Downscale (X : Int; Y : Int) return Int;

   function Sim_Sin is new Sim_Angles.Sin (Int);
   function Sim_Cos is new Sim_Angles.Cos (Int);

   function Tilt_Rotation (Rotation : Sim_Angle; Tilt : Int) return Sim_Angle;
   function Tilt_Clamped_Rotation (Rotation : Sim_Angle;
				   Tilt : Int) return Sim_Angle;
   function Min_Int (X : Int; Y : Int) return Int;
   function Find_Sign (X : Int) return Int;
   function Absolute (X : Int) return Nat;

   function Sim_Isatadd (X : Int; Y : Int) return Int;
   function Saturate (X : Large) return Int;

   procedure Tick (This : in out State) is
      use type Int;

      Contacting_Ground : Boolean;

      X_Thrust : array (Position) of Int := (0, 0, 0);
      Y_Thrust : array (Position) of Int := (0, 0, 0);
      Z_Thrust : array (Position) of Int := (0, 0, 0);

      Thrusts : array (Position) of Int;

      Gravity : constant array (Position) of Int := (0, 0, 10);
      Normal_Force : array (Position) of Int := (0, 0, 0);

      Forces : array (Position) of Int;

      Cos_Z : Int;
      Sin_Z : Int;

      Retreat_Or_Go_Forth : Int;
      Strafe : Int;
   begin
      Retreat_Or_Go_Forth := Boolean'Pos (This.Controls.Back) - Boolean'Pos (This.Controls.Forward);
      Strafe := Boolean'Pos (This.Controls.Left) - Boolean'Pos (This.Controls.Right);

      Contacting_Ground := This.Positions (Z).Value >= 0;

      Cos_Z := Downscale (Sim_Cos (This.Z_Rotation), 32);
      Sin_Z := Downscale (Sim_Sin (This.Z_Rotation), 32);

      if Contacting_Ground then
	 Y_Thrust (X) := Retreat_Or_Go_Forth * Sin_Z;
	 Y_Thrust (Y) := Retreat_Or_Go_Forth * Cos_Z;

	 X_Thrust (X) := Strafe * Cos_Z;
	 X_Thrust (Y) := Strafe * (-Sin_Z);

	 if This.Controls.Jumping then
	    Z_Thrust (Z) := Downscale (-Int'Last, 512);
	 end if;
      end if;

      Normal_Force (Z) := -Boolean'Pos (This.Controls.Left);

      for II in Position loop
	 Thrusts (II) := X_Thrust (II) + (Y_Thrust (II) + Z_Thrust (II));
      end loop;

      for II in Position loop
	 Forces (II) := Gravity (II) + (Normal_Force (II) + Thrusts (II));
      end loop;

      for II in Position loop
	 declare
	    Position : Int;
	    Old_Position : Int;

	    Old_Velocity : Int;

	    Force : Int;

	    Guess_Velocity : Int;

	    Mu : Int;
	    Friction : Int;
	    New_Velocity : Int;
	    New_Position : Int;
	 begin
	    Position := This.Positions (II).Value;
	    Old_Position := This.Positions (II).Old;

	    Force := Forces (II);

	    Old_Velocity := Position - Old_Position;

	    Guess_Velocity := Sim_Isatadd (Force, Old_Velocity);

	    if X = II or Y = II then
	       if Contacting_Ground then
		  Mu := 5;
	       else
		  Mu := 0;
	       end if;
	    else
	       Mu := 5;
	    end if;

	    Friction := Min_Int (Int (Absolute (Guess_Velocity)), Mu) *
	      (-Find_Sign (Guess_Velocity));

	    New_Velocity := Sim_Isatadd (Guess_Velocity, Friction);

	    if Z = II and Contacting_Ground and New_Velocity > 0 then
	       New_Velocity := 0;
	    end if;

	    New_Position := Sim_Isatadd (Position, New_Velocity);

	    This.Positions (II).Value := New_Position;
	    This.Positions (II).Old := Position;
	 end;

	 This.Z_Rotation := Tilt_Rotation (This.Z_Rotation, Int (This.Controls.Z_Tilt));
	 This.X_Rotation :=
	   Tilt_Clamped_Rotation (This.X_Rotation, -Int (This.Controls.X_Tilt));
      end loop;
   end Tick;

   function Tilt_Rotation (Rotation : Sim_Angle; Tilt : Int) return Sim_Angle is
      Increment : Sim_Angle;
      Result : Sim_Angle;
   begin
      Increment := Sim_Angles.To_Angle (1, Rotation_Speed);

      if Absolute (Tilt) <= Dead_Zone then
	 Result := Rotation;
      else
	 if 0 = Tilt then
	    Result := Rotation;
	 elsif Tilt > 0 then
	    Result := Rotation + Increment;
	 else
	    Result := Rotation - Increment;
	 end if;
      end if;

      return Result;
   end Tilt_Rotation;

   function Tilt_Clamped_Rotation (Rotation : Sim_Angle;
				   Tilt : Int) return Sim_Angle is
      Minimum : Sim_Angle;
      Maximum : Sim_Angle;
      Increment : Sim_Angle;

      Result : Sim_Angle;
   begin
      Minimum := Sim_Angles.To_Angle (3, 16);
      Maximum := Sim_Angles.To_Angle (5, 16);
      Increment := Sim_Angles.To_Angle (1, Rotation_Speed);

      if Absolute (Tilt) <= Dead_Zone then
	 Result := Rotation;
      else
	 if 0 = Tilt then
	    Result := Rotation;
	 elsif Tilt > 0 then
	    Result := Sim_Angles.Add_Clamped (Minimum, Maximum, Rotation, Increment);
	 else
	    Result := Sim_Angles.Subtract_Clamped (Minimum, Maximum, Rotation, Increment);
	 end if;
      end if;

      return Result;
   end Tilt_Clamped_Rotation;

   function Min_Int (X : Int; Y : Int) return Int is
      Result : Int;
   begin
      if X < Y then
	 Result := X;
      else
	 Result := Y;
      end if;
      return Result;
   end Min_Int;

   function Find_Sign (X : Int) return Int is
      Result : Int;
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

   function Absolute (X : Int) return Nat is
      Result : Nat;
   begin
      -- Avoid tricky arithmetic overflow possibilities

      if Int'First = X then
	 Result := Nat (-(Int'First + 1)) + 1;
      elsif X < 0 then
	 Result := Nat (-X);
      else
	 Result := Nat (X);
      end if;
      return Result;
   end Absolute;

   function Sim_Isatadd (X : Int; Y : Int) return Int is
   begin
      return Saturate (Large (X) + Large (Y));
   end Sim_Isatadd;

   function Saturate (X : Large) return Int is
      Result : Int;
   begin
      if X > Large (Int'Last) then
	 Result := Int'Last;
      elsif X < Large (Int'First) then
	 Result := Int'First;
      else
	 Result := Int (X);
      end if;
      return Result;
   end Saturate;

   function Downscale (X : Int; Y : Int) return Int is
   begin
      return Int ((Large (Y) * Large (X)) / Large (Int'Last));
   end Downscale;
end Linted.Simulate;
