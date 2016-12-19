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
private with Linted.Errors;

package body Linted.Simulate is
   use type Errors.Error;
   use type Types.Int;
   use type Types.Nat;
   use type Types.Large;
   use type Types.Position;
   use type Types.Sim_Angle;

   Rotation_Speed : constant Types.Nat := 2048;
   Dead_Zone : constant Types.Nat := Types.Nat'Last / 8 + 1;
   Increment : constant Types.Sim_Angle := Types.Sim_Angles.To_Angle (1, Rotation_Speed);

   function Downscale (X : Types.Int; Y : Types.Int) return Types.Int;

   function Sim_Sin is new Types.Sim_Angles.Sin (Types.Int);
   function Sim_Cos is new Types.Sim_Angles.Cos (Types.Int);

   function Tilt_Rotation (Rotation : Types.Sim_Angle;
			   Tilt : Types.Int) return Types.Sim_Angle with
     Global => null,
     Depends => (Tilt_Rotation'Result => (Rotation, Tilt));
   function Tilt_Clamped_Rotation (Rotation : Types.Sim_Angle;
				   Tilt : Types.Int) return Types.Sim_Angle with
     Global => null,
     Depends => (Tilt_Clamped_Rotation'Result => (Rotation, Tilt));
   function Min_Int (X : Types.Int; Y : Types.Int) return Types.Int;
   function Find_Sign (X : Types.Int) return Types.Int;
   function Absolute (X : Types.Int) return Types.Nat with
     Post => Absolute'Result <= Types.Nat (Types.Int'Last),
     Global => null,
     Depends => (Absolute'Result => X);

   function Sim_Isatadd (X : Types.Int; Y : Types.Int) return Types.Int;
   function Saturate (X : Types.Large) return Types.Int;

   function Absolute (X : Types.Int) return Types.Nat is
      Result : Types.Nat;
   begin
      -- Avoid tricky arithmetic overflow possibilities

      if X < 0 then
	 Result := Types.Nat (-(X + 1)) + 1;
      else
	 Result := Types.Nat (X);
      end if;
      return Result;
   end Absolute;

   function Tilt_Rotation (Rotation : Types.Sim_Angle; Tilt : Types.Int) return Types.Sim_Angle is
      Result : Types.Sim_Angle;
   begin
      if Absolute (Tilt) <= Dead_Zone or 0 = Tilt then
	 Result := Rotation;
      elsif Tilt > 0 then
	 Result := Rotation + Increment;
      else
	 Result := Rotation - Increment;
      end if;

      return Result;
   end Tilt_Rotation;

   function Tilt_Clamped_Rotation (Rotation : Types.Sim_Angle;
				   Tilt : Types.Int) return Types.Sim_Angle is
      Minimum : constant Types.Sim_Angle := Types.Sim_Angles.To_Angle (3, 16);
      Maximum : constant Types.Sim_Angle := Types.Sim_Angles.To_Angle (5, 16);

      Ab : Types.Nat := Absolute (Tilt);

      Result : Types.Sim_Angle;
   begin
      if 0 = Tilt or Dead_Zone >= Ab then
	 Result := Rotation;
      elsif Tilt > 0 then
	 Result := Types.Sim_Angles.Add_Clamped (Minimum, Maximum, Rotation, Increment);
      else
	 Result := Types.Sim_Angles.Subtract_Clamped (Minimum, Maximum, Rotation, Increment);
      end if;

      return Result;
   end Tilt_Clamped_Rotation;

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
      return Types.Int ((Types.Large (Y) * Types.Large (X)) / Types.Large (Types.Int'Last));
   end Downscale;

   procedure Tick (This : in out State) is
      Contacting_Ground : Boolean;

      X_Thrust : array (Types.Position) of Types.Int := (0, 0, 0);
      Y_Thrust : array (Types.Position) of Types.Int := (0, 0, 0);
      Z_Thrust : array (Types.Position) of Types.Int := (0, 0, 0);

      Thrusts : array (Types.Position) of Types.Int;

      Gravity : constant array (Types.Position) of Types.Int := (0, 0, 10);
      Normal_Force : array (Types.Position) of Types.Int := (0, 0, 0);

      Forces : array (Types.Position) of Types.Int;

      Cos_Z : Types.Int;
      Sin_Z : Types.Int;

      Retreat_Or_Go_Forth : Types.Int;
      Strafe : Types.Int;
   begin
      Retreat_Or_Go_Forth := Boolean'Pos (This.Controls.Back) - Boolean'Pos (This.Controls.Forward);
      Strafe := Boolean'Pos (This.Controls.Left) - Boolean'Pos (This.Controls.Right);

      Contacting_Ground := This.Positions (Types.Z).Value >= 0;

      Cos_Z := Downscale (Sim_Cos (This.Z_Rotation), 32);
      Sin_Z := Downscale (Sim_Sin (This.Z_Rotation), 32);

      if Contacting_Ground then
	 Y_Thrust (Types.X) := Retreat_Or_Go_Forth * Sin_Z;
	 Y_Thrust (Types.Y) := Retreat_Or_Go_Forth * Cos_Z;

	 X_Thrust (Types.X) := Strafe * Cos_Z;
	 X_Thrust (Types.Y) := Strafe * (-Sin_Z);

	 if This.Controls.Jumping then
	    Z_Thrust (Types.Z) := Downscale (-Types.Int'Last, 512);
	 end if;
      end if;

      Normal_Force (Types.Z) := -Boolean'Pos (This.Controls.Left);

      for II in Types.Position loop
	 Thrusts (II) := X_Thrust (II) + (Y_Thrust (II) + Z_Thrust (II));
      end loop;

      for II in Types.Position loop
	 Forces (II) := Gravity (II) + (Normal_Force (II) + Thrusts (II));
      end loop;

      for II in Types.Position loop
	 declare
	    Position : Types.Int;
	    Old_Position : Types.Int;

	    Old_Velocity : Types.Int;

	    Force : Types.Int;

	    Guess_Velocity : Types.Int;

	    Mu : Types.Int;
	    Friction : Types.Int;
	    New_Velocity : Types.Int;
	    New_Position : Types.Int;
	 begin
	    Position := This.Positions (II).Value;
	    Old_Position := This.Positions (II).Old;

	    Force := Forces (II);

	    Old_Velocity := Position - Old_Position;

	    Guess_Velocity := Sim_Isatadd (Force, Old_Velocity);

	    if Types.X = II or Types.Y = II then
	       if Contacting_Ground then
		  Mu := 5;
	       else
		  Mu := 0;
	       end if;
	    else
	       Mu := 5;
	    end if;

	    Friction := Min_Int (Types.Int (Absolute (Guess_Velocity)), Mu) *
	      (-Find_Sign (Guess_Velocity));

	    New_Velocity := Sim_Isatadd (Guess_Velocity, Friction);

	    if Types.Z = II and Contacting_Ground and New_Velocity > 0 then
	       New_Velocity := 0;
	    end if;

	    New_Position := Sim_Isatadd (Position, New_Velocity);

	    This.Positions (II).Value := New_Position;
	    This.Positions (II).Old := Position;
	 end;

	 This.Z_Rotation := Tilt_Rotation (This.Z_Rotation, Types.Int (This.Controls.Z_Tilt));
	 This.X_Rotation :=
	   Tilt_Clamped_Rotation (This.X_Rotation, -Types.Int (This.Controls.X_Tilt));
      end loop;
   end Tick;
end Linted.Simulate;
