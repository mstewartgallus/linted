-- Copyright 2015 Steven Stewart-Gallus
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
private with Ada.Command_Line;
private with Ada.Real_Time;

private with Linted.Angles;
private with Linted.Controls_Reader;
private with Linted.Errors;
private with Linted.KOs;
private with Linted.Timer;
private with Linted.Triggers;
private with Linted.Update_Writer;

package body Linted.Simulator is
   package Command_Line renames Ada.Command_Line;
   package Real_Time renames Ada.Real_Time;

   Event_Trigger : aliased Triggers.Trigger;

   package Controls_Reader is new Linted.Controls_Reader.Worker (Event_Trigger'Access);
   package Update_Writer is new Linted.Update_Writer.Worker (Event_Trigger'Access);
   package Timer is new Linted.Timer.Worker (Event_Trigger'Access);

   use type Errors.Error;

   type Large is range -2 ** (64 - 1) ..  2 ** (64 - 1) -1;

   type Sim_Natural is mod 2 ** 32;
   type Sim_Integer is range -2 ** (32 - 1) ..  2 ** (32 - 1) - 1;

   Rotation_Speed : constant Sim_Natural := 2048;
   Dead_Zone : constant Sim_Natural := Sim_Natural'Last / 8;

   package Sim_Angles is new Linted.Angles (Sim_Natural);

   subtype Sim_Angle is Sim_Angles.Angle;
   use type Sim_Angle;

   type Differentiable is record
      Value : Sim_Integer;
      Old : Sim_Integer;
   end record;

   type Position is (X, Y, Z);
   type Varying_Positions is array (Position) of Differentiable;

   type State is record
      Controls : Linted.Controls_Reader.Controls;

      Positions : Varying_Positions;

      Z_Rotation : Sim_Angle;
      X_Rotation : Sim_Angle;
   end record;

   procedure Simulate_Tick (This : in out State);

   function Downscale (X : Sim_Integer; Y : Sim_Integer) return Sim_Integer;

   function Sim_Sin is new Sim_Angles.Sin (Sim_Integer);
   function Sim_Cos is new Sim_Angles.Cos (Sim_Integer);

   function Tilt_Rotation (Rotation : Sim_Angle; Tilt : Sim_Integer) return Sim_Angle;
   function Tilt_Clamped_Rotation (Rotation : Sim_Angle;
				   Tilt : Sim_Integer) return Sim_Angle;
   function Min_Int (X : Sim_Integer; Y : Sim_Integer) return Sim_Integer;
   function Find_Sign (X : Sim_Integer) return Sim_Integer;
   function Absolute (X : Sim_Integer) return Sim_Natural;

   function Sim_Isatadd (X : Sim_Integer; Y : Sim_Integer) return Sim_Integer;
   function Saturate (X : Large) return Sim_Integer;

   Controller_KO : KOs.KO;
   Updater_KO : KOs.KO;

   My_State : State := (Positions => ((0, 0),
				      (10 * 1024, 10 * 1024),
				      (0, 0)),

			Z_Rotation => Sim_Angles.To_Angle (0, 1),
			X_Rotation => Sim_Angles.To_Angle (3, 16),

			Controls => (Z_Tilt => 0, X_Tilt => 0,
				     Back => False,
				     Forward => False,
				     Left => False,
				     Right => False,
				     Jumping => False));
   Next_Time : Real_Time.Time;

   task Main_Task;
   task body Main_Task is
   begin
      if Command_Line.Argument_Count < 2 then
	 raise Constraint_Error with "At least two arguments";
      end if;

      declare
	 Maybe_Controller_KO : constant KOs.KO_Results.Result := KOs.Open (Command_Line.Argument (1), KOs.Read_Write);
      begin
	 if Maybe_Controller_KO.Erroneous then
	    raise Constraint_Error with "Erroneous controller path";
	 end if;
	 Controller_KO := Maybe_Controller_KO.Data;
      end;

      declare
	 Maybe_Updater_KO : constant KOs.KO_Results.Result := KOs.Open (Command_Line.Argument (2), KOs.Read_Write);
      begin
	 if Maybe_Updater_KO.Erroneous then
	    raise Constraint_Error with "Erroneous updater path";
	 end if;
	 Updater_KO := Maybe_Updater_KO.Data;
      end;

      Controls_Reader.Start (Controller_KO);

      Next_Time := Real_Time.Clock;

      Timer.Wait_Until (Next_Time);

      loop
	 Triggers.Wait (Event_Trigger);

	 declare
	    Option_Event : constant Linted.Controls_Reader.Option_Events.Option := Controls_Reader.Poll;
	 begin
	    if not Option_Event.Empty then
	       My_State.Controls := Option_Event.Data.Data;
	    end if;
	 end;

	 declare
	    use type Real_Time.Time;
	    Option_Event : constant Linted.Timer.Option_Events.Option := Timer.Poll;
	 begin
	    if not Option_Event.Empty then
	       Simulate_Tick (My_State);
	       Update_Writer.Write (Updater_KO, (X_Position => Linted.Update_Writer.Update_Int (My_State.Positions (X).Value),
						 Y_Position => Linted.Update_Writer.Update_Int (My_State.Positions (Y).Value),
						 Z_Position => Linted.Update_Writer.Update_Int (My_State.Positions (Z).Value),

						 Z_Rotation => Linted.Update_Writer.Update_Nat (Sim_Angles.From_Angle (My_State.Z_Rotation)),
						 X_Rotation => Linted.Update_Writer.Update_Nat (Sim_Angles.From_Angle (My_State.X_Rotation))));

	       Next_Time := Next_Time + Real_Time.Nanoseconds ((1000000000 / 60) / 2);
	       Timer.Wait_Until (Next_Time);
	    end if;
	 end;

	 declare
	    Option_Event : constant Linted.Update_Writer.Option_Events.Option := Update_Writer.Poll;
	 begin
	    if not Option_Event.Empty then
	       null;
	    end if;
	 end;
      end loop;
   end Main_Task;

   procedure Simulate_Tick (This : in out State) is
      use type Sim_Integer;

      Contacting_Ground : Boolean;

      X_Thrust : array (Position) of Sim_Integer := (0, 0, 0);
      Y_Thrust : array (Position) of Sim_Integer := (0, 0, 0);
      Z_Thrust : array (Position) of Sim_Integer := (0, 0, 0);

      Thrusts : array (Position) of Sim_Integer;

      Gravity : constant array (Position) of Sim_Integer := (0, 0, 10);
      Normal_Force : array (Position) of Sim_Integer := (0, 0, 0);

      Forces : array (Position) of Sim_Integer;

      Cos_Z : Sim_Integer;
      Sin_Z : Sim_Integer;

      Retreat_Or_Go_Forth : Sim_Integer;
      Strafe : Sim_Integer;
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
	    Z_Thrust (Z) := Downscale (-Sim_Integer'Last, 512);
	 end if;
      end if;

      Normal_Force (Z) := -Boolean'Pos (This.Controls.Left);

      for II in Position loop
	 Thrusts (II) := X_Thrust (II) + Y_Thrust (II) + Z_Thrust (II);
      end loop;

      for II in Position loop
	 Forces (II) := Gravity (II) + Normal_Force (II) + Thrusts (II);
      end loop;

      for II in Position loop
	 declare
	    Position : Sim_Integer;
	    Old_Position : Sim_Integer;

	    Old_Velocity : Sim_Integer;

	    Force : Sim_Integer;

	    Guess_Velocity : Sim_Integer;

	    Mu : Sim_Integer;
	    Friction : Sim_Integer;
	    New_Velocity : Sim_Integer;
	    New_Position : Sim_Integer;
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

	    Friction := Min_Int (Sim_Integer (Absolute (Guess_Velocity)), Mu) *
	      (-Find_Sign (Guess_Velocity));

	    New_Velocity := Sim_Isatadd (Guess_Velocity, Friction);

	    if Z = II and Contacting_Ground and New_Velocity > 0 then
	       New_Velocity := 0;
	    end if;

	    New_Position := Sim_Isatadd (Position, New_Velocity);

	    This.Positions (II).Value := New_Position;
	    This.Positions (II).Old := Position;
	 end;

	 This.Z_Rotation := Tilt_Rotation (This.Z_Rotation, Sim_Integer (This.Controls.Z_Tilt));
	 This.X_Rotation :=
	   Tilt_Clamped_Rotation (This.X_Rotation, -Sim_Integer (This.Controls.X_Tilt));
      end loop;
   end Simulate_Tick;

   function Tilt_Rotation (Rotation : Sim_Angle; Tilt : Sim_Integer) return Sim_Angle is
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
				   Tilt : Sim_Integer) return Sim_Angle is
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

   function Min_Int (X : Sim_Integer; Y : Sim_Integer) return Sim_Integer is
      Result : Sim_Integer;
   begin
      if X < Y then
	 Result := X;
      else
	 Result := Y;
      end if;
      return Result;
   end Min_Int;

   function Find_Sign (X : Sim_Integer) return Sim_Integer is
      Result : Sim_Integer;
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

   function Absolute (X : Sim_Integer) return Sim_Natural is
      Result : Sim_Natural;
   begin
      -- Avoid tricky arithmetic overflow possibilities

      if Sim_Integer'First = X then
	 Result := Sim_Natural (-(Sim_Integer'First + 1)) + 1;
      elsif X < 0 then
	 Result := Sim_Natural (-X);
      else
	 Result := Sim_Natural (X);
      end if;
      return Result;
   end Absolute;

   function Sim_Isatadd (X : Sim_Integer; Y : Sim_Integer) return Sim_Integer is
   begin
      return Saturate (Large (X) + Large (Y));
   end Sim_Isatadd;

   function Saturate (X : Large) return Sim_Integer is
      Result : Sim_Integer;
   begin
      if X > Large (Sim_Integer'Last) then
	 Result := Sim_Integer'Last;
      elsif X < Large (Sim_Integer'First) then
	 Result := Sim_Integer'First;
      else
	 Result := Sim_Integer (X);
      end if;
      return Result;
   end Saturate;

   function Downscale (X : Sim_Integer; Y : Sim_Integer) return Sim_Integer is
   begin
      return Sim_Integer ((Large (Y) * Large (X)) / Large (Sim_Integer'Last));
   end Downscale;
end Linted.Simulator;
