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
private with Ada.Command_Line;
private with Ada.Real_Time;
private with Ada.Synchronous_Task_Control;

private with Linted.Controls_Reader;
private with Linted.Errors;
private with Linted.KOs;
private with Linted.Timer;
private with Linted.Update_Writer;
private with Linted.Simulate;

package body Linted.Simulator with Spark_MODE => Off is

   package Command_Line renames Ada.Command_Line;
   package Real_Time renames Ada.Real_Time;
   package Simulate renames Linted.Simulate;

   Event_Trigger : aliased Ada.Synchronous_Task_Control.Suspension_Object;

   procedure Signal is
   begin
      null;
   end Signal;

   package Controls_Reader is new Linted.Controls_Reader.Worker;
   package Update_Writer is new Linted.Update_Writer.Worker;
   package Timer is new Linted.Timer.Worker;

   use type Errors.Error;
   use type Simulate.Int;

   Controller_KO : KOs.KO;
   Updater_KO : KOs.KO;

   My_State : Simulate.State := (Positions => ((0, 0),
				      (10 * 1024, 10 * 1024),
				      (0, 0)),

			Z_Rotation => Simulate.Sim_Angles.To_Angle (0, 1),
			X_Rotation => Simulate.Sim_Angles.To_Angle (3, 16),

			Controls => (Z_Tilt => 0, X_Tilt => 0,
				     Back => False,
				     Forward => False,
				     Left => False,
				     Right => False,
				     Jumping => False));
   Next_Time : Real_Time.Time;

   task A;
   task body A is
   begin
      loop
	 Controls_Reader.Wait;
	 Ada.Synchronous_Task_Control.Set_True (Event_Trigger);
      end loop;
   end A;

   task B;
   task body B is
   begin
      loop
	 Timer.Wait;
	 Ada.Synchronous_Task_Control.Set_True (Event_Trigger);
      end loop;
   end B;

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
	 Ada.Synchronous_Task_Control.Suspend_Until_True (Event_Trigger);

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
	       Simulate.Tick (My_State);
	       Update_Writer.Write (Updater_KO, (X_Position => Linted.Update_Writer.Update_Int (My_State.Positions (Simulate.X).Value),
						 Y_Position => Linted.Update_Writer.Update_Int (My_State.Positions (Simulate.Y).Value),
						 Z_Position => Linted.Update_Writer.Update_Int (My_State.Positions (Simulate.Z).Value),

						 Z_Rotation => Linted.Update_Writer.Update_Nat (Simulate.Sim_Angles.From_Angle (My_State.Z_Rotation)),
						 X_Rotation => Linted.Update_Writer.Update_Nat (Simulate.Sim_Angles.From_Angle (My_State.X_Rotation))));

	       Next_Time := Next_Time + Real_Time.Nanoseconds ((1000000000 / 60) / 2);
	       Timer.Wait_Until (Next_Time);
	    end if;
	 end;
      end loop;
   end Main_Task;
end Linted.Simulator;
