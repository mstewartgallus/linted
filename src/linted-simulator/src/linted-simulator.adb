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

private with Linted.Channels;
private with Linted.Controls_Reader;
private with Linted.Errors;
private with Linted.KOs;
private with Linted.Timer;
private with Linted.Update_Writer;
private with Linted.Simulate;
private with Linted.Types;

package body Linted.Simulator with
     Spark_Mode => Off is

   package Command_Line renames Ada.Command_Line;
   package Real_Time renames Ada.Real_Time;
   package STC renames Ada.Synchronous_Task_Control;

   use type Errors.Error;
   use type Types.Int;
   use type Real_Time.Time;

   type Tick_Event is record
      null;
   end record;

   package Control_Event_Channels is new Linted.Channels
     (Linted.Controls_Reader.Event);
   package Timer_Event_Channels is new Linted.Channels (Tick_Event);

   task A;
   task B;
   task Main_Task;

   package My_Controls_Reader is new Controls_Reader.Worker;
   package My_Update_Writer is new Update_Writer.Worker;
   package My_Timer is new Timer.Worker;

   Event_Trigger : STC.Suspension_Object;
   Control_Event_Channel : Control_Event_Channels.Channel;
   Timer_Event_Channel : Timer_Event_Channels.Channel;

   task body Main_Task is
      Controller_KO : KOs.KO;
      Updater_KO : KOs.KO;

      My_State : Simulate.State :=
        (Objects =>
           (0 => ((0, 0), (10 * 1024, 10 * 1024), (0, 0)),
            1 => ((0, 0), (0, 0), (-1000, -1000))),

         Z_Rotation => Types.Sim_Angles.To_Angle (0, 1),
         X_Rotation => Types.Sim_Angles.To_Angle (3, 16),

         Controls =>
           (Z_Tilt => 0,
            X_Tilt => 0,
            Back => False,
            Forward => False,
            Left => False,
            Right => False,
            Jumping => False),
         Counter => 750110405);
      Next_Time : Real_Time.Time;
   begin
      if Command_Line.Argument_Count < 2 then
         raise Constraint_Error with "At least two arguments";
      end if;

      declare
         Maybe_Controller_KO : constant KOs.KO_Results.Result :=
           KOs.Open (Command_Line.Argument (1), KOs.Read_Write);
      begin
         if Maybe_Controller_KO.Erroneous then
            raise Constraint_Error with "Erroneous controller path";
         end if;
         Controller_KO := Maybe_Controller_KO.Data;
      end;

      declare
         Maybe_Updater_KO : constant KOs.KO_Results.Result :=
           KOs.Open (Command_Line.Argument (2), KOs.Read_Write);
      begin
         if Maybe_Updater_KO.Erroneous then
            raise Constraint_Error with "Erroneous updater path";
         end if;
         Updater_KO := Maybe_Updater_KO.Data;
      end;

      My_Controls_Reader.Start (Controller_KO);

      Next_Time := Real_Time.Clock;

      My_Timer.Wait_Until (Next_Time);

      loop
         STC.Suspend_Until_True (Event_Trigger);

         declare
            Option_Event : Control_Event_Channels.Option_Element_Ts.Option;
         begin
            Control_Event_Channel.Poll (Option_Event);
            if not Option_Event.Empty then
               My_State.Controls := Option_Event.Data.Data;
            end if;
         end;

         declare
            Option_Event : Timer_Event_Channels.Option_Element_Ts.Option;
         begin
            Timer_Event_Channel.Poll (Option_Event);
            if not Option_Event.Empty then
               Simulate.Tick (My_State);
               My_Update_Writer.Write
                 (Updater_KO,
                  (X_Position =>
                     Update_Writer.Update_Int
                       (My_State.Objects (0) (Types.X).Value),
                   Y_Position =>
                     Update_Writer.Update_Int
                       (My_State.Objects (0) (Types.Y).Value),
                   Z_Position =>
                     Update_Writer.Update_Int
                       (My_State.Objects (0) (Types.Z).Value),

                   MX_Position =>
                     Update_Writer.Update_Int
                       (My_State.Objects (1) (Types.X).Value),
                   MY_Position =>
                     Update_Writer.Update_Int
                       (My_State.Objects (1) (Types.Y).Value),
                   MZ_Position =>
                     Update_Writer.Update_Int
                       (My_State.Objects (1) (Types.Z).Value),

                   Z_Rotation =>
                     Update_Writer.Update_Nat
                       (Types.Sim_Angles.From_Angle (My_State.Z_Rotation)),
                   X_Rotation =>
                     Update_Writer.Update_Nat
                       (Types.Sim_Angles.From_Angle (My_State.X_Rotation))));

               Next_Time :=
                 Next_Time + Real_Time.Nanoseconds ((1000000000 / 60) / 2);
               My_Timer.Wait_Until (Next_Time);
            end if;
         end;
      end loop;
   end Main_Task;

   task body A is
   begin
      loop
         declare
            Event : Controls_Reader.Event;
         begin
            My_Controls_Reader.Wait (Event);
            Control_Event_Channel.Push (Event);
            STC.Set_True (Event_Trigger);
         end;
      end loop;
   end A;

   task body B is
      T : Tick_Event;
   begin
      loop
         My_Timer.Wait;
         Timer_Event_Channel.Push (T);
         STC.Set_True (Event_Trigger);
      end loop;
   end B;
end Linted.Simulator;
