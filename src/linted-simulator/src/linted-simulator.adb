-- Copyright 2015,2016,2017 Steven Stewart-Gallus
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

private with Linted.Channels;
private with Linted.Controls_Reader;
private with Linted.Errors;
private with Linted.KOs;
private with Linted.Timer;
private with Linted.Update_Writer;
private with Linted.Update;
private with Linted.Simulate;
private with Linted.Triggers;
private with Linted.Types;

package body Linted.Simulator with
     Spark_Mode => Off is

   package Command_Line renames Ada.Command_Line;
   package Real_Time renames Ada.Real_Time;

   use type Errors.Error;
   use type Types.Int;
   use type Real_Time.Time;

   type Tick_Event is record
      null;
   end record;

   package Timer_Event_Channels is new Linted.Channels (Tick_Event);

   procedure On_Tick;
   task Main_Task;

   package My_Timer is new Timer.Worker (On_Tick);

   package My_Trigger is new Triggers.Handle;
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

      Read_Future : Controls_Reader.Future;
      Read_Future_Live : Boolean := False;

      Write_Future : Update_Writer.Future;
      Write_Future_Live : Boolean := False;

      Update_Pending : Boolean := False;
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

      Controls_Reader.Read
        (Controller_KO,
         My_Trigger.Signal_Handle,
         Read_Future);
      Read_Future_Live := True;

      Next_Time := Real_Time.Clock;

      My_Timer.Wait_Until (Next_Time);

      loop
         Triggers.Wait (My_Trigger.Wait_Handle);

         if Read_Future_Live then
            declare
               Event : Controls_Reader.Event;
               Init : Boolean;
            begin
               Controls_Reader.Read_Poll (Read_Future, Event, Init);
               if Init then
                  Read_Future_Live := False;

                  My_State.Controls := Event.Data;

                  Controls_Reader.Read
                    (Controller_KO,
                     My_Trigger.Signal_Handle,
                     Read_Future);
                  Read_Future_Live := True;
               end if;
            end;
         end if;

         if Write_Future_Live then
            declare
               Err : Errors.Error;
               Init : Boolean;
            begin
               Update_Writer.Write_Poll (Write_Future, Err, Init);
               if Init then
                  Write_Future_Live := False;
               end if;
            end;
         end if;

         declare
            Event : Tick_Event;
            Success : Boolean;
         begin
            Timer_Event_Channels.Poll (Timer_Event_Channel, Event, Success);
            if Success then
               Simulate.Tick (My_State);
               Update_Pending := True;
               Next_Time :=
                 Next_Time + Real_Time.Nanoseconds ((1000000000 / 60) / 2);
               My_Timer.Wait_Until (Next_Time);
            end if;
         end;

         if Update_Pending and not Write_Future_Live then
            Update_Writer.Write
              (Updater_KO,
               (X_Position =>
                  Update.Int (My_State.Objects (0) (Types.X).Value),
                Y_Position =>
                  Update.Int (My_State.Objects (0) (Types.Y).Value),
                Z_Position =>
                  Update.Int (My_State.Objects (0) (Types.Z).Value),

                MX_Position =>
                  Update.Int (My_State.Objects (1) (Types.X).Value),
                MY_Position =>
                  Update.Int (My_State.Objects (1) (Types.Y).Value),
                MZ_Position =>
                  Update.Int (My_State.Objects (1) (Types.Z).Value),

                Z_Rotation =>
                  Update.Nat
                    (Types.Sim_Angles.From_Angle (My_State.Z_Rotation)),
                X_Rotation =>
                  Update.Nat
                    (Types.Sim_Angles.From_Angle (My_State.X_Rotation))),
               My_Trigger.Signal_Handle,
               Write_Future);
            Write_Future_Live := True;
            Update_Pending := False;
         end if;
      end loop;
   end Main_Task;

   procedure On_Tick is
      T : Tick_Event;
   begin
      Timer_Event_Channels.Push (Timer_Event_Channel, T);
      Triggers.Signal (My_Trigger.Signal_Handle);
   end On_Tick;
end Linted.Simulator;
