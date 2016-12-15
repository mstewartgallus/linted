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
private with Ada.Synchronous_Task_Control;

private with Linted.MVars;

package body Linted.Timer with SPARK_Mode => Off is
   package Real_Time renames Ada.Real_Time;

   type Command is record
      Time : Real_Time.Time := Real_Time.Time_First;
   end record;

   type Tick_Event is record
      null;
   end record;

   package Command_MVars is new Linted.MVars (Command);
   package Tick_Event_MVars is new Linted.MVars (Tick_Event);

   package body Worker is
      Event_Trigger : Ada.Synchronous_Task_Control.Suspension_Object;

      My_Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      My_Command_MVar : Command_MVars.MVar;
      My_Event_MVar : Tick_Event_MVars.MVar;

      task Timer_Task;

      procedure Wait_Until (Time : Real_Time.Time) is
      begin
	 My_Command_MVar.Set ((Time => Time));
	 Ada.Synchronous_Task_Control.Set_True (My_Trigger);
      end Wait_Until;

      procedure Wait is begin
	 Ada.Synchronous_Task_Control.Suspend_Until_True (Event_Trigger);
      end Wait;

      New_Command : Command_MVars.Option_Element_Ts.Option;

      task body Timer_Task is
	 use type Real_Time.Time;
      begin
	 loop
	    Ada.Synchronous_Task_Control.Suspend_Until_True (My_Trigger);

	    My_Command_MVar.Poll (New_Command);
	    if not New_Command.Empty then
	       declare
		  Time : constant Real_Time.Time := New_Command.Data.Time;
		  T : Tick_Event;
	       begin
		  delay until Time;
		  My_Event_MVar.Set (T);
		  Ada.Synchronous_Task_Control.Set_True (Event_Trigger);
	       end;
	    end if;
	 end loop;
      end Timer_Task;

      function Poll return Option_Events.Option is
	 T : Event;
	 New_Event : Tick_Event_MVars.Option_Element_Ts.Option;
      begin
	 My_Event_MVar.Poll (New_Event);
	 if New_Event.Empty then
	    return (Empty => True);
	 else
	    return (False, T);
	 end if;
      end Poll;
   end Worker;
end Linted.Timer;
