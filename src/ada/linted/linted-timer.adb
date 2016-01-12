-- Copyright 2015 Steven Stewart-Gallus
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
private with Ada.Real_Time;

private with Linted.MVars;

package body Linted.Timer is
   package Real_Time renames Ada.Real_Time;

   type Tick_Event is record
      Overrun : Integer;
   end record;

   package Tick_Event_MVars is new Linted.MVars (Tick_Event);

   package body Worker is
      task Timer_Task;

      My_Event_MVar : Tick_Event_MVars.MVar;

      task body Timer_Task is
	 T : Real_Time.Time;
	 use type Real_Time.Time;

	 Was_Full : Boolean;

	 Overrun : Integer := 0;
      begin
	 T := Real_Time.Clock;
	 loop
	    My_Event_MVar.Set_And_Check (Tick_Event'(Overrun => Overrun), Was_Full);
	    Triggers.Signal (Event_Trigger.all);

	    if Was_Full then
	       Overrun := Overrun + 1;
	    else
	       Overrun := 0;
	    end if;

	    delay until T;
	    T := T + Real_Time.Nanoseconds ((1000000000 / 60) / 2);
	 end loop;
      end Timer_Task;

      function Poll return Option_Events.Option is
	 Event : Tick_Event_MVars.Option_Element_Ts.Option;
      begin
	 My_Event_MVar.Poll (Event);
	 if Event.Empty then
	    return (Empty => True);
	 else
	    return (False, (Overrun => Event.Data.Overrun));
	 end if;
      end Poll;
   end Worker;
end Linted.Timer;
