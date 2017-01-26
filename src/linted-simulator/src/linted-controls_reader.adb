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
with Ada.Synchronous_Task_Control;
with Ada.Unchecked_Conversion;

with Interfaces.C;
with Interfaces;

with System.Storage_Elements;
with System;

with Linted.Channels;
with Linted.Reader;
with Linted.MVars;

package body Linted.Controls_Reader with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type Controls.Int;
   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;
   use type Errors.Error;

   package Command_MVars is new Linted.MVars (KOs.KO);
   package Worker_Event_Channels is new Linted.Channels (Linted.Reader.Event);

   package body Worker is
      task Reader_Task;

      procedure On_Reader_Event (Worker_Event : Reader.Event);

      package My_Worker is new Reader.Worker (On_Reader_Event);

      My_Trigger : STC.Suspension_Object;

      Data_Being_Read : aliased Controls.Storage;

      My_Command_MVar : Command_MVars.MVar;
      Work_Event : Worker_Event_Channels.Channel;

      procedure Start (Object : KOs.KO) is
      begin
         Command_MVars.Set (My_Command_MVar, Object);
         STC.Set_True (My_Trigger);
      end Start;

      procedure On_Reader_Event (Worker_Event : Reader.Event) is
      begin
	 Worker_Event_Channels.Push (Work_Event, Worker_Event);
	 STC.Set_True (My_Trigger);
      end On_Reader_Event;

      task body Reader_Task is
         Err : Errors.Error;
         C : Controls.Packet;
         Object : KOs.KO;
         Object_Initialized : Boolean := False;
      begin
         loop
            STC.Suspend_Until_True (My_Trigger);

            declare
               New_Command : Command_MVars.Option_Element_Ts.Option;
            begin
               Command_MVars.Poll (My_Command_MVar, New_Command);
               if not New_Command.Empty then
                  Object := New_Command.Data;
                  Object_Initialized := True;
               end if;
            end;

            declare
               Options_Event : Worker_Event_Channels.Option_Element_Ts.Option;
            begin
               Worker_Event_Channels.Poll (Work_Event, Options_Event);
               if not Options_Event.Empty then
                  Err := Options_Event.Data.Err;
                  if Err = Linted.Errors.Success then
                     Controls.From_Storage (Data_Being_Read, C);
                  end if;
		  On_Event ((C, Err));
               end if;
            end;

            if Object_Initialized then
               My_Worker.Read
                 (Object,
                  Data_Being_Read (1)'Address,
                  Data_Being_Read'Size / Interfaces.C.char'Size);
            end if;
         end loop;
      end Reader_Task;
   end Worker;
end Linted.Controls_Reader;
