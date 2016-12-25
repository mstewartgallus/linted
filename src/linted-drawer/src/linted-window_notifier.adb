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
with Ada.Synchronous_Task_Control;
with Ada.Unchecked_Conversion;

with Interfaces.C;
with Interfaces;

with System.Storage_Elements;
with System;

with Linted.Channels;
with Linted.Errors;
with Linted.Reader;
with Linted.Logs;
with Linted.MVars;

package body Linted.Window_Notifier with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   type Event is record
      Err : Errors.Error;
   end record;

   package Command_MVars is new MVars (KOs.KO);
   package Read_Done_Event_Channels is new Channels (Event);
   package Worker_Event_Channels is new Channels (Reader.Event);

   package body Worker is
      type Storage_Access is
        not null access all Storage_Elements.Storage_Element;

      package Reader_Worker is new Reader.Worker;
      function Convert is new Ada.Unchecked_Conversion
        (Storage_Access,
         System.Address);

      use type Errors.Error;

      task Reader_Task;

      My_Trigger : STC.Suspension_Object;

      My_Command_MVar : Command_MVars.MVar;
      Work_Event : Worker_Event_Channels.Channel;
      My_Event_Channel : Read_Done_Event_Channels.Channel;

      Data_Being_Read : aliased Storage_Elements.Storage_Array (1 .. 1);

      procedure Start (Object : KOs.KO) is
      begin
         My_Command_MVar.Set (Object);
         STC.Set_True (My_Trigger);
      end Start;

      procedure Wait is
         E : Event;
      begin
         My_Event_Channel.Pop (E);
      end Wait;

      task A;
      task body A is
         Worker_Event : Reader.Event;
      begin
         loop
            Reader_Worker.Wait (Worker_Event);
            Work_Event.Push (Worker_Event);
            STC.Set_True (My_Trigger);
         end loop;
      end A;

      task body Reader_Task is
         Err : Errors.Error;
         Object : KOs.KO;
         Object_Initialized : Boolean := False;
      begin
         loop
	    Logs.Log (Logs.Info, "Suspend");
            STC.Suspend_Until_True (My_Trigger);
	    Logs.Log (Logs.Info, "Unsuspend");

            declare
               New_Command : Command_MVars.Option_Element_Ts.Option;
            begin
               My_Command_MVar.Poll (New_Command);
               if not New_Command.Empty then
                  Object := New_Command.Data;
                  Object_Initialized := True;
               end if;
            end;

            declare
               Options_Event : Worker_Event_Channels.Option_Element_Ts.Option;
            begin
               Work_Event.Poll (Options_Event);
               if not Options_Event.Empty then
                  Err := Options_Event.Data.Err;
                  My_Event_Channel.Push (Event'(Err => Err));
               end if;
            end;

            if Object_Initialized then
               Reader_Worker.Read
                 (Object,
                  Convert (Data_Being_Read (1)'Access),
                  Data_Being_Read'Size / Interfaces.C.char'Size);
            end if;
         end loop;
      end Reader_Task;
   end Worker;
end Linted.Window_Notifier;
