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

package body Linted.Update_Reader with
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
   use type Errors.Error;

   package Command_MVars is new MVars (KOs.KO);
   package Read_Done_Event_Channels is new Channels (Event);
   package Worker_Event_Channels is new Channels (Reader.Event);

   type Storage_Access is not null access all Storage_Elements.Storage_Element;

   function Convert is new Ada.Unchecked_Conversion
     (Storage_Access,
      System.Address);

   package body Worker is
      task Reader_Task;

      package Reader_Worker is new Reader.Worker;

      Data_Being_Read : aliased Update.Storage;
      My_Trigger : STC.Suspension_Object;
      My_Command_MVar : Command_MVars.MVar;
      Work_Event : Worker_Event_Channels.Channel;
      My_Event_Channel : Read_Done_Event_Channels.Channel;

      procedure Start (Object : KOs.KO) is
      begin
         Command_MVars.Set (My_Command_MVar, Object);
         STC.Set_True (My_Trigger);
      end Start;

      procedure Wait (E : out Event) is
      begin
         Read_Done_Event_Channels.Pop (My_Event_Channel, E);
      end Wait;

      task A;
      task body A is
         Worker_Event : Reader.Event;
      begin
         loop
            Reader_Worker.Wait (Worker_Event);
            Worker_Event_Channels.Push (Work_Event, Worker_Event);
            STC.Set_True (My_Trigger);
         end loop;
      end A;

      task body Reader_Task is
         Err : Errors.Error;
         U : Update.Packet;
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
                  if Err = Errors.Success then
                     Update.From_Storage (Data_Being_Read, U);
                  end if;
                  Read_Done_Event_Channels.Push (My_Event_Channel, (U, Err));
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

end Linted.Update_Reader;
