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

with Interfaces;
with Interfaces.C;

with System;
with System.Storage_Elements;

with Linted.Channels;
with Linted.Writer;

package body Linted.Update_Writer is
   package STC renames Ada.Synchronous_Task_Control;
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type Interfaces.Unsigned_32;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type Errors.Error;

   type Write_Command is record
      Object : KOs.KO;
      Data : Update.Packet;
   end record;

   type Write_Done_Event is record
      Err : Errors.Error;
   end record;

   package Write_Command_Channels is new Linted.Channels (Write_Command);
   package Write_Done_Event_Channels is new Linted.Channels (Write_Done_Event);
   package Worker_Event_Channels is new Linted.Channels (Linted.Writer.Event);

   type Storage_Access is not null access all Storage_Elements.Storage_Element;

   function Convert is new Ada.Unchecked_Conversion
     (Storage_Access,
      System.Address);

   package body Worker with
        Spark_Mode => Off is

      task A;
      task Writer_Task;

      My_Trigger : STC.Suspension_Object;

      package My_Worker is new Writer.Worker;

      Write_Command_Channel : Write_Command_Channels.Channel;
      Work_Event : Worker_Event_Channels.Channel;
      Write_Done_Event_Channel : Write_Done_Event_Channels.Channel;

      Data_Being_Written : aliased Update.Storage;

      task body A is
         Worker_Event : Linted.Writer.Event;
      begin
         loop
            My_Worker.Wait (Worker_Event);
            Worker_Event_Channels.Push (Work_Event, Worker_Event);
            STC.Set_True (My_Trigger);
         end loop;
      end A;

      procedure Write (Object : KOs.KO; Data : Update.Packet) is
      begin
         Write_Command_Channels.Push (Write_Command_Channel, (Object, Data));
         STC.Set_True (My_Trigger);
      end Write;

      procedure Wait (E : out Errors.Error) is
         Event : Write_Done_Event;
      begin
         Write_Done_Event_Channels.Pop (Write_Done_Event_Channel, Event);
         E := Event.Err;
      end Wait;

      task body Writer_Task is
         Err : Errors.Error;
         Pending_Update : Update.Packet;
         Object : KOs.KO;
         Update_Pending : Boolean := False;
         Update_In_Progress : Boolean := False;
      begin
         loop
            STC.Suspend_Until_True (My_Trigger);

            declare
               Option_Event : Worker_Event_Channels.Option_Element_Ts.Option;
            begin
               Worker_Event_Channels.Poll (Work_Event, Option_Event);
               if not Option_Event.Empty then
                  Err := Option_Event.Data.Err;

                  Update_In_Progress := False;

                  Write_Done_Event_Channels.Push
                    (Write_Done_Event_Channel,
                     Write_Done_Event'(Err => Err));
               end if;
            end;

            declare
               Option_Command : Write_Command_Channels.Option_Element_Ts
                 .Option;
            begin
               Write_Command_Channels.Poll
                 (Write_Command_Channel,
                  Option_Command);
               if not Option_Command.Empty then
                  Object := Option_Command.Data.Object;
                  Pending_Update := Option_Command.Data.Data;
                  Update_Pending := True;
               end if;
            end;

            if Update_Pending and not Update_In_Progress then
               Update.To_Storage (Pending_Update, Data_Being_Written);
               My_Worker.Write
                 (Object,
                  Convert (Data_Being_Written (1)'Access),
                  Data_Being_Written'Size / C.char'Size);
               Update_In_Progress := True;
               Update_Pending := False;
            end if;
         end loop;
      end Writer_Task;
   end Worker;
end Linted.Update_Writer;
