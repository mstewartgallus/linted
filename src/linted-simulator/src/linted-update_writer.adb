-- Copyright 2015,2016,2017 Steven Stewart-Gallus
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
with Ada.Text_IO;

with Interfaces;
with Interfaces.C;

with System;
with System.Storage_Elements;

with Linted.Channels;
with Linted.Writer;
with Linted.Triggers;

package body Linted.Update_Writer is
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

   package Write_Command_Channels is new Linted.Channels (Write_Command);

   package body Worker with
        Spark_Mode => Off is

      task Writer_Task;

      package My_Trigger is new Triggers.Handle;

      Write_Command_Channel : Write_Command_Channels.Channel;

      Data_Being_Written : aliased Update.Storage;

      procedure Write (Object : KOs.KO; Data : Update.Packet) is
      begin
         Write_Command_Channels.Push (Write_Command_Channel, (Object, Data));
         Triggers.Signal (My_Trigger.Signal_Handle);
      end Write;

      task body Writer_Task is
         Err : Errors.Error;
         Pending_Update : Update.Packet;
         Object : KOs.KO;
         Update_Pending : Boolean := False;
         Update_In_Progress : Boolean := False;
         Write_Future : Writer.Future;
         Write_Future_Live : Boolean := False;
      begin
         loop
            Triggers.Wait (My_Trigger.Wait_Handle);

            if Write_Future_Live then
               declare
                  Event : Writer.Event;
                  Init : Boolean;
               begin
                  Writer.Write_Poll (Write_Future, Event, Init);
                  if Init then
                     Write_Future_Live := False;
                     Err := Event.Err;

                     Update_In_Progress := False;

                     On_Event (Err);
                  end if;
               end;
            end if;

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

            if not Write_Future_Live and
              Update_Pending and
              not Update_In_Progress
            then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Write");
               Update.To_Storage (Pending_Update, Data_Being_Written);
               Writer.Write
                 (Object,
                  Data_Being_Written (1)'Address,
                  Data_Being_Written'Size / C.char'Size,
                  My_Trigger.Signal_Handle,
                  Write_Future);
               Update_In_Progress := True;
               Update_Pending := False;
               Write_Future_Live := True;
            end if;
         end loop;
      end Writer_Task;
   end Worker;
end Linted.Update_Writer;
