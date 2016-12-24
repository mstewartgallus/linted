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

   subtype Tuple is Storage_Elements.Storage_Array (1 .. 4);

   function To_Bytes (Number : Update_Nat) return Tuple;
   function To_Nat (Number : Update_Int) return Update_Nat;

   type Write_Command is record
      Object : KOs.KO;
      Data : Update;
   end record;

   type Write_Done_Event is record
      Err : Errors.Error;
   end record;

   package Write_Command_Channels is new Linted.Channels (Write_Command);
   package Write_Done_Event_Channels is new Linted.Channels (Write_Done_Event);
   package Worker_Event_Channels is new Linted.Channels (Linted.Writer.Event);

   package body Worker with
        Spark_Mode => Off is
      use type Errors.Error;

      task A;
      task Writer_Task;

      type Storage_Access is
        not null access all Storage_Elements.Storage_Element;

      function Convert is new Ada.Unchecked_Conversion
        (Storage_Access,
         System.Address);

      My_Trigger : STC.Suspension_Object;

      package My_Worker is new Writer.Worker;

      Write_Command_Channel : Write_Command_Channels.Channel;
      Work_Event : Worker_Event_Channels.Channel;
      Write_Done_Event_Channel : Write_Done_Event_Channels.Channel;

      Data_Being_Written : aliased Storage_Elements.Storage_Array (1 .. 8 * 4);

      task body A is
         Worker_Event : Linted.Writer.Event;
      begin
         loop
            My_Worker.Wait (Worker_Event);
            Work_Event.Push (Worker_Event);
            STC.Set_True (My_Trigger);
         end loop;
      end A;

      procedure Write (Object : KOs.KO; Data : Update) is
      begin
         Write_Command_Channel.Push ((Object, Data));
         STC.Set_True (My_Trigger);
      end Write;

      procedure Wait (E : out Linted.Errors.Error) is
         Event : Write_Done_Event;
      begin
         Write_Done_Event_Channel.Pop (Event);
         E := Event.Err;
      end Wait;

      task body Writer_Task is
         Err : Errors.Error;
         Pending_Update : Update;
         Object : KOs.KO;
         Update_Pending : Boolean := False;
         Update_In_Progress : Boolean := False;
      begin
         loop
            STC.Suspend_Until_True (My_Trigger);

            declare
               Option_Event : Worker_Event_Channels.Option_Element_Ts.Option;
            begin
               Work_Event.Poll (Option_Event);
               if not Option_Event.Empty then
                  Err := Option_Event.Data.Err;

                  Update_In_Progress := False;

                  Write_Done_Event_Channel.Push
                    (Write_Done_Event'(Err => Err));
               end if;
            end;

            declare
               Option_Command : Write_Command_Channels.Option_Element_Ts
                 .Option;
            begin
               Write_Command_Channel.Poll (Option_Command);
               if not Option_Command.Empty then
                  Object := Option_Command.Data.Object;
                  Pending_Update := Option_Command.Data.Data;
                  Update_Pending := True;
               end if;
            end;

            if Update_Pending and not Update_In_Progress then
               Data_Being_Written (1 .. 4) :=
                 To_Bytes (To_Nat (Pending_Update.X_Position));
               Data_Being_Written (5 .. 8) :=
                 To_Bytes (To_Nat (Pending_Update.Y_Position));
               Data_Being_Written (9 .. 12) :=
                 To_Bytes (To_Nat (Pending_Update.Z_Position));

               Data_Being_Written (13 .. 16) :=
                 To_Bytes (To_Nat (Pending_Update.MX_Position));
               Data_Being_Written (17 .. 20) :=
                 To_Bytes (To_Nat (Pending_Update.MY_Position));
               Data_Being_Written (21 .. 24) :=
                 To_Bytes (To_Nat (Pending_Update.MZ_Position));

               Data_Being_Written (25 .. 28) :=
                 To_Bytes (Pending_Update.Z_Rotation);
               Data_Being_Written (29 .. 32) :=
                 To_Bytes (Pending_Update.X_Rotation);

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

   function To_Nat (Number : Update_Int) return Update_Nat is
      Y : Update_Nat;
   begin
      if Number < 0 then
         Y :=
           Update_Nat (Number + Update_Int'Last) -
           Update_Nat (Update_Int'Last);
      else
         Y := Update_Nat (Number);
      end if;
      return Y;
   end To_Nat;

   -- Big Endian
   function To_Bytes (Number : Update_Nat) return Tuple is
      X : Storage_Elements.Storage_Array (1 .. 4) := (0, 0, 0, 0);
   begin
      X (1) :=
        Storage_Elements.Storage_Element
          (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 24) and
           16#FF#);
      X (2) :=
        Storage_Elements.Storage_Element
          (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 16) and
           16#FF#);
      X (3) :=
        Storage_Elements.Storage_Element
          (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 8) and
           16#FF#);
      X (4) := Storage_Elements.Storage_Element (Number and 16#FF#);
      return X;
   end To_Bytes;
end Linted.Update_Writer;
