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
private with Ada.Synchronous_Task_Control;
private with Ada.Unchecked_Conversion;
private with Interfaces.C;
private with Interfaces;
private with System.Storage_Elements;
private with System;

private with Linted.Channels;
private with Linted.Errors;
private with Linted.Reader;
private with Linted.MVars;

package body Linted.Window_Notifier with
     Spark_Mode => Off is
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

   package Command_MVars is new Linted.MVars (KOs.KO);
   package Read_Done_Event_Channels is new Linted.Channels (Event);
   package Worker_Event_Channels is new Linted.Channels (Linted.Reader.Event);

   package body Worker is
      task Reader_Task;

      package Worker is new Linted.Reader.Worker;

      My_Trigger : Ada.Synchronous_Task_Control.Suspension_Object;

      Data_Being_Read : aliased Storage_Elements.Storage_Array (1 .. 1);

      My_Command_MVar : Command_MVars.MVar;
      Work_Event : Worker_Event_Channels.Channel;
      My_Event_Channel : Read_Done_Event_Channels.Channel;

      procedure Start (Object : KOs.KO) is
      begin
         My_Command_MVar.Set (Object);
         Ada.Synchronous_Task_Control.Set_True (My_Trigger);
      end Start;

      procedure Wait is
         E : Event;
      begin
         My_Event_Channel.Pop (E);
      end Wait;

      task A;
      task body A is
         Worker_Event : Linted.Reader.Event;
      begin
         loop
            Worker.Wait (Worker_Event);
            Work_Event.Push (Worker_Event);
            Ada.Synchronous_Task_Control.Set_True (My_Trigger);
         end loop;
      end A;

      task body Reader_Task is
         type Storage_Access is
           not null access all Storage_Elements.Storage_Element;

         function Convert is new Ada.Unchecked_Conversion
           (Storage_Access,
            System.Address);
         use type Errors.Error;

         Err : Errors.Error;
         Object : KOs.KO;
         Object_Initialized : Boolean := False;
      begin
         loop
            Ada.Synchronous_Task_Control.Suspend_Until_True (My_Trigger);

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
               Worker.Read
                 (Object,
                  Convert (Data_Being_Read (1)'Access),
                  Data_Being_Read'Size / Interfaces.C.char'Size);
            end if;
         end loop;
      end Reader_Task;
   end Worker;
end Linted.Window_Notifier;
