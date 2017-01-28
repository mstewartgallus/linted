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
with Interfaces.C;
with Interfaces;
with System.Storage_Elements;
with System;

with Linted.Reader;
with Linted.MVars;
with Linted.Triggers;

package body Linted.Update_Reader with
     Spark_Mode => Off is
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

   package body Worker is
      task Reader_Task;

      package My_Trigger is new Triggers.Handle;

      Data_Being_Read : aliased Update.Storage;
      My_Command_MVar : Command_MVars.MVar;

      procedure Start (Object : KOs.KO) is
      begin
         Command_MVars.Set (My_Command_MVar, Object);
         Triggers.Signal (My_Trigger.Signal_Handle);
      end Start;

      task body Reader_Task is
         Err : Errors.Error;
         U : Update.Packet;
         Object : KOs.KO;
         Object_Initialized : Boolean := False;
	 Read_Future_Has_Value : Boolean := False;
	 Read_Future : Reader.Future;
      begin
         loop
	    Triggers.Wait (My_Trigger.Wait_Handle);

            declare
               New_Command : Command_MVars.Option_Element_Ts.Option;
            begin
               Command_MVars.Poll (My_Command_MVar, New_Command);
               if not New_Command.Empty then
                  Object := New_Command.Data;
                  Object_Initialized := True;
               end if;
            end;

	    if Read_Future_Has_Value then
	       declare
		  Init : Boolean;
		  Event : Reader.Event;
	       begin
		  Reader.Read_Poll (Read_Future, Event, Init);
		  if Init then
		     Read_Future_Has_Value := False;
		     Err := Event.Err;
		     if Err = Errors.Success then
			Update.From_Storage (Data_Being_Read, U);
		     end if;

		     On_Event ((U, Err));
		  end if;
	       end;
	    end if;

            if not Read_Future_Has_Value and Object_Initialized then
               Reader.Read
                 (Object,
                  Data_Being_Read (1)'Address,
                  Data_Being_Read'Size / Interfaces.C.char'Size,
		  My_Trigger.Signal_Handle,
		  Read_Future);
	       Read_Future_Has_Value := True;
            end if;
         end loop;
      end Reader_Task;
   end Worker;

end Linted.Update_Reader;
