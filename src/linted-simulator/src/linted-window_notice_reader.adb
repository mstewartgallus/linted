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
private with Ada.Unchecked_Conversion;
private with Interfaces.C;
private with Interfaces;
private with System.Storage_Elements;
private with System;

private with Linted.Reader;
private with Linted.MVars;

package body Linted.Window_Notice_Reader is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   type Storage_Access is not null access all Storage_Elements.Storage_Element;

   type Read_Done_Event is record
      Err : Errors.Error;
   end record;

   package Command_MVars is new Linted.MVars (KOs.KO);
   package Read_Done_Event_MVars is new Linted.MVars (Read_Done_Event);

   package body Worker is
      task Reader_Task;

      My_Trigger : aliased Triggers.Trigger;

      package Worker is new Linted.Reader.Worker (My_Trigger'Access);

      Data_Being_Read : aliased Storage_Elements.Storage_Array (1 .. 1);

      My_Command_MVar : Command_MVars.MVar;
      My_Event_MVar : Read_Done_Event_MVars.MVar;

      procedure Start (Object : KOs.KO) is
      begin
	 Command_MVars.Set (My_Command_MVar, Object);
	 Triggers.Signal (My_Trigger);
      end Start;

      function Poll return Option_Events.Option is
	 New_Event : Read_Done_Event_MVars.Option_Element_Ts.Option;
      begin
	 New_Event := Read_Done_Event_MVars.Poll (My_Event_MVar);
	 if New_Event.Empty then
	    return (Empty => True);
	 else
	    return (False, New_Event.Data.Err);
	 end if;
      end Poll;

      Object : KOs.KO;
      Object_Initialized : Boolean := False;

      task body Reader_Task is
	 function Convert is new Ada.Unchecked_Conversion (Storage_Access, System.Address);
	 use type Errors.Error;
      begin
	 loop
	    Triggers.Wait (My_Trigger);

	    declare
	       New_Command : Command_MVars.Option_Element_Ts.Option;
	    begin
	       New_Command := Command_MVars.Poll (My_Command_MVar);
	       if not New_Command.Empty then
		  Object := New_Command.Data;
		  Object_Initialized := True;
	       end if;
	    end;

	    declare
	       Options_Event : constant Linted.Reader.Option_Events.Option := Worker.Poll;
	    begin
	       if not Options_Event.Empty then
		  Read_Done_Event_MVars.Set (My_Event_MVar, (Err => Options_Event.Data.Err));
		  Triggers.Signal (Event_Trigger.all);
	       end if;
	    end;

	    if Object_Initialized then
	       Worker.Read (Object, Convert (Data_Being_Read (1)'Access), Data_Being_Read'Size / Interfaces.C.char'Size);
	    end if;
	 end loop;
      end Reader_Task;
   end Worker;
end Linted.Window_Notice_Reader;
