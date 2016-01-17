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
with Linted_Unix;

private with Linted.MVars;

package body Linted.IO_Pool is
   package C renames Interfaces.C;

   package Unix renames Linted_Unix;

   type Write_Command is record
      Object : KOs.KO;
      Buf : System.Address;
      Count : C.size_t;
   end record;

   type Write_Done_Event is record
      Err : Errors.Error;
      Bytes_Written : C.size_t;
   end record;

   package Write_Command_MVars is new Linted.MVars (Write_Command);
   package Write_Done_Event_MVars is new Linted.MVars (Write_Done_Event);

   package body Writer_Worker is
      task Writer_Task;

      My_Trigger : Triggers.Trigger;
      My_Command_MVar : Write_Command_MVars.MVar;
      My_Event_MVar : Write_Done_Event_MVars.MVar;

      procedure Write (Object : KOs.KO; Buf : System.Address; Count : C.size_t) is
      begin
	 Write_Command_MVars.Set (My_Command_MVar, (Object, Buf, Count));
	 Triggers.Signal (My_Trigger);
      end Write;

      function Poll return Option_Writer_Events.Option is
	 Event : Write_Done_Event_MVars.Option_Element_Ts.Option;
      begin
	 Event := Write_Done_Event_MVars.Poll (My_Event_MVar);
	 if Event.Empty then
	    return (Empty => True);
	 else
	    return (False, (Event.Data.Bytes_Written, Event.Data.Err));
	 end if;
      end Poll;

      New_Write_Command : Write_Command_MVars.Option_Element_Ts.Option;
      Result : Unix.ssize_t;

      task body Writer_Task is
	 use type Unix.ssize_t;
	 use type C.size_t;
      begin
	 loop
	    Triggers.Wait (My_Trigger);

	    New_Write_Command := Write_Command_MVars.Poll (My_Command_MVar);
	    if not New_Write_Command.Empty then
	       declare
		  Err : C.int;
		  Bytes_Written : C.size_t := 0;

		  Object : constant KOs.KO := New_Write_Command.Data.Object;
		  Buf : constant System.Address := New_Write_Command.Data.Buf;
		  Count : constant C.size_t := New_Write_Command.Data.Count;
	       begin
		  loop
		     Result := Unix.Write (C.int (Object), Buf, Count - Bytes_Written);
		     if Result < 0 then
			Err := Unix.Errno;
			if Err /= Unix.EINTR then
			   exit;
			end if;
		     else
			Err := 0;
			Bytes_Written := Bytes_Written + C.size_t (Result);
			if Bytes_Written = Count then
			   exit;
			end if;
		     end if;
		  end loop;

		  Write_Done_Event_MVars.Set (My_Event_MVar, Write_Done_Event'(Err => Errors.Error (Err), Bytes_Written => Bytes_Written));
		  Triggers.Signal (Event_Trigger.all);
	       end;
	    end if;
	 end loop;
      end Writer_Task;
   end Writer_Worker;

   type Read_Command is record
      Object : KOs.KO;
      Buf : System.Address;
      Count : C.size_t;
   end record;

   type Read_Done_Event is record
      Err : Errors.Error;
      Bytes_Read : C.size_t;
   end record;

   package Read_Command_MVars is new Linted.MVars (Read_Command);
   package Read_Done_Event_MVars is new Linted.MVars (Read_Done_Event);

   package body Reader_Worker is
      task Reader_Task;

      My_Trigger : Triggers.Trigger;
      My_Command_MVar : Read_Command_MVars.MVar;
      My_Event_MVar : Read_Done_Event_MVars.MVar;

      procedure Read (Object : KOs.KO; Buf : System.Address; Count : C.size_t) is
      begin
	 Read_Command_MVars.Set (My_Command_MVar, (Object, Buf, Count));
	 Triggers.Signal (My_Trigger);
      end Read;

      function Poll return Option_Reader_Events.Option is
	 Event : Read_Done_Event_MVars.Option_Element_Ts.Option;
      begin
	 Event := Read_Done_Event_MVars.Poll (My_Event_MVar);
	 if Event.Empty then
	    return (Empty => True);
	 else
	    return (False, (Event.Data.Bytes_Read, Event.Data.Err));
	 end if;
      end Poll;

      New_Read_Command : Read_Command_MVars.Option_Element_Ts.Option;
      Result : Unix.ssize_t;

      task body Reader_Task is
	 use type Unix.ssize_t;
	 use type C.size_t;
      begin
	 loop
	    Triggers.Wait (My_Trigger);

	    New_Read_Command := Read_Command_MVars.Poll (My_Command_MVar);
	    if not New_Read_Command.Empty then
	       declare
		  Err : C.int;
		  Bytes_Read : C.size_t := 0;

		  Object : constant KOs.KO := New_Read_Command.Data.Object;
		  Buf : constant System.Address := New_Read_Command.Data.Buf;
		  Count : constant C.size_t := New_Read_Command.Data.Count;
	       begin
		  loop
		     Result := Unix.Read (C.int (Object), Buf, Count - Bytes_Read);
		     if Result < 0 then
			Err := Unix.Errno;
			if Err /= Unix.EINTR then
			   exit;
			end if;
		     elsif 0 = Result then
			Err := 0;
			exit;
		     else
			Err := 0;
			Bytes_Read := Bytes_Read + C.size_t (Result);
			if Bytes_Read = Count then
			   exit;
			end if;
		     end if;
		  end loop;

		  Read_Done_Event_MVars.Set (My_Event_MVar, (Err => Errors.Error (Err), Bytes_Read => Bytes_Read));
		  Triggers.Signal (Event_Trigger.all);
	       end;
	    end if;
	 end loop;
      end Reader_Task;
   end Reader_Worker;
end Linted.IO_Pool;
