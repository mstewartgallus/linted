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
private with Ada.Synchronous_Task_Control;
private with Ada.Unchecked_Conversion;
private with Interfaces.C;
private with Interfaces;
private with System.Storage_Elements;
private with System;

private with Linted.Reader;
private with Linted.MVars;

package body Linted.Controls_Reader is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   subtype Tuple is Storage_Elements.Storage_Array (1 .. 4);

   function From_Bytes (T : Tuple) return Controls_Int;

   type Read_Done_Event is record
      Err : Errors.Error;
      Data : Controls;
   end record;

   package Command_MVars is new Linted.MVars (KOs.KO);
   package Read_Done_Event_MVars is new Linted.MVars (Read_Done_Event);

   package body Worker with SPARK_Mode => Off is
      task Reader_Task;

      package Worker is new Linted.Reader.Worker;

      My_Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      Event_Trigger : Ada.Synchronous_Task_Control.Suspension_Object;

      Data_Being_Read : aliased Storage_Elements.Storage_Array (1 .. 2 * 4 + 1);

      My_Command_MVar : Command_MVars.MVar;
      My_Event_MVar : Read_Done_Event_MVars.MVar;

      procedure Start (Object : KOs.KO) is
      begin
	 My_Command_MVar.Set (Object);
	 Ada.Synchronous_Task_Control.Set_True (My_Trigger);
      end Start;

      function Poll return Option_Events.Option is
	 New_Event : Read_Done_Event_MVars.Option_Element_Ts.Option;
      begin
	 My_Event_MVar.Poll (New_Event);
	 if New_Event.Empty then
	    return (Empty => True);
	 else
	    return (False, (New_Event.Data.Data, New_Event.Data.Err));
	 end if;
      end Poll;

      procedure Wait is
      begin
	 Ada.Synchronous_Task_Control.Suspend_Until_True (Event_Trigger);
      end Wait;

      task A;
      task body A is
      begin
	 loop
	    Worker.Wait;
	    Ada.Synchronous_Task_Control.Set_True (My_Trigger);
	 end loop;
      end A;

      Err : Errors.Error;
      C : Controls;
      Object : KOs.KO;
      Object_Initialized : Boolean := False;

      task body Reader_Task is
	 type Storage_Access is not null access all Storage_Elements.Storage_Element;

	 function Convert is new Ada.Unchecked_Conversion (Storage_Access, System.Address);
	 use type Errors.Error;
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
	       Options_Event : constant Linted.Reader.Option_Events.Option := Worker.Poll;
	    begin
	       if not Options_Event.Empty then
		  Err := Options_Event.Data.Err;
		  if Err = Linted.Errors.Success then
		     C.Z_Tilt := From_Bytes (Data_Being_Read (1 .. 4));
		     C.X_Tilt := From_Bytes (Data_Being_Read (5 .. 8));

		     C.Left := (Interfaces.Unsigned_8 (Data_Being_Read (9)) and Interfaces.Shift_Left (1, 8 - 1)) /= 0;
		     C.Right := (Interfaces.Unsigned_8 (Data_Being_Read (9)) and Interfaces.Shift_Left (1, 8 - 2)) /= 0;
		     C.Forward := (Interfaces.Unsigned_8 (Data_Being_Read (9)) and Interfaces.Shift_Left (1, 8 - 3)) /= 0;
		     C.Back := (Interfaces.Unsigned_8 (Data_Being_Read (9)) and Interfaces.Shift_Left (1, 8 - 4)) /= 0;

		     C.Jumping := (Interfaces.Unsigned_8 (Data_Being_Read (9)) and Interfaces.Shift_Left (1, 8 - 5)) /= 0;
		  end if;
		  My_Event_MVar.Set ((Err, C));
		  Ada.Synchronous_Task_Control.Set_True (Event_Trigger);
	       end if;
	    end;

	    if Object_Initialized then
	       Worker.Read (Object, Convert (Data_Being_Read (1)'Access), Data_Being_Read'Size / Interfaces.C.char'Size);
	    end if;
	 end loop;
      end Reader_Task;
   end Worker;

   function From_Bytes (T : Tuple) return Controls_Int is
      X : Interfaces.Unsigned_32;
      Y : Controls_Int;
   begin
	 X := Interfaces.Unsigned_32 (T (4)) or
	   Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (3)), 8) or
	   Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (2)), 16) or
	   Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (1)), 24);
	 if X <= Interfaces.Unsigned_32 (Controls_Int'Last) then
	    Y := Controls_Int (X);
	 else
	    Y := -Controls_Int (not X + 1);
	 end if;
	 return Y;
   end From_Bytes;
end Linted.Controls_Reader;
