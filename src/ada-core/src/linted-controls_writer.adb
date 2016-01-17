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

private with Linted.MVars;
private with Linted.Writer;

package body Linted.Controls_Writer is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type Interfaces.Unsigned_32;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;

   type Controls_Nat is mod 2 ** 32;

   type Storage_Access is not null access all Storage_Elements.Storage_Element;
   subtype Tuple is Storage_Elements.Storage_Array (1 .. 4);

   function To_Bytes (Number : Controls_Nat) return Tuple;
   function To_Nat (Number : Controls_Int) return Controls_Nat;

   type Write_Command is record
      Object : KOs.KO;
      Data : Controls;
   end record;

   type Write_Done_Event is record
      Err : Errors.Error;
   end record;

   package Write_Command_MVars is new Linted.MVars (Write_Command);
   package Write_Done_Event_MVars is new Linted.MVars (Write_Done_Event);

   package body Worker is
      task Writer_Task;

      My_Trigger : aliased Triggers.Trigger;

      package Worker is new Linted.Writer.Worker (My_Trigger'Access);

      Write_Command_MVar : Write_Command_MVars.MVar;
      Write_Done_Event_MVar : Write_Done_Event_MVars.MVar;

      procedure Write (Object : KOs.KO; Data : Controls) is
      begin
	 Write_Command_MVars.Set (Write_Command_MVar, (Object, Data));
	 Triggers.Signal (My_Trigger);
      end Write;

      function Poll return Option_Events.Option is
	 Option_Event : Write_Done_Event_MVars.Option_Element_Ts.Option;
      begin
	 Option_Event := Write_Done_Event_MVars.Poll (Write_Done_Event_MVar);
	 if Option_Event.Empty then
	       return (Empty => True);
	 else
	       return (False, Option_Event.Data.Err);
	 end if;
      end Poll;

      Data_Being_Written : aliased Storage_Elements.Storage_Array (1 .. 2 * 4 + 1);

      Err : Errors.Error;
      Pending_Controls : Controls;
      Object : KOs.KO;
      Controls_Pending : Boolean := False;
      Controls_In_Progress : Boolean := False;

      task body Writer_Task is
	 use type Errors.Error;
	 use type Interfaces.Unsigned_8;

	 function Convert is new Ada.Unchecked_Conversion (Storage_Access, System.Address);
      begin
	 loop
	    Triggers.Wait (My_Trigger);

	    declare
	       Option_Event : constant Linted.Writer.Option_Events.Option := Worker.Poll;
	    begin
	       if not Option_Event.Empty then
		  Err := Option_Event.Data.Err;

		  Controls_In_Progress := False;

		  Write_Done_Event_MVars.Set (Write_Done_Event_MVar, Write_Done_Event'(Err => Err));
		  Triggers.Signal (Event_Trigger.all);
	       end if;
	    end;

	    declare
	       Option_Command : Write_Command_MVars.Option_Element_Ts.Option;
	    begin
	       Option_Command := Write_Command_MVars.Poll (Write_Command_MVar);
	       if not Option_Command.Empty then
		  Object := Option_Command.Data.Object;
		  Pending_Controls := Option_Command.Data.Data;
		  Controls_Pending := True;
	       end if;
	    end;

	    if Controls_Pending and not Controls_In_Progress then
	       Data_Being_Written (1 .. 4) := To_Bytes (To_Nat (Pending_Controls.Z_Tilt));
	       Data_Being_Written (5 .. 8) := To_Bytes (To_Nat (Pending_Controls.X_Tilt));

	       Data_Being_Written (9) := Storage_Elements.Storage_Element (
		 (if Pending_Controls.Left then Interfaces.Shift_Left (Interfaces.Unsigned_8 (1), 8 - 1) else 0) or
		   (if Pending_Controls.Right then Interfaces.Shift_Left (1, 8 - 2) else 0) or
		     (if Pending_Controls.Forward then Interfaces.Shift_Left (1, 8 - 3) else 0) or
		       (if Pending_Controls.Back then Interfaces.Shift_Left (1, 8 - 4) else 0) or
			 (if Pending_Controls.Jumping then Interfaces.Shift_Left (1, 8 - 5) else 0)
		 );

	       Worker.Write (Object, Convert (Data_Being_Written (1)'Access), Data_Being_Written'Size / C.char'Size);
	       Controls_In_Progress := True;
	       Controls_Pending := False;
	    end if;
	 end loop;
      end Writer_Task;
   end Worker;

   function To_Nat (Number : Controls_Int) return Controls_Nat is
   begin
      if Number < 0 then
	 return Controls_Nat (Number + Controls_Int'Last) - Controls_Nat (Controls_Int'Last);
      else
	 return Controls_Nat (Number);
      end if;
   end To_Nat;

   -- Big Endian
   function To_Bytes (Number : Controls_Nat) return Tuple is
      X : Storage_Elements.Storage_Array (1 .. 4);
   begin
      X (1) := Storage_Elements.Storage_Element (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 24) and 16#FF#);
      X (2) := Storage_Elements.Storage_Element (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 16) and 16#FF#);
      X (3) := Storage_Elements.Storage_Element (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 8) and 16#FF#);
      X (4) := Storage_Elements.Storage_Element (Number and 16#FF#);
      return X;
   end To_Bytes;
end Linted.Controls_Writer;
