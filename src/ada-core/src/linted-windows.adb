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

private with Libc;
private with Libc.Errno;

package body Linted.Windows is
   package Errno renames Libc.Errno;
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type C.int;
   use type C.size_t;
   use type Libc.ssize_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   type Storage_Access is not null access all Storage_Elements.Storage_Element;
   subtype Tuple is Storage_Elements.Storage_Array (1 .. 4);

   function From_Bytes (T : Tuple) return Window;

   procedure Read (KO : KOs.KO; Win : out Window; Err : out Errors.Error) is
      function Convert is new Ada.Unchecked_Conversion (Storage_Access, System.Address);
   begin
      declare
	 Buf : aliased Storage_Elements.Storage_Array (1 .. 4);
      begin
	 if Libc.PRead (C.int (KO), Convert (Buf (1)'Unchecked_Access), 4, 0) < 0 then
	    Win := 0;
	    Err := Errors.Error (Errno.Errno);
	    return;
	 end if;
	 Win := From_Bytes (Buf);
      end;
      Err := 0;
   end Read;

   function From_Bytes (T : Tuple) return Window is
      X : Interfaces.Unsigned_32;
   begin
      X := Interfaces.Unsigned_32 (T (4)) or
	Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (3)), 8) or
	Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (2)), 16) or
	Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (1)), 24);
      return Window (X);
   end From_Bytes;
end Linted.Windows;
