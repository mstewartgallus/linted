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
with Ada.Characters.Latin_1;
private with Ada.Unchecked_Conversion;
private with Interfaces.C.Strings;

private with Linted.Writer;

package body Linted.Stdio with SPARK_Mode => Off is
   package C renames Interfaces.C;

   use Linted.Errors;
   use Linted.KOs;

   procedure Write_Line (Object : KO; Str : String) is
      Dummy : Error;
   begin
      Write_String (Object, Str & Ada.Characters.Latin_1.LF, Dummy);
   end Write_Line;

   procedure Write_String (Object : KO; Str : String; Err : out Error) is
      function Convert is new Ada.Unchecked_Conversion (Interfaces.C.Strings.chars_ptr, System.Address);
      X : Interfaces.C.Strings.chars_ptr;
      Bytes_Written : C.size_t;
   begin
      X := Interfaces.C.Strings.New_String (Str);
      Write (Object, Convert (X), Interfaces.C.Strings.Strlen (X), Bytes_Written, Err);
      Interfaces.C.Strings.Free (X);
   end Write_String;

   package Writer is new Linted.Writer.Worker;

   procedure Write (Object : KO;
		   Buf : System.Address;
		   Count : C.size_t;
		   Bytes_Written : out C.size_t;
		   Err : out Error) is
   begin
      Writer.Write (Object, Buf, Count);

      declare
	 Event : Linted.Writer.Event;
      begin
	 Writer.Wait (Event);
	 Bytes_Written := Event.Bytes_Written;
	 Err := Event.Err;
      end;
   end Write;
end Linted.Stdio;
