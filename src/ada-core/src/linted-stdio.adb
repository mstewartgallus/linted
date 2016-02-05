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
private with Interfaces.C.Strings;

private with Linted.Libc;
private with Linted.Writer;
private with Linted.Triggers;

package body Linted.Stdio is
   package C renames Interfaces.C;
   use Linted.Triggers;

   procedure Write_String (Object : KO; Str : String; Err : out Error) is
      function Convert is new Ada.Unchecked_Conversion (Interfaces.C.Strings.chars_ptr, System.Address);
      X : Interfaces.C.Strings.chars_ptr;
      Bytes_Written : C.size_t;
   begin
      X := Interfaces.C.Strings.New_String (Str);
      Write (Object, Convert (X), Libc.Strlen (Interfaces.C.Strings.Value (X)), Bytes_Written, Err);
      Interfaces.C.Strings.Free (X);
   end Write_String;

   Event_Trigger : aliased Trigger;

   package Writer is new Linted.Writer.Worker (Event_Trigger'Access);

   procedure Write (Object : KO;
		   Buf : System.Address;
		   Count : C.size_t;
		   Bytes_Written : out C.size_t;
		   Err : out Error) is
   begin
      Writer.Write (Object, Buf, Count);

      loop
	 Event_Trigger.Wait;

	 declare
	    Option_Event : constant Linted.Writer.Option_Events.Option := Writer.Poll;
	 begin
	    if not Option_Event.Empty then
	       Bytes_Written := Option_Event.Data.Bytes_Written;
	       Err := Option_Event.Data.Err;
	       exit;
	    end if;
	 end;
      end loop;
   end Write;
end Linted.Stdio;
