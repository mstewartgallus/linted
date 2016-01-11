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
with Interfaces.C;
with System;

with Linted.Options;
with Linted.Triggers;

package Linted.IO_Pool is
   pragma Elaborate_Body;

   use type Interfaces.C.int;

   subtype Valid_Error is Interfaces.C.int range 0 .. Interfaces.C.int'Last;
   type Error is  new Valid_Error
     with Default_Value => 0;

   subtype Valid_KO is Interfaces.C.int range -1 .. Interfaces.C.int'Last;
   type KO is new Valid_KO
     with Default_Value => -1;

   Standard_Input : constant KO;
   Standard_Output : constant KO;
   Standard_Error : constant KO;

   generic
      type Element_T is private;
   package Results is
      type Result (Erroneous : Boolean) is record
	 case Erroneous is
	    when True => Err : Error := 0;
	    when False => Data : Element_T;
	 end case;
      end record;
   end Results;

   Success : constant Error;

   type Writer_Event is record
      Bytes_Written : Interfaces.C.size_t;
      Err : Error;
   end record;

   package Option_Writer_Events is new Linted.Options (Writer_Event);

   generic
      Event_Trigger : not null access Linted.Triggers.Trigger;
   package Writer_Worker is
      procedure Write (Object : KO; Buf : System.Address; Count : Interfaces.C.size_t);
      function Poll return Option_Writer_Events.Option;
   end Writer_Worker;

   type Reader_Event is record
      Bytes_Read : Interfaces.C.size_t;
      Err : Error;
   end record;

   package Option_Reader_Events is new Linted.Options (Reader_Event);

   generic
      Event_Trigger : not null access Linted.Triggers.Trigger;
   package Reader_Worker is
      procedure Read (Object : KO; Buf : System.Address; Count : Interfaces.C.size_t);
      function Poll return Option_Reader_Events.Option;
   end Reader_Worker;

   package KO_Results is new Results (KO);

   function Open (Pathname : String) return KO_Results.Result;
   function Close (Object : KO) return Error;

private
   Success : constant Error := 0;

   Invalid : constant KO := -1;

   Standard_Input : constant KO := 0;
   Standard_Output : constant KO := 1;
   Standard_Error : constant KO := 2;
end Linted.IO_Pool;
