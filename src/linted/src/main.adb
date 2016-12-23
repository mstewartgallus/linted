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
private with Ada.Command_Line;
private with Ada.Strings.Unbounded;
private with Ada.Unchecked_Conversion;

private with Interfaces.C;
private with Interfaces.C.Strings;

with System;

private with Libc.Unistd;

private with Linted.Env;
private with Linted.Errors;
private with Linted.KOs;
private with Linted.Process;
private with Linted.Stdio;
private with Linted.Settings;

private with Linted.Last_Chance;

procedure Main is
   package Command_Line renames Ada.Command_Line;
   package U renames Ada.Strings.Unbounded;

   use type Linted.Errors.Error;

   type Envvar is record
      Key : U.Unbounded_String;
      Value : U.Unbounded_String;
   end record;

   Envvars_Count : constant Positive := 18;

   function Default_Envvars (X : Positive) return Envvar with
     Pre => X <= Envvars_Count;

   function Default_Envvars (X : Positive) return Envvar is (case X is
      when 1 => (U.To_Unbounded_String ("LINTED_PROCESS_NAME"), U.To_Unbounded_String ("linted")),
      when 2 => (U.To_Unbounded_String ("LINTED_SYSTEM_CONF_PATH"), U.To_Unbounded_String (Linted.Settings.LNTD_SYSTEM_CONF_PATH)),
      when 3 => (U.To_Unbounded_String ("LINTED_UNIT_PATH"), U.To_Unbounded_String (Linted.Settings.LNTD_UNIT_PATH)),
      when 4 => (U.To_Unbounded_String ("LINTED_INIT"), U.To_Unbounded_String (Linted.Settings.LNTD_INIT)),
      when 5 => (U.To_Unbounded_String ("LINTED_MONITOR"), U.To_Unbounded_String (Linted.Settings.LNTD_MONITOR)),
      when 6 => (U.To_Unbounded_String ("LINTED_STARTUP"), U.To_Unbounded_String (Linted.Settings.LNTD_STARTUP)),
      when 7 => (U.To_Unbounded_String ("LINTED_SANDBOX"), U.To_Unbounded_String (Linted.Settings.LNTD_SANDBOX)),
      when 8 => (U.To_Unbounded_String ("LINTED_WAITER"), U.To_Unbounded_String (Linted.Settings.LNTD_WAITER)),
      when 9 => (U.To_Unbounded_String ("LINTED_AUDIO"), U.To_Unbounded_String (Linted.Settings.LNTD_AUDIO)),
      when 10 => (U.To_Unbounded_String ("LINTED_AUDIO_FSTAB"), U.To_Unbounded_String (Linted.Settings.LNTD_AUDIO_FSTAB)),
      when 11 => (U.To_Unbounded_String ("LINTED_GUI"), U.To_Unbounded_String (Linted.Settings.LNTD_GUI)),
      when 12 => (U.To_Unbounded_String ("LINTED_GUI_FSTAB"), U.To_Unbounded_String (Linted.Settings.LNTD_GUI_FSTAB)),
      when 13 => (U.To_Unbounded_String ("LINTED_SIMULATOR"), U.To_Unbounded_String (Linted.Settings.LNTD_SIMULATOR)),
      when 14 => (U.To_Unbounded_String ("LINTED_SIMULATOR_FSTAB"), U.To_Unbounded_String (Linted.Settings.LNTD_SIMULATOR_FSTAB)),
      when 15 => (U.To_Unbounded_String ("LINTED_DRAWER"), U.To_Unbounded_String (Linted.Settings.LNTD_DRAWER)),
      when 16 => (U.To_Unbounded_String ("LINTED_DRAWER_FSTAB"), U.To_Unbounded_String (Linted.Settings.LNTD_DRAWER_FSTAB)),
      when 17 => (U.To_Unbounded_String ("LINTED_WINDOW"), U.To_Unbounded_String (Linted.Settings.LNTD_WINDOW)),
      when 18 => (U.To_Unbounded_String ("LINTED_WINDOW_FSTAB"), U.To_Unbounded_String (Linted.Settings.LNTD_WINDOW_FSTAB)),
      when others => raise Constraint_Error);

   Arg_Count : Integer;

   Need_Help : Boolean := False;
   Need_Version : Boolean := False;
   Bad_Option : Boolean := False;

   Pid : Linted.Process.Id;

   Err : Linted.Errors.Error;
begin
   Arg_Count := Command_Line.Argument_Count;
   for II in 1 .. Arg_Count loop
      declare
	 Argument : String := Command_Line.Argument (1);
      begin
	 if "--help" = Argument then
	    Need_Help := True;
	 elsif "--version" = Argument then
	    Need_Version := True;
	 else
	    Bad_Option := True;
	 end if;
      end;
   end loop;

   if Need_Help then
      Libc.Unistd.u_exit (1);
   end if;

   if Bad_Option then
      Libc.Unistd.u_exit (1);
   end if;

   if Need_Version then
      Libc.Unistd.u_exit (1);
   end if;

   for II in 1 .. Envvars_Count loop
      declare
	 Key : U.Unbounded_String;
	 Default_Value : U.Unbounded_String;
      begin
	 declare
	    E : Envvar := Default_Envvars (II);
	 begin
	    Key := E.Key;
	    Default_Value := E.Value;
	 end;

	 Err := Linted.Env.Set (U.To_String (Key), U.To_String (Default_Value), False);
	 if Err /= 0 then
	    goto On_Err;
	 end if;
      end;
   end loop;

   Pid := Linted.Process.Current;
   declare
      P : String := Linted.Process.Id'Image (Pid);
      V : String := P (2 .. P'Last);
   begin
      Err := Linted.Env.Set ("MANAGERPID", V, True);
      if Err /= 0 then
	 goto On_Err;
      end if;

      Linted.Stdio.Write_Line (Linted.KOs.Standard_Output, "LINTED_PID=" & V);
   end;

   declare
      Init : String := Linted.Env.Get ("LINTED_INIT");
      Arr : aliased Interfaces.C.char_array := Interfaces.C.To_C (Init);
      P : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.To_Chars_Ptr (Arr'Unchecked_Access);
      Argv : aliased array (1 .. 2) of aliased Interfaces.C.Strings.Chars_Ptr
	:= (
	    P,
	    Interfaces.C.Strings.Null_Ptr
	   );
      type Ptr_Access is access all Interfaces.C.Strings.Chars_Ptr;
      function Convert is new Ada.Unchecked_Conversion (Ptr_Access, System.Address);
   begin
      Err := Linted.Errors.Error (Libc.Unistd.execv (P, Convert (Argv (1)'Unchecked_Access)));
   end;

<<On_Err>>
   Linted.Stdio.Write_Line (Linted.KOs.Standard_Error, Linted.Errors.To_String (Err));
   Libc.Unistd.u_exit (1);
end Main;
