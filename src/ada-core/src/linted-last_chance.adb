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
with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

with Interfaces.C;

with Libc.Stdlib;
with Libc.Unistd;

package body Linted.Last_Chance with
     Spark_Mode => Off is
   package Exceptions renames Ada.Exceptions;
   package Task_Id renames Ada.Task_Identification;
   package Task_Term renames Ada.Task_Termination;
   package C renames Interfaces.C;

   protected Grim_Reaper is
      procedure Last_Gasp
        (Term_Cause : Task_Term.Cause_Of_Termination;
         T : Task_Id.Task_Id;
         X : Exceptions.Exception_Occurrence);
   end Grim_Reaper;

   protected body Grim_Reaper is
      procedure Last_Gasp
        (Term_Cause : Task_Term.Cause_Of_Termination;
         T : Task_Id.Task_Id;
         X : Exceptions.Exception_Occurrence)
      is
         Str : aliased C.char_array :=
           C.To_C (Exceptions.Exception_Information (X));
         Res : C.long;
      begin
         Res := Libc.Unistd.write (2, Str (Str'First)'Address, Str'Length);
         pragma Unreferenced (Res);
         Libc.Stdlib.c_exit (1);
      end Last_Gasp;
   end Grim_Reaper;
begin
   Task_Term.Set_Dependents_Fallback_Handler (Grim_Reaper.Last_Gasp'Access);
end Linted.Last_Chance;
