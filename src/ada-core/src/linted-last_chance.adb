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
with Ada.Characters.Latin_1;
with Ada.Exceptions;

with Interfaces.C;

with Libc.Stdlib;
with Libc.Unistd;

package body Linted.Last_Chance with
     Spark_Mode => Off is
   package Exceptions renames Ada.Exceptions;

   procedure Last_Chance_Handler
     (Except : Exceptions.Exception_Occurrence) with
      No_Return;
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

   procedure Last_Chance_Handler (Except : Exceptions.Exception_Occurrence) is
      X : aliased Interfaces.C.char_array :=
        Interfaces.C.To_C
          (Exceptions.Exception_Information (Except) &
           Ada.Characters.Latin_1.LF);
      Res : Interfaces.C.long;
   begin
      --  Res := Libc.Unistd.write (2, X (X'First)'Address, X'Length);
      pragma Unreferenced (Res);
      Libc.Stdlib.c_exit (1);
   end Last_Chance_Handler;
end Linted.Last_Chance;
