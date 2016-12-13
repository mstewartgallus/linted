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
private with Ada.Exceptions;
private with Ada.Text_IO;

with Libc.Unistd;

package body Linted.Last_Chance with SPARK_Mode => Off is
   package Exceptions renames Ada.Exceptions;
   package Text_IO renames Ada.Text_IO;

   procedure Last_Chance_Handler (Except : Exceptions.Exception_Occurrence);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

   procedure Last_Chance_Handler (Except : Exceptions.Exception_Occurrence) is
   begin
      Text_IO.Put_Line (Exceptions.Exception_Information (Except));
      Libc.Unistd.u_exit (1);
      loop
	 null;
      end loop;
   end Last_Chance_Handler;
end Linted.Last_Chance;
