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
with Ada.Command_Line;

with Interfaces.C;
with Interfaces.C.Strings;

with Libc.Syslog;

with Linted.Errors;
with Linted.KOs;

package body Linted.Logs with
     Spark_Mode => Off is
   package C renames Interfaces.C;
   package C_Strings renames Interfaces.C.Strings;

   use type C.unsigned;

   procedure syslog
     (pri : C.int;
      fmt : C_Strings.chars_ptr;
      str : C_Strings.chars_ptr);
   pragma Import (C, syslog, "syslog");

   procedure Log (Pri : Priority; Str : String) is
      Sys_Pri : C.int;
   begin
      declare
         Ident : aliased C.char_array :=
           C.To_C (Ada.Command_Line.Command_Name);
      begin
         Libc.Syslog.openlog
           (C_Strings.To_Chars_Ptr (Ident'Unchecked_Access),
            C.int (C.unsigned (Libc.Syslog.LOG_PID) or Libc.Syslog.LOG_NDELAY),
            Libc.Syslog.LOG_USER);
      end;

      case Pri is
         when Error =>
            Sys_Pri := Libc.Syslog.LOG_ERR;
         when Warning =>
            Sys_Pri := Libc.Syslog.LOG_WARNING;
         when Info =>
            Sys_Pri := Libc.Syslog.LOG_INFO;
      end case;
      declare
         Fmt : aliased C.char_array := C.To_C ("%s");
         C_Str : aliased C.char_array := C.To_C (Str);
      begin
         syslog
           (Sys_Pri,
            C_Strings.To_Chars_Ptr (Fmt'Unchecked_Access),
            C_Strings.To_Chars_Ptr (C_Str'Unchecked_Access));
      end;
   end Log;

end Linted.Logs;
