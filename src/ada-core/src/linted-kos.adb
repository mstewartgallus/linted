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
with Unix.Fcntl;
with Unix.Unistd;
with C89.Errno;
with Interfaces.C.Strings;

package body Linted.KOs is
   package C renames Interfaces.C;
   package Errno renames C89.Errno;

   function Open (Pathname : String) return KO_Results.Result is
      use type Errors.Error;
      use type C.unsigned;

      X : aliased C.char_array := C.To_C (Pathname);
      Err : Errors.Error;
      Fd : C.int;
   begin
      Fd := Unix.Fcntl.open (Interfaces.C.Strings.To_Chars_Ptr (X'Unchecked_Access), C.int (C.unsigned (Unix.Fcntl.O_RDWR) or Unix.Fcntl.O_CLOEXEC), 0);
      if Fd < 0 then
	 Err := Errors.Error (Errno.Errno);
      else
	 Err := 0;
      end if;

      if Err /= 0 then
	 return (Erroneous => True, Err => Err);
      else
	 return (Erroneous => False, Data => KO (Fd));
      end if;
   end Open;

   function Close (Object : KOs.KO) return Errors.Error is
   begin
      if Unix.Unistd.close (C.int (Object)) < 0 then
	 return Errors.Error (Errno.Errno);
      else
	 return 0;
      end if;
   end Close;
end Linted.KOs;
