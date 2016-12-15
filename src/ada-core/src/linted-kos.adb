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
with Libc.Fcntl;
with Libc.Unistd;
with Libc.Errno;
with Interfaces.C.Strings;

package body Linted.KOs is
   package C renames Interfaces.C;
   package Errno renames Libc.Errno;

   function Open (Pathname : String; Flags : Open_Flags) return KO_Results.Result with SPARK_Mode => Off is
      use type Errors.Error;
      use type C.unsigned;

      X : aliased C.char_array := C.To_C (Pathname);
      Err : Errors.Error;
      Fd : C.int;

      C_Flags : C.unsigned := Libc.Fcntl.O_CLOEXEC;

      Has_Read_Only : constant Boolean := (Flags and Read_Only) /= 0;
      Has_Write_Only : constant Boolean := (Flags and Write_Only) /= 0;
      Has_Read_Write : constant Boolean := (Flags and Read_Write) /= 0;
   begin
      if (Flags and not (Read_Only or Write_Only or Read_Write)) /= 0 then
	 return (Erroneous => True, Err => Errors.Invalid_Parameter);
      end if;

      if Has_Read_Only and Has_Write_Only then
	 return (Erroneous => True, Err => Errors.Invalid_Parameter);
      end if;

      if Has_Read_Only and Has_Read_Write then
	 return (Erroneous => True, Err => Errors.Invalid_Parameter);
      end if;

      if Has_Write_Only and Has_Read_Write then
	 return (Erroneous => True, Err => Errors.Invalid_Parameter);
      end if;

      if Has_Read_Only then
	 C_Flags := C_Flags or Libc.Fcntl.O_RDONLY;
      end if;

      if Has_Write_Only then
	 C_Flags := C_Flags or Libc.Fcntl.O_WRONLY;
      end if;

      if Has_Read_Write then
	 C_Flags := C_Flags or Libc.Fcntl.O_RDWR;
      end if;

      Fd := Libc.Fcntl.open (Interfaces.C.Strings.To_Chars_Ptr (X'Unchecked_Access), C.int (C_Flags), 0);
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

   function Close (Object : KOs.KO) return Errors.Error with SPARK_Mode => Off is
   begin
      if Libc.Unistd.close (C.int (Object)) < 0 then
	 return Errors.Error (Errno.Errno);
      else
	 return 0;
      end if;
   end Close;
end Linted.KOs;
