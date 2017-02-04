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
with Interfaces.C.Strings;

with Libc.Fcntl;
with Libc.Unistd;
with Libc.Errno;

package body Linted.KOs is
   package C renames Interfaces.C;
   package Errno renames Libc.Errno;

   use type Libc.Sys.Types.ssize_t;

   function Open
     (Pathname : String;
      Flags : Open_Flags) return KO_Results.Result with
      Spark_Mode => Off is

      use type Linted.Errors.Error;
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

      Fd :=
        Libc.Fcntl.open
          (Interfaces.C.Strings.To_Chars_Ptr (X'Unchecked_Access),
           C.int (C_Flags),
           0);
      if Fd < 0 then
         declare
            Errno_Err : Interfaces.C.int;
         begin
            Errno.Errno_Get (Errno_Err);
            Err := Errors.Error (Errno_Err);
         end;
      else
         Err := 0;
      end if;

      if Err /= 0 then
         return (Erroneous => True, Err => Err);
      else
         return (Erroneous => False, Data => KO (Fd));
      end if;
   end Open;

   function Close (Object : KOs.KO) return Errors.Error with
      Spark_Mode => Off is
   begin
      if Libc.Unistd.close (C.int (Object)) < 0 then
         declare
            Errno_Err : Interfaces.C.int;
         begin
            Errno.Errno_Get (Errno_Err);
            return Errors.Error (Errno_Err);
         end;
      else
         return 0;
      end if;
   end Close;

   function Pread
     (Object : KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Offset : Libc.Sys.Types.off_t;
      Bytes_Read : out Interfaces.C.size_t) return Errors.Error with
      Spark_Mode => Off is
      Bytes : Libc.Sys.Types.ssize_t;
   begin
      Bytes :=
        Libc.Unistd.pread (Interfaces.C.int (Object), Buf, Count, Offset);
      if Bytes < 0 then
         declare
            Errno_Err : Interfaces.C.int;
         begin
            Errno.Errno_Get (Errno_Err);
	    Bytes_Read := 0;
            return Linted.Errors.Error (Errno_Err);
         end;
      end if;
      Bytes_Read := Interfaces.C.size_t (Bytes);
      return Linted.Errors.Success;
   end Pread;
end Linted.KOs;
