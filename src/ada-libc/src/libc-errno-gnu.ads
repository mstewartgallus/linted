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
with Interfaces.C.Strings;
with Libc.Errno.POSIX_2008;

package Libc.Errno.GNU with SPARK_Mode => Off is
   pragma Preelaborate;

   program_invocation_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/errno.h:54
   pragma Import (C, program_invocation_name, "program_invocation_name");

   program_invocation_short_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/errno.h:54
   pragma Import (C, program_invocation_short_name, "program_invocation_short_name");

   ENOTBLK : constant := 15;
   ECHRNG : constant := 44;
   EL2NSYNC : constant := 45;
   EL3HLT : constant := 46;
   EL3RST : constant := 47;
   ELNRNG : constant := 48;
   EUNATCH : constant := 49;
   ENOCSI : constant := 50;
   EL2HLT : constant := 51;
   EBADE : constant := 52;
   EBADR : constant := 53;
   EXFULL : constant := 54;
   ENOANO : constant := 55;
   EBADRQC : constant := 56;
   EBADSLT : constant := 57;
   EDEADLOCK : constant := Libc.Errno.POSIX_2008.EDEADLK;
   EBFONT : constant := 59;
   ENONET : constant := 64;
   ENOPKG : constant := 65;
   EREMOTE : constant := 66;
   EADV : constant := 68;
   ESRMNT : constant := 69;
   ECOMM : constant := 70;
   EDOTDOT : constant := 73;
   ENOTUNIQ : constant := 76;
   EBADFD : constant := 77;
   EREMCHG : constant := 78;
   ELIBACC : constant := 79;
   ELIBBAD : constant := 80;
   ELIBSCN : constant := 81;
   ELIBMAX : constant := 82;
   ELIBEXEC : constant := 83;
   ERESTART : constant := 85;
   ESTRPIPE : constant := 86;
   EUSERS : constant := 87;
   ESOCKTNOSUPPORT : constant := 94;
   EPFNOSUPPORT : constant := 96;
   EADDRNOTAVAIL : constant := 99;
   ESHUTDOWN : constant := 108;
   ETOOMANYREFS : constant := 109;
   EHOSTDOWN : constant := 112;
   EUCLEAN : constant := 117;
   ENOTNAM : constant := 118;
   ENAVAIL : constant := 119;
   EISNAM : constant := 120;
   EREMOTEIO : constant := 121;
   ENOMEDIUM : constant := 123;
   EMEDIUMTYPE : constant := 124;
   ENOKEY : constant := 126;
   EKEYEXPIRED : constant := 127;
   EKEYREVOKED : constant := 128;
   EKEYREJECTED : constant := 129;
   ERFKILL : constant := 132;
   EHWPOISON : constant := 133;
end Libc.Errno.GNU;
