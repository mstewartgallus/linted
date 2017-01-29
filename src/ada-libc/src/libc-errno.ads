-- Copyright 2015, 2017 Steven Stewart-Gallus
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

package Libc.Errno with
     Abstract_State =>
     (Errno with
      External =>
        (Async_Readers, Async_Writers, Effective_Reads, Effective_Writes))
is
   pragma Preelaborate;

   function Errno return Interfaces.C.int with
      Volatile_Function;
   pragma Import (C, Errno, "linted_adarts_libc_errno");

   procedure Errno_Get (Err : out Interfaces.C.int);

   procedure Errno_Set (Err : Interfaces.C.int) with
      Global => (Output => Errno),
      Depends => (Errno => Err);
   pragma Import (C, Errno_Set, "linted_adarts_libc_errno_set");

   EDOM : constant := 33;
   EILSEQ : constant := 84;
   ERANGE : constant := 34;
end Libc.Errno;
