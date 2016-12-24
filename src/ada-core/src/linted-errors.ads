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
with Interfaces.C;

with Libc.Errno.POSIX_2008;

package Linted.Errors with
     Abstract_State => null is
   pragma Preelaborate;

   subtype Valid_Error is
     Interfaces.C
       .unsigned range
       0 ..
         Interfaces.C.unsigned (Interfaces.C.int'Last);
   type Error is new Valid_Error with
        Default_Value => 0;

   Success : constant Error;
   Permission : constant Error;
   Protocol : constant Error;
   Unimplemented : constant Error;
   Out_Of_Memory : constant Error;
   Invalid_Parameter : constant Error;

   function To_String (E : Error) return String with
      Spark_Mode => Off;

private
   Success : constant Error := 0;

   Permission : constant Error := Libc.Errno.POSIX_2008.EPERM;
   Protocol : constant Error := Libc.Errno.POSIX_2008.EPROTO;
   Unimplemented : constant Error := Libc.Errno.POSIX_2008.ENOSYS;
   Out_Of_Memory : constant Error := Libc.Errno.POSIX_2008.ENOMEM;
   Invalid_Parameter : constant Error := Libc.Errno.POSIX_2008.EINVAL;
end Linted.Errors;
