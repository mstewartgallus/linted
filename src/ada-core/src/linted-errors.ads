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
with Interfaces.C;

package Linted.Errors is
   subtype Valid_Error is Interfaces.C.int range 0 .. Interfaces.C.int'Last;
   type Error is  new Valid_Error
     with Default_Value => 0;

   Success : constant Error;

private
   Success : constant Error := 0;
end Linted.Errors;
