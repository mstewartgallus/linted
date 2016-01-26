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

with Linted.Errors;
with Linted.Results;

package Linted.KOs is
   pragma Preelaborate;

   use type Interfaces.C.int;

   subtype Valid_KO is Interfaces.C.int range -1 .. Interfaces.C.int'Last;
   type KO is new Valid_KO
     with Default_Value => -1;

   Standard_Input : constant KO;
   Standard_Output : constant KO;
   Standard_Error : constant KO;

   package KO_Results is new Linted.Results (KO);

   function Open (Pathname : String) return KO_Results.Result;
   function Close (Object : KO) return Errors.Error;

private
   Invalid : constant KO := -1;

   Standard_Input : constant KO := 0;
   Standard_Output : constant KO := 1;
   Standard_Error : constant KO := 2;
end Linted.KOs;
