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
with Linted.IO_Pool;
with Linted.Errors;

package Linted.KOs is
   subtype KO is Linted.IO_Pool.KO;

   Standard_Input : KO renames Linted.IO_Pool.Standard_Input;
   Standard_Output : KO renames Linted.IO_Pool.Standard_Output;
   Standard_Error : KO renames Linted.IO_Pool.Standard_Error;

   package KO_Results renames Linted.IO_Pool.KO_Results;

   function Open (Pathname : String) return KO_Results.Result
     renames Linted.IO_Pool.Open;
   function Close (Object : KO) return Linted.Errors.Error
     renames Linted.IO_Pool.Close;
end Linted.KOs;
