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
limited with System;

with Linted.Errors;
with Linted.KOs;

package Linted.Stdio is
   pragma Elaborate_Body;

   use Linted.Errors;
   use Linted.KOs;

   procedure Write_String (Object : KO;
			   Str : String;
			   Err : out Error);

   procedure Write (Object : KO;
		    Buf : System.Address;
		    Count :  Interfaces.C.size_t;
		    Bytes_Written : out Interfaces.C.size_t;
		    Err : out Error);
end Linted.Stdio;
