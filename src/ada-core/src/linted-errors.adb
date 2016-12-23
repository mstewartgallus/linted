-- Copyright 2016 Steven Stewart-Gallus
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

with Libc.Errno;
with Libc.String;

package body Linted.Errors is
   use type Interfaces.C.int;
   use type Interfaces.C.size_t;

   function To_String (E : Error) return String with SPARK_Mode => Off is
      N : Interfaces.C.size_t := 20;
      Err : Interfaces.C.Int;
   begin
      loop
	 declare
	    Buf : aliased Interfaces.C.Char_Array :=  (1 .. N + 1 => Interfaces.C.nul);
	 begin
	    Err := Libc.String.strerror_r (Interfaces.C.int (E),
					   Interfaces.C.Strings.To_Chars_Ptr (Buf'Unchecked_Access),
					   N);
	    if Err /= Libc.Errno.ERANGE then
	       if 0 = Err then
		  Buf (N) := Interfaces.C.nul;
		  return Interfaces.C.To_Ada (Buf);
	       end if;
	       raise Constraint_Error;
	    end if;
	    N := 2 * N + 10;
	 end;
      end loop;
   end To_String;
end Linted.Errors;
