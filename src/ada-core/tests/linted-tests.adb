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
with Linted.Lists;
with Linted.Last_Chance;

package body Linted.Tests is
   package My_Lists is new Linted.Lists (Integer);
   package My_Pool is new My_Lists.Pool (20);

   L : My_Lists.List;

   procedure Run is
      X : Integer := 0;
      Previous : Integer := -1;
   begin
      loop
	 declare
	    N : My_Lists.Node_Access;
	 begin
	    My_Pool.Allocate (N);
	    if My_Lists.Is_Null (N) then
	       exit;
	    end if;
	    L.Insert (X, N);
	    X := X + 1;
	 end;
      end loop;

      loop
	 declare
	    N : My_Lists.Node_Access;
	    Current : Integer;
	 begin
	    L.Remove (Current, N);
	    if My_Lists.Is_Null (N) then
	       exit;
	    end if;
	    pragma Assert (Current = Previous + 1);
	    Previous := Current;
	 end;
      end loop;
   end Run;
end Linted.Tests;
