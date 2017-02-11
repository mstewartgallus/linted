-- Copyright 2016,2017 Steven Stewart-Gallus
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
with Linted.Queue;
with Linted.Last_Chance;

package body Linted.Tests is
   Len : constant := 20;
   type Ix is mod Len + 1 with Default_Value => 0;
   type Int is mod Len + 1 with Default_Value => 0;

   function Is_Valid (X : Int) return Boolean is (True);
   package My_Queue is new Queue (Int, Ix, Is_Valid);

   procedure Run is
   begin
      for II in 1 .. Int (Len) loop
	 My_Queue.Enqueue (II);
      end loop;

      for II in 1 .. Int (Len) loop
	 declare
	    Current : Int;
	 begin
	    My_Queue.Dequeue (Current);
	    if Current /= II then
	       raise Program_Error;
	    end if;
	 end;
      end loop;
   end Run;
end Linted.Tests;
