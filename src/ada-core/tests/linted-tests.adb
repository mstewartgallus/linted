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
with Linted.Queues;
with Linted.Last_Chance;

package body Linted.Tests is
   Len : constant := 20;

   package My_Queues is new Linted.Queues (Integer, Len);
   package My_User is new My_Queues.User;

   L : My_Queues.Queue;

   procedure Run is
      X : Integer := 0;
      Previous : Integer := -1;
   begin
      for II in 1 .. Len loop
	 My_User.Insert (L, X);
	 X := X + 1;
      end loop;

      loop
	 declare
	    Current : Integer;
	    Init : Boolean;
	 begin
	    My_User.Remove (L, Current, Init);
	    if not Init then
	       exit;
	    end if;
	    pragma Assert (Current = Previous + 1);
	    Previous := Current;
	 end;
      end loop;
   end Run;
end Linted.Tests;
