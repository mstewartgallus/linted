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
package body Linted.MVars is
   protected body MVar is
      procedure Poll (Option : out Option_Element_Ts.Option) is
      begin
	 if Full then
	    Full := False;
	    Option := (False, Current);
	 else
	    Option := (Empty => True);
	 end if;
      end Poll;

      procedure Set (D : Element_T) is
      begin
	 Current := D;
	 Full := True;
      end Set;

      procedure Set_And_Check (D : Element_T; Was_Full : out Boolean) is
      begin
	 Was_Full := Full;
	 Current := D;
	 Full := True;
      end Set_And_Check;
   end MVar;
end Linted.MVars;
