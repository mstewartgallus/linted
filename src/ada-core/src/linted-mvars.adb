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
package body Linted.MVars is
   protected body MVar is
      procedure Poll (Option : out Option_Element_Ts.Option) is
      begin
	 Option := Current;
	 if not Current.Empty then
	    Current := (Empty => True);
	 end if;
      end Poll;

      procedure Set (D : Element_T) is
      begin
	 Current := (Empty => False, Data => D);
      end Set;
   end MVar;
end Linted.MVars;
