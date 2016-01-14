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
   function Poll (This : in out MVar) return Option_Element_Ts.Option is
      Option : Option_Element_Ts.Option;
   begin
      if This.Full then
	 This.Object.Poll (Option, This.Full'Access);
	 return Option;
      else
	 return (Empty => True);
      end if;
   end Poll;

   procedure Set (This : in out MVar; D : Element_T) is
   begin
      This.Object.Set (D);
      This.Full := True;
   end Set;

   protected body Impl is
      procedure Poll (Option : out Option_Element_Ts.Option;
		      Full : not null access Atomic_Boolean) is
      begin
	 if Full.all then
	    Full.all := False;
	    Option := (False, Current);
	 else
	    Option := (Empty => True);
	 end if;
      end Poll;

      procedure Set (D : Element_T) is
      begin
	 Current := D;
      end Set;
   end Impl;
end Linted.MVars;
