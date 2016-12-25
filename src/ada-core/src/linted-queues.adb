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
package body Linted.Queues with
     Spark_Mode => Off is
   function Is_Null (N : Node_Access) return Boolean is
   begin
      return N = null;
   end Is_Null;

   function Is_Free (N : Node_Access) return Boolean is
   begin
      if N = null then
         return True;
      end if;
      return not Boolean (N.In_Queue);
   end Is_Free;

   protected body Queue is
      procedure Insert (C : Element_T; N : in out Node_Access) is
	 Input : Node_Access;
      begin
	 Input := N;
	 N := null;
         pragma Assert (not Input.In_Queue);
	 pragma Assert (Input.Tail = null);

	 Input.In_Queue := True;
	 Input.Contents := C;

	 if First = null or Last = null then
	    First := Input;
	 else
	    Last.Tail := Atomic_Node_Access (Input);
	 end if;
	 Last := Input;
      end Insert;

      procedure Remove (C : out Element_T; N : out Node_Access) is
      begin
         if First = null or Last = null then
            N := null;
         else
	    declare
	       Removed : Node_Access;
	    begin
	       Removed := First;
	       pragma Assert (Removed.In_Queue);
	       First := Node_Access (Removed.Tail);
	       Removed.In_Queue := False;
	       Removed.Tail := null;

	       C := Removed.Contents;
	       N := Removed;
	    end;
         end if;
      end Remove;
   end Queue;

   package body Pool with
        Refined_State => (State => (Spare_Nodes, Contents)) is

      Contents : array (1 .. Initial_Count) of aliased Node;
      Spare_Nodes : Queue;

      procedure Allocate (N : out Node_Access) is
	 Dummy : Element_T;
      begin
         Spare_Nodes.Remove (Dummy, N);
      end Allocate;

      procedure Free (N : in out Node_Access) is
	 Dummy : Element_T;
      begin
         Spare_Nodes.Insert (Dummy, N);
      end Free;

   begin
      for II in Contents'Range loop
	 declare
	    N : Node_Access := Contents (II)'Unchecked_Access;
	    Dummy : Element_T;
	 begin
	    pragma Assert (Is_Free (N));
	    pragma Assert (not Is_Null (N));
	    Spare_Nodes.Insert (Dummy, N);
	 end;
      end loop;
   end Pool;
end Linted.Queues;
