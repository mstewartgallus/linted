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
with Linted.Wait_Lists;

package body Linted.Queues with
     Spark_Mode => Off,
     Refined_State => (State => (Contents, Spare_Nodes, Triggers)) is

   type Node_Array is array (Positive range <>) of aliased Node;

   Contents : Node_Array (1 .. Max_Nodes_In_Flight);
   Spare_Nodes : Queue;
   Triggers : Wait_Lists.Wait_List;

   procedure Allocate (N : out Node_Not_Null_Access) with
      Post => Is_Free (N);
   procedure Free (N : Node_Not_Null_Access) with
      Pre => Is_Free (N);

   function Is_Free (N : Node_Not_Null_Access) return Boolean is
   begin
      return not Boolean (N.In_Queue);
   end Is_Free;

   protected body Queue is
      procedure Insert (N : Node_Not_Null_Access) is
      begin
         pragma Assert (not N.In_Queue);
         pragma Assert (N.Tail = null);

         N.In_Queue := True;

         if First = null or Last = null then
            First := Node_Access (N);
         else
            Last.Tail := Atomic_Node_Access (N);
         end if;
         Last := Node_Access (N);
      end Insert;

      procedure Remove (N : out Node_Access) is
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

               N := Removed;
            end;
         end if;
      end Remove;
   end Queue;

   procedure Free (N : Node_Not_Null_Access) is
      Dummy : Element_T;
   begin
      N.Contents := Dummy;
      Spare_Nodes.Insert (N);
      Wait_Lists.Signal (Triggers);
   end Free;

   procedure Allocate (N : out Node_Not_Null_Access) is
      Dummy : Element_T;
      Removed : Node_Access;
   begin
      loop
         Spare_Nodes.Remove (Removed);
         if Removed /= null then
            exit;
         end if;
	 Wait_Lists.Wait (Triggers);
      end loop;
      N := Node_Not_Null_Access (Removed);
      N.Contents := Dummy;
   end Allocate;

   procedure Insert (Q : in out Queue; C : Element_T) is
      Dummy : aliased Node;
      N : Node_Not_Null_Access := Dummy'Unchecked_Access;
   begin
      Allocate (N);
      N.Contents := C;
      Q.Insert (N);
   end Insert;

   procedure Remove
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean)
   is
      Removed : Node_Access;
      Dummy : aliased Node;
      N : Node_Not_Null_Access := Dummy'Unchecked_Access;
   begin
      Q.Remove (Removed);
      if Removed = null then
         Init := False;
      else
	 N := Node_Not_Null_Access (Removed);
	 C := N.Contents;
         Free (N);
         Init := True;
      end if;
   end Remove;

begin
   for II in Contents'Range loop
      declare
         N : Node_Not_Null_Access := Contents (II)'Unchecked_Access;
      begin
         Spare_Nodes.Insert (N);
      end;
   end loop;
end Linted.Queues;
