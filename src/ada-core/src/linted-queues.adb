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
with Linted.Wait_Lists;

package body Linted.Queues with
     Spark_Mode => Off,
     Refined_State =>
     (State => (Spare_Nodes, Triggers, Elements, Tails, In_Queues))
is

   Elements : array
   (1 .. Node_Not_Null_Access (Max_Nodes_In_Flight)) of Element_T;
   Tails : array
   (1 .. Node_Not_Null_Access (Max_Nodes_In_Flight)) of Node_Access :=
     (others => 0);
   In_Queues : array
   (1 .. Node_Not_Null_Access (Max_Nodes_In_Flight)) of Boolean :=
     (others => False) with
      Ghost;

   Spare_Nodes : Queue;
   Triggers : Wait_Lists.Wait_List;

   procedure Allocate (N : out Node_Not_Null_Access) with
      Post => Is_Free (N);
   procedure Free (N : Node_Not_Null_Access) with
      Pre => Is_Free (N);

   function Is_Free (N : Node_Not_Null_Access) return Boolean is
   begin
      return not In_Queues (N);
   end Is_Free;

   protected body Queue is
      procedure Enqueue (N : Node_Not_Null_Access) is
      begin
         pragma Assert (not In_Queues (N));
         pragma Assert (Tails (N) = 0);

         In_Queues (N) := True;

         if First = 0 or Last = 0 then
            First := Node_Access (N);
         else
            Tails (Last) := Node_Access (N);
         end if;
         Last := Node_Access (N);
      end Enqueue;

      procedure Try_Dequeue (N : out Node_Access) is
      begin
         if First = 0 or Last = 0 then
            N := 0;
         else
            declare
               Removed : Node_Access;
            begin
               Removed := First;
               pragma Assert (In_Queues (Removed));
               First := Tails (Removed);
               In_Queues (Removed) := False;
               Tails (Removed) := 0;

               N := Removed;
            end;
         end if;
      end Try_Dequeue;
   end Queue;

   procedure Free (N : Node_Not_Null_Access) is
      Dummy : Element_T;
   begin
      Elements (N) := Dummy;
      Spare_Nodes.Enqueue (N);
      Wait_Lists.Signal (Triggers);
   end Free;

   procedure Allocate (N : out Node_Not_Null_Access) is
      Dummy : Element_T;
      Removed : Node_Access;
   begin
      loop
         Spare_Nodes.Try_Dequeue (Removed);
         if Removed /= 0 then
            exit;
         end if;
         Wait_Lists.Wait (Triggers);
      end loop;
      N := Node_Not_Null_Access (Removed);
      Elements (N) := Dummy;
   end Allocate;

   procedure Enqueue (Q : in out Queue; C : Element_T) is
      --  FAKE!
      N : Node_Not_Null_Access := 1;
   begin
      Allocate (N);
      Elements (N) := C;
      Q.Enqueue (N);
   end Enqueue;

   procedure Try_Dequeue
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean)
   is
      Removed : Node_Access;
   begin
      Q.Try_Dequeue (Removed);
      if Removed = 0 then
         Init := False;
      else
         declare
            N : Node_Not_Null_Access := Node_Not_Null_Access (Removed);
         begin
            C := Elements (N);
            Free (N);
            Init := True;
         end;
      end if;
   end Try_Dequeue;

begin
   for II in 1 .. Node_Not_Null_Access (Max_Nodes_In_Flight) loop
      declare
         N : Node_Not_Null_Access := II;
      begin
         Spare_Nodes.Enqueue (N);
      end;
   end loop;
end Linted.Queues;
