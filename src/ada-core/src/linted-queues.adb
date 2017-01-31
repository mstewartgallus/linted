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
     Refined_State =>
     (Spare_Nodes => PSpare_Nodes,
      Triggers => PTriggers,
      Elements => PElements,
      Tails => PTails,
      In_Queues => PIn_Queues)
is

   type PElements_Array is array (Node_Access range <>) of Element_T;
   type PTails_Array is array (Node_Access range <>) of Node_Access with
        Atomic_Components;
   type Booleans_Array is array (Node_Access range <>) of Boolean;

   PElements : PElements_Array (1 .. Node_Access (Max_Nodes_In_Flight));
   PTails : PTails_Array (1 .. Node_Access (Max_Nodes_In_Flight)) :=
     (others => 0);
   PIn_Queues : Booleans_Array (1 .. Node_Access (Max_Nodes_In_Flight)) :=
     (others => False) with
      Ghost;

   PSpare_Nodes : Queue;
   PTriggers : Wait_Lists.Wait_List;

   procedure Allocate (N : out Node_Access) with
      Global =>
      (In_Out => (PSpare_Nodes, PTriggers, PElements, PIn_Queues, PTails)),
      Depends =>
      (PSpare_Nodes => (PSpare_Nodes, PTails),
       PTriggers => (PTriggers, PSpare_Nodes, PTails),
       PElements => (PTails, PSpare_Nodes, PElements),
       PIn_Queues => (PSpare_Nodes, PTails, PIn_Queues),
       N => (PTails, PSpare_Nodes),
       PTails => (PTails, PSpare_Nodes));
   procedure Free (N : Node_Access) with
      Global =>
      (In_Out => (PSpare_Nodes, PTriggers, PElements, PIn_Queues, PTails)),
      Depends =>
      (PSpare_Nodes => (N, PSpare_Nodes),
       PTriggers => PTriggers,
       PElements => (N, PElements),
       PIn_Queues => (N, PIn_Queues),
       PTails => (PTails, N, PSpare_Nodes));

   protected body Queue is
      procedure Enqueue (N : Node_Access) is
         T : Node_Access;
      begin
         T := PTails (N);
         pragma Assert (T = 0);

         pragma Assert (not PIn_Queues (N));

         PIn_Queues (N) := True;

         if First = 0 or Last = 0 then
            First := N;
         else
            PTails (Last) := N;
         end if;
         Last := N;
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

               pragma Assert (PIn_Queues (Removed));

               First := PTails (Removed);
               PIn_Queues (Removed) := False;
               PTails (Removed) := 0;

               N := Removed;
            end;
         end if;
      end Try_Dequeue;
   end Queue;

   procedure Free (N : Node_Access) is
      Dummy : Element_T;
   begin
      pragma Assert (not PIn_Queues (N));

      PElements (N) := Dummy;
      PSpare_Nodes.Enqueue (N);
      Wait_Lists.Signal (PTriggers);
   end Free;

   procedure Allocate (N : out Node_Access) is
      Dummy : Element_T;
      Removed : Node_Access;
   begin
      loop
         PSpare_Nodes.Try_Dequeue (Removed);
         if Removed /= 0 then
            exit;
         end if;
         Wait_Lists.Wait (PTriggers);
      end loop;
      N := Removed;
      PElements (N) := Dummy;

      pragma Assert (not PIn_Queues (N));
   end Allocate;

   procedure Enqueue (Q : in out Queue; C : Element_T) is
      N : Node_Access;
   begin
      Allocate (N);
      PElements (N) := C;
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
         declare
            Dummy : Element_T;
         begin
            C := Dummy;
         end;
         Init := False;
      else
         declare
            N : Node_Access := Removed;
         begin
            C := PElements (N);
            Free (N);
            Init := True;
         end;
      end if;
   end Try_Dequeue;

begin
   for II in 1 .. Node_Access (Max_Nodes_In_Flight) loop
      declare
         N : Node_Access := II;
      begin
         PSpare_Nodes.Enqueue (N);
      end;
   end loop;
end Linted.Queues;
