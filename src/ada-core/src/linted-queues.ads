-- Copyright 2016, 2017 Steven Stewart-Gallus
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
generic
   type Element_T is private;
   Max_Nodes_In_Flight : Positive;
package Linted.Queues with
   Spark_Mode,
   Initializes => Spare_Nodes,
   Abstract_State =>
   ((Spare_Nodes with External),
    (Triggers with External),
    (Elements with External),
    (Tails with External),
    (In_Queues with External)) is
   pragma Elaborate_Body;

   type Node_Access is private;

   protected type Queue is
   private
      procedure Enqueue (N : Node_Access) with
         Global => (In_Out => (Tails, In_Queues)),
         Depends =>
         (Queue => (N, Queue),
          Tails => (Queue, N, Tails),
          In_Queues => (N, In_Queues));
      procedure Try_Dequeue (N : out Node_Access) with
         Global => (In_Out => (Tails, In_Queues)),
         Depends =>
         (Queue => (Queue, Tails),
          Tails => (Queue, Tails),
          N => Queue,
          In_Queues => (Queue, In_Queues));
      First : Node_Access;
      Last : Node_Access;
   end Queue;

   procedure Enqueue (Q : in out Queue; C : Element_T) with
      Global =>
      (In_Out => (Spare_Nodes, Triggers, Elements, Tails, In_Queues)),
      Depends =>
      (Q => (Spare_Nodes, Tails, Q),
       Elements => (Spare_Nodes, Tails, C, Elements),
       Triggers => (Triggers, Tails, Spare_Nodes),
       Spare_Nodes => (Tails, Spare_Nodes),
       Tails => (Spare_Nodes, Q, Tails),
       In_Queues => (Spare_Nodes, Tails, In_Queues));

   procedure Try_Dequeue
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean) with
      Global =>
      (In_Out => (Spare_Nodes, Elements, Triggers, Tails, In_Queues)),
      Depends =>
      (Q => (Q, Tails),
       Tails => (Spare_Nodes, Q, Tails),
       Spare_Nodes => (Q, Spare_Nodes),
       Elements => (Q, Elements),
       Triggers => (Q, Triggers),
       C => (Elements, Q),
       Init => Q,
       In_Queues => (Q, In_Queues));

private
   type Node_Access is new Natural with
        Default_Value => 0;
end Linted.Queues;
