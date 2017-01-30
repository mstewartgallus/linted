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
   Abstract_State => ((State with External), (Ghost_State)) is
   pragma Elaborate_Body;

   type Node_Access is private;

   protected type Queue is
   private
      procedure Enqueue (N : Node_Access) with
         Global => (In_Out => (State, Ghost_State)),
         Depends =>
         (Queue => (N, Queue),
          State => (Queue, N, State),
          Ghost_State => (N, Ghost_State));
      procedure Try_Dequeue (N : out Node_Access) with
         Global => (In_Out => (State, Ghost_State)),
         Depends =>
         (Queue => (Queue, State),
          State => (Queue, State),
          N => Queue,
          Ghost_State => (Queue, Ghost_State));
      First : Node_Access;
      Last : Node_Access;
   end Queue;

   procedure Enqueue (Q : in out Queue; C : Element_T);

   procedure Try_Dequeue
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean);

private
   type Node_Access is new Natural with
        Default_Value => 0;
end Linted.Queues;
