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
generic
   type Element_T is private;
   Max_Nodes_In_Flight : Positive;
package Linted.Queues with
   Spark_Mode,
   Abstract_State => (State with External) is
   pragma Elaborate_Body;

   type Queue is limited private;

   procedure Insert (Q : in out Queue; C : Element_T) with
      Global => (In_Out => State),
      Depends => (State => (State, Q, C), Q => (State, Q, C));
   procedure Remove
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean) with
      Global => (In_Out => State),
      Depends =>
      (State => (State, Q),
       Q => (State, Q),
       C => (State, Q),
       Init => (State, Q));

private
   pragma SPARK_Mode (Off);

   type Node_Access;

   type Node;

   type Atomic_Boolean is new Boolean with
        Atomic;
   type Atomic_Node_Access is access all Node with
        Atomic;
   type Node_Access is access all Node;
   type Node_Not_Null_Access is not null access all Node;

   type Node is record
      Contents : Element_T;
      Tail : Atomic_Node_Access := null;
      In_Queue : Atomic_Boolean := False;
   end record;

   function Is_Free (N : Node_Not_Null_Access) return Boolean;

   protected type Queue is
      procedure Insert (N : Node_Not_Null_Access) with
         Pre => Is_Free (N);
      procedure Remove (N : out Node_Access) with
         Post =>
         (if N /= null then Is_Free (Node_Not_Null_Access (N)) else True);
   private
      First : Node_Access;
      Last : Node_Access;
   end Queue;

end Linted.Queues;
