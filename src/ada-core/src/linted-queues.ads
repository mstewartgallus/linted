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
   Spark_Mode => On,
   Abstract_State => (State with External) is
   type Queue is limited private;

   generic
   package User with
      Spark_Mode => On,
      Abstract_State => (User_State with External) is
      procedure Insert (Q : in out Queue; C : Element_T);
      procedure Remove
        (Q : in out Queue;
         C : out Element_T;
         Init : out Boolean);
   end User;

private
   pragma SPARK_Mode (Off);

   type Node_Access;

   type Node;

   type Atomic_Boolean is new Boolean with
        Atomic;
   type Atomic_Node_Access is access all Node with
        Atomic;
   type Node_Access is access all Node;

   type Node is record
      Contents : Element_T;
      Tail : Atomic_Node_Access := null;
      In_Queue : Atomic_Boolean := False;
   end record;

   function Is_Free (N : Node_Access) return Boolean;
   function Is_Null (N : Node_Access) return Boolean;

   protected type Queue is
      procedure Insert (C : Element_T; N : in out Node_Access) with
         Pre => not Is_Null (N) and Is_Free (N),
         Post => Is_Null (N);
      procedure Remove (C : out Element_T; N : out Node_Access) with
         Post => Is_Free (N);
   private
      First : Node_Access;
      Last : Node_Access;
   end Queue;

end Linted.Queues;
