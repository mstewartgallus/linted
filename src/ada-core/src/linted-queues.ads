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
private with Linted.Wait_Lists;

generic
   type Element_T is private;
   Max_Nodes_In_Flight : Positive;
package Linted.Queues with
   Spark_Mode is
   pragma Elaborate_Body;

   type Queue is limited private;

   procedure Enqueue (Q : in out Queue; C : Element_T);
   procedure Try_Dequeue
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean);

   procedure Dequeue (Q : in out Queue; C : out Element_T);

private
   pragma SPARK_Mode (Off);

   type Node_Access is new Natural;
   subtype Node_Not_Null_Access is Node_Access range 1 .. Node_Access'Last;

   function Is_Free (N : Node_Not_Null_Access) return Boolean with
      Ghost;

   protected type PQueue is
      procedure Enqueue (N : Node_Not_Null_Access) with
         Pre => Is_Free (N);
      procedure Try_Dequeue (N : out Node_Access) with
         Post => (if N /= 0 then Is_Free (Node_Not_Null_Access (N)) else True);
   private
      First : Node_Access := 0;
      Last : Node_Access := 0;
   end PQueue;

   type Queue is record
      List : PQueue;
      On_Full : Wait_Lists.Wait_List;
   end record;
end Linted.Queues;
