-- Copyright 2017 Steven Stewart-Gallus
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
private with Linted.Queues;

generic
   type Element_T is private;
   Max_Nodes_In_Flight : Positive;
package Linted.WQueues with
   Spark_Mode,
   Initializes => State,
   Abstract_State => (State with External) is
   pragma Elaborate_Body;

   type WQueue is limited private;

   procedure Enqueue (Q : in out WQueue; C : Element_T) with
      Global => (In_Out => State),
      Depends => (Q => (Q, State, C), State => (Q, State, C));
   procedure Dequeue (Q : in out WQueue; C : out Element_T) with
      Global => (In_Out => State),
      Depends => (Q => (Q, State), State => (Q, State), C => (State, Q));

private
   pragma SPARK_Mode (Off);

   package My_Queues is new Queues (Element_T, Max_Nodes_In_Flight);

   type WQueue is record
      Wait_List : Wait_Lists.Wait_List;
      Queue : My_Queues.Queue;
   end record;
end Linted.WQueues;
