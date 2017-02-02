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
package body Linted.WQueues with
     Spark_Mode => Off,
     Refined_State =>
     (State =>
        (My_Queues.Spare_Nodes,
         My_Queues.Tails,
         My_Queues.In_Queues,
         My_Queues.Elements))
is
   procedure Enqueue (Q : in out WQueue; C : Element_T) is
   begin
      My_Queues.Enqueue (Q.Queue, C);
      Wait_Lists.Signal (Q.Wait_List);
   end Enqueue;

   procedure Dequeue (Q : in out WQueue; C : out Element_T) is
      Init : Boolean;
   begin
      loop
         My_Queues.Try_Dequeue (Q.Queue, C, Init);
         if Init then
            exit;
         end if;
         Wait_Lists.Wait (Q.Wait_List);
      end loop;
   end Dequeue;
end Linted.WQueues;
