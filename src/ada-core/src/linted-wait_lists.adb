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
package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   protected body Wait_List is
      procedure Insert (N : STC_Node_Access) is
      begin
         N.Next_Trigger := Head;
         Head := N;
      end Insert;

      procedure Remove (N : STC_Node_Access) is
         Last : STC_Node_Access;
         Current_Trigger : STC_Node_Access;
      begin
         Last := null;
         Current_Trigger := Head;

         pragma Assert (Current_Trigger /= null);

         if Current_Trigger = N then
            Head := Current_Trigger.Next_Trigger;
         else
            loop
               Last := Current_Trigger;
               Current_Trigger := Current_Trigger.Next_Trigger;

               pragma Assert (Current_Trigger /= null);

               if Current_Trigger = N then
                  Last.Next_Trigger := Current_Trigger.Next_Trigger;
                  exit;
               end if;
            end loop;
         end if;
      end Remove;

      procedure Broadcast is
         Current_Trigger : STC_Node_Access;
      begin
         Current_Trigger := Head;
         loop
            if null = Current_Trigger then
               exit;
            end if;
            STC.Set_True (Current_Trigger.Trigger);
            Current_Trigger := Current_Trigger.Next_Trigger;
         end loop;
      end Broadcast;
   end Wait_List;

   procedure Wait (W : in out Wait_List) is
      N : aliased STC_Node;
   begin
      W.Insert (N'Unchecked_Access);
      STC.Suspend_Until_True (N.Trigger);
      W.Remove (N'Unchecked_Access);
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
   begin
      W.Broadcast;
   end Broadcast;
end Linted.Wait_Lists;
