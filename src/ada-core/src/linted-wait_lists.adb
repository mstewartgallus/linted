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
with Ada.Synchronous_Task_Control;

package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   type Node is record
      Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      Next_Trigger : Node_Access;
   end record;

   protected body Wait_List is
      procedure Insert (N : Node_Nonnull_Access) is
      begin
         if First = null or Last = null then
            First := Node_Access (N);
         else
            Last.Next_Trigger := Node_Access (N);
         end if;
         Last := Node_Access (N);
      end Insert;

      procedure Remove (N : Node_Nonnull_Access) is
         Current_Trigger : Node_Nonnull_Access := Node_Nonnull_Access (First);
      begin
         if Current_Trigger = N then
            First := Current_Trigger.Next_Trigger;
            if Node_Nonnull_Access (Last) = N then
               pragma Assert (Last.Next_Trigger = null);
               Last := null;
            end if;
         else
            loop
               declare
                  Before : Node_Nonnull_Access := Current_Trigger;
               begin
                  Current_Trigger :=
                    Node_Nonnull_Access (Current_Trigger.Next_Trigger);

                  if Current_Trigger = N then
                     Before.Next_Trigger := Current_Trigger.Next_Trigger;
                     if Node_Nonnull_Access (Last) = N then
                        pragma Assert (Last.Next_Trigger = null);
                        Last := Node_Access (Before);
                     end if;
                     exit;
                  end if;
               end;
            end loop;
         end if;
      end Remove;

      procedure Broadcast is
         Current_Trigger : Node_Access;
      begin
         Current_Trigger := First;
         loop
            if null = Current_Trigger then
               exit;
            end if;
            STC.Set_True (Current_Trigger.Trigger);
            Current_Trigger := Current_Trigger.Next_Trigger;
         end loop;
      end Broadcast;

      procedure Signal is
         Current_Trigger : Node_Access;
      begin
         Current_Trigger := First;

         if Current_Trigger /= null then
            STC.Set_True (Current_Trigger.Trigger);
         end if;
      end Signal;
   end Wait_List;

   procedure Wait (W : in out Wait_List) is
      N : aliased Node;
   begin
      W.Insert (N'Unchecked_Access);
      STC.Suspend_Until_True (N.Trigger);
      W.Remove (N'Unchecked_Access);
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
   begin
      W.Broadcast;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
   begin
      W.Signal;
   end Signal;
end Linted.Wait_Lists;
