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
private with Ada.Synchronous_Task_Control;

package Linted.Wait_Lists with
     Spark_Mode => On is
   type Wait_List is limited private;

   procedure Wait (W : in out Wait_List) with
      Global => null,
      Depends => (W => W);
   procedure Broadcast (W : in out Wait_List) with
      Global => null,
      Depends => (W => W);

private
   pragma SPARK_Mode (Off);

   type STC_Node;

   type STC_Node_Access is access all STC_Node;

   type STC_Node is record
      Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      Next_Trigger : STC_Node_Access;
   end record;

   protected type Wait_List is
      procedure Insert (N : STC_Node_Access);
      procedure Remove (N : STC_Node_Access);
      procedure Broadcast;
   private
      Head : STC_Node_Access;
   end Wait_List;
end Linted.Wait_Lists;
