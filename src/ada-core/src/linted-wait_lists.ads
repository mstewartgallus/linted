-- Copyright 2016,2017 Steven Stewart-Gallus
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
private with Linted.Atomics;
private with Ada.Synchronous_Task_Control;

package Linted.Wait_Lists with
     Spark_Mode is
   pragma Preelaborate;

   type Wait_List is limited private;

   procedure Wait (W : in out Wait_List) with
      Global => null,
      Depends => (W => W);
   procedure Broadcast (W : in out Wait_List) with
      Global => null,
      Depends => (W => W);
   procedure Signal (W : in out Wait_List) with
      Global => null,
      Depends => (W => W);

private
   pragma SPARK_Mode (Off);

   type Node;

   type Default_False is new Boolean with
        Default_Value => False;

   package Tagged_Accessors is
      type Node_Access is private;

      type Tag_Type is (Normal, Signal, Broadcast);

      function To (Ptr : access Node) return Node_Access;
      function To (Ptr : access Node; My_Tag : Tag_Type) return Node_Access;
      function From (Ptr : Node_Access) return access Node;
      function Tag (Ptr : Node_Access) return Tag_Type;
   private
      type Node_Access is mod 2**64 with
           Default_Value => 0;
   end Tagged_Accessors;

   type Node is record
      Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      Next : Tagged_Accessors.Node_Access;
   end record;

   package Node_Access_Atomics is new Atomics (Tagged_Accessors.Node_Access);
   package Boolean_Atomics is new Atomics (Default_False);

   type Wait_List is record
      Root : Node_Access_Atomics.Atomic;
      Triggered : Boolean_Atomics.Atomic;
   end record;
end Linted.Wait_Lists;
