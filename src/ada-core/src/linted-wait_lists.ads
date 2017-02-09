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

   type Node_Type is (Normal_Type, Signal_Type, Broadcast_Type);

   type Node (T : Node_Type := Normal_Type);

   type Node_Access is access all Node;

   type Node (T : Node_Type := Normal_Type) is record
      Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      Next : Node_Access;
      case T is
	 when Normal_Type =>
	    null;
	 when Signal_Type =>
	    null;
	 when Broadcast_Type =>
	    null;
      end case;
   end record;

   type Default_False is new Boolean with
        Default_Value => False;

   package Node_Access_Atomics is new Atomics (Node_Access);
   package Boolean_Atomics is new Atomics (Default_False);

   type Wait_List is record
      Root : Node_Access_Atomics.Atomic;
      Triggered : Boolean_Atomics.Atomic;
   end record;
end Linted.Wait_Lists;
