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
with Ada.Real_Time;

private with Linted.XAtomics;
private with Linted.Sched;
private with Linted.Tagged_Accessors;

package Linted.Wait_Lists with
     Spark_Mode is
   pragma Elaborate_Body;

   type Wait_List is limited private;

   procedure Wait (W : in out Wait_List) with
      Global => (Input => Ada.Real_Time.Clock_Time),
      Depends => (W => W, null => Ada.Real_Time.Clock_Time);
   procedure Broadcast (W : in out Wait_List) with
      Global => (Input => Ada.Real_Time.Clock_Time),
      Depends => (W => W, null => Ada.Real_Time.Clock_Time);
   procedure Signal (W : in out Wait_List) with
      Global => (Input => Ada.Real_Time.Clock_Time),
      Depends => (W => W, null => Ada.Real_Time.Clock_Time);

private
   pragma SPARK_Mode (Off);

   type Node;

   type Default_False is new Boolean with
        Default_Value => False;

   type Node_Access is access all Node;

   package Tags is new Tagged_Accessors (Node_Access);
   package Node_Access_Atomics is new XAtomics (Tags.Tagged_Access);
   package Boolean_Atomics is new XAtomics (Default_False);

   type Wait_List is record
      Root : Node_Access_Atomics.Atomic;
      Triggered : Boolean_Atomics.Atomic;
      Head_Contention : Sched.Contention;
   end record;
end Linted.Wait_Lists;
