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
with Ada.Finalization;

private with Linted.GCC_Atomics;
private with Linted.Sched;
private with Linted.Tagged_Accessors;

package Linted.Wait_Lists with
     Spark_Mode,
     Abstract_State => (State with External) is
   pragma Elaborate_Body;

   type Wait_List is limited private;

   procedure Wait (W : in out Wait_List) with
      Global => (In_Out => State),
      Depends => ((State, W) => (State, W));
   procedure Broadcast (W : in out Wait_List) with
      Global => (In_Out => State),
      Depends => ((State, W) => (State, W));
   procedure Signal (W : in out Wait_List) with
      Global => (In_Out => State),
      Depends => ((State, W) => (State, W));
private
   pragma SPARK_Mode (Off);

   type Node;

   type Default_False is new Boolean with
        Default_Value => False;
   for Default_False'Size use 8;

   type Node_Access is access all Node;

   package Tags is new Tagged_Accessors (Node_Access);
   package Node_Access_Atomics is new GCC_Atomics.Atomic_Ts
     (Tags.Tagged_Access);
   package Boolean_Atomics is new GCC_Atomics.Atomic_Ts (Default_False);

   type Queue is new Ada.Finalization.Limited_Controlled with record
      Head : Node_Access_Atomics.Atomic;
      Tail : Node_Access_Atomics.Atomic;
      Head_Contention : Sched.Contention;
      Tail_Contention : Sched.Contention;
   end record;

   overriding procedure Initialize (Q : in out Queue) with
      Global => null,
      Depends => (Q => Q);

   type Wait_List is record
      Q : Queue;
      Triggered : Boolean_Atomics.Atomic;
   end record;
end Linted.Wait_Lists;
