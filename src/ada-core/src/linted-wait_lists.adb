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
with Ada.Synchronous_Task_Control;

package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   type Node is record
      Trigger : STC.Suspension_Object;
      Next : Node_Access;
   end record;

   procedure Insert (W : in out Wait_List; N : Node_Access) is
      Head : Node_Access;
      Success : Boolean;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         N.Next := Head;
         Node_Access_Atomics.Compare_And_Swap (W.Root, Head, N, Success);
         exit when Success;
      end loop;
   end Insert;

   procedure Wait (W : in out Wait_List) is
      N : aliased Node;
      P : Default_False;
   begin
      Boolean_Atomics.Swap (W.Pending, P, False);
      if not P then
         Insert (W, N'Unchecked_Access);
         STC.Suspend_Until_True (N.Trigger);
      end if;
   end Wait;

   procedure Signal (W : in out Wait_List) is
      Root : Node_Access;
      Next : Node_Access;
   begin
      Boolean_Atomics.Set (W.Pending, True);
      Node_Access_Atomics.Swap (W.Root, Root, null);
      if Root /= null then
         Next := Root.Next;
         Root.Next := null;
         Boolean_Atomics.Set (W.Pending, False);
         STC.Set_True (Root.Trigger);

         loop
            exit when Next = null;
            declare
               New_Next : Node_Access := Next.Next;
            begin
               Insert (W, Next);
               Next := New_Next;
            end;
         end loop;
      end if;
   end Signal;
end Linted.Wait_Lists;
