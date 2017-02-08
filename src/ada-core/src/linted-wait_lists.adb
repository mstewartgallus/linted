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

   procedure Insert (W : in out Wait_List; N : Node_Access);

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

   procedure Broadcast (W : in out Wait_List) is
      Root : Node_Access;
      Next : Node_Access;
   begin
      Node_Access_Atomics.Swap (W.Root, Root, null);
      if null = Root then
         Boolean_Atomics.Set (W.Pending, True);
      end if;

      while Root /= null loop
         Next := Root.Next;
         Root.Next := null;
         STC.Set_True (Root.Trigger);
         Root := Next;
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Root : Node_Access;
      Success : Boolean;
      P : Default_False;
   begin
      loop
         Node_Access_Atomics.Swap (W.Root, Root, null);
         if null = Root then
            Boolean_Atomics.Set (W.Pending, True);
            exit;
         end if;

         declare
            Next : Node_Access;
         begin
            Next := Root.Next;
            Root.Next := null;
            STC.Set_True (Root.Trigger);
            Root := Next;
         end;
         if null = Root then
            exit;
         end if;

         Node_Access_Atomics.Compare_And_Swap (W.Root, null, Root, Success);
         if Success then
            Boolean_Atomics.Swap (W.Pending, P, False);
            if not P then
               exit;
            end if;
         end if;

         --  We have to broadcast in the worst case when we possibly were
         --  signalled more than once.
         loop
            declare
               Next : Node_Access;
            begin
               Next := Root.Next;
               Root.Next := null;
               STC.Set_True (Root.Trigger);
               Root := Next;
            end;
            exit when Root = null;
         end loop;
         exit;
      end loop;
   end Signal;
end Linted.Wait_Lists;
