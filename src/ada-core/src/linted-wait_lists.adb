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
   procedure Insert_Lots (W : in out Wait_List; Root : Node_Access);

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

   procedure Insert_Lots (W : in out Wait_List; Root : Node_Access) is
      Success : Boolean;
      Current_Root : Node_Access;
   begin
      Current_Root := Root;
      while Current_Root /= null loop
         --  Perf opt in the case that there are no waiters
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            null,
            Current_Root,
            Success);
         if Success then
            exit;
         end if;

         declare
            Next : Node_Access;
         begin
            Next := Current_Root.Next;
            Current_Root.Next := null;
            Insert (W, Current_Root);
            Current_Root := Next;
         end;
      end loop;
   end Insert_Lots;

   procedure Wait (W : in out Wait_List) is
      Is_Triggered : Default_False;
   begin
      loop
         Boolean_Atomics.Get (W.Triggered, Is_Triggered);
         if Is_Triggered then
            exit;
         end if;
         declare
            N : aliased Node;
         begin
            Insert (W, N'Unchecked_Access);
            STC.Suspend_Until_True (N.Trigger);
         end;
      end loop;
      Boolean_Atomics.Set (W.Triggered, False);
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
      Root : Node_Access;
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      Node_Access_Atomics.Swap (W.Root, Root, null);

      while Root /= null loop
         declare
            Next : Node_Access;
         begin
            Next := Root.Next;
            Root.Next := null;
            STC.Set_True (Root.Trigger);
            Root := Next;
         end;
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Root : Node_Access;
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      Node_Access_Atomics.Swap (W.Root, Root, null);

      if null = Root then
         return;
      end if;

      declare
         Next : Node_Access;
      begin
         Next := Root.Next;
         Root.Next := null;
         STC.Set_True (Root.Trigger);
         Root := Next;
      end;

      Insert_Lots (W, Root);
   end Signal;
end Linted.Wait_Lists;
