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

   use type Tags.Tag_Bits;
   use type Sched.Backoff_State;

   type Node is record
      Trigger : STC.Suspension_Object;
      Next : Node_Access_Atomics.Atomic;
   end record;

   procedure Insert (W : in out Wait_List; N : Node_Access);
   procedure Pop (W : in out Wait_List; N : out Node_Access);

   Local_Waiter : Node_Access := null;
   pragma Thread_Local_Storage (Local_Waiter);

   procedure Insert (W : in out Wait_List; N : Node_Access) is
      Head : Tags.Tagged_Access;
      Success : Boolean;
   begin
      loop
         Head := Node_Access_Atomics.From (W.Root);
         N.Next := Node_Access_Atomics.To (Tags.To (Tags.From (Head), 0));
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            Head,
            Tags.To (N, Tags.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (W.Head_Contention);
      end loop;
      Sched.Success (W.Head_Contention);
   end Insert;

   procedure Wait (W : in out Wait_List) is
      Is_Triggered : Default_False;
   begin
      Boolean_Atomics.Swap (W.Triggered, Is_Triggered, False);
      if Is_Triggered then
         return;
      end if;

      if null = Local_Waiter then
         Local_Waiter := new Node;
      end if;

      Insert (W, Local_Waiter);
      STC.Suspend_Until_True (Local_Waiter.Trigger);
   end Wait;

   procedure Pop (W : in out Wait_List; N : out Node_Access) is
      Head : Tags.Tagged_Access;
      Next : Tags.Tagged_Access;
      Success : Boolean;
   begin
      loop
	 Head := Node_Access_Atomics.From (W.Root);
         if null = Tags.From (Head) then
            N := null;
            Sched.Success (W.Head_Contention);
            return;
         end if;
         N := Tags.From (Head);
         --  .Next is safe because nodes are never deallocated
	 Next := Node_Access_Atomics.From (N.Next);
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            Head,
            Tags.To (Tags.From (Next), Tags.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (W.Head_Contention);
      end loop;
      Sched.Success (W.Head_Contention);
      N.Next := Node_Access_Atomics.To (Tags.To (null, 0));
   end Pop;

   procedure Broadcast (W : in out Wait_List) is
      Head : Tags.Tagged_Access;
      N : Node_Access;
      Success : Boolean;
   begin
      W.Triggered := Boolean_Atomics.To (True);

      loop
	 Head := Node_Access_Atomics.From (W.Root);
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            Head,
            Tags.To (null, Tags.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (W.Head_Contention);
      end loop;
      Sched.Success (W.Head_Contention);

      N := Tags.From (Head);
      while N /= null loop
	 declare
	    Next : Node_Access;
	 begin
	    Next := Tags.From (Node_Access_Atomics.From (N.Next));
	    STC.Set_True (N.Trigger);
	    N := Next;
	 end;
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Head : Node_Access;
   begin
      W.Triggered := Boolean_Atomics.To (True);

      Pop (W, Head);
      if Head /= null then
         STC.Set_True (Head.Trigger);
      end if;
   end Signal;
end Linted.Wait_Lists;
