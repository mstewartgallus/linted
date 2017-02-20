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

--  Use Simple, Fast, and Practical Non-Blocking and Blocking
--  Concurrent Queue Algorithms from
--  http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html

package body Linted.Wait_Lists with
     Spark_Mode => Off,
     Refined_State => (State => (Free_List, Free_List_Contention)) is
   package STC renames Ada.Synchronous_Task_Control;

   use type Tags.Tagged_Access;
   use type Tags.Tag_Bits;
   use type Sched.Backoff_State;

   type Suspend is record
      Suspend : STC.Suspension_Object;
   end record;

   type Suspend_Access is access Suspend;

   type Node is record
      Trigger : Suspend_Access;
      Next : Node_Access_Atomics.Atomic;
   end record;

   Local_Trigger : Suspend_Access := null;
   pragma Thread_Local_Storage (Local_Trigger);

   Free_List : Node_Access_Atomics.Atomic;
   Free_List_Contention : Sched.Contention;

   procedure Enqueue (W : in out Wait_List; Trigger : Suspend_Access);
   procedure Dequeue (W : in out Wait_List; Trigger : out Suspend_Access);
   procedure Allocate (N : out Node_Access);
   procedure Deallocate (N : Node_Access);

   procedure Initialize (W : in out Wait_List) is
      N : Node_Access;
   begin
      --  Can't use the free list
      N := new Node;
      N.Next := Node_Access_Atomics.To (Tags.To (null, 0));
      W.Head := Node_Access_Atomics.To (Tags.To (N, 0));
      W.Tail := Node_Access_Atomics.To (Tags.To (N, 0));
   end Initialize;

   procedure Wait (W : in out Wait_List) is
      Is_Triggered : Default_False;
   begin
      Boolean_Atomics.Swap (W.Triggered, Is_Triggered, False);
      if Is_Triggered then
         return;
      end if;

      if null = Local_Trigger then
         Local_Trigger := new Suspend;
      end if;

      Enqueue (W, Local_Trigger);
      STC.Suspend_Until_True (Local_Trigger.Suspend);
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
      Trigger : Suspend_Access;
      Success : Boolean;
   begin
      W.Triggered := Boolean_Atomics.To (True);

      loop
         Dequeue (W, Trigger);
         exit when null = Trigger;
         STC.Set_True (Trigger.Suspend);
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Trigger : Suspend_Access;
   begin
      W.Triggered := Boolean_Atomics.To (True);

      Dequeue (W, Trigger);
      if Trigger /= null then
         STC.Set_True (Trigger.Suspend);
      end if;
   end Signal;

   procedure Enqueue (W : in out Wait_List; Trigger : Suspend_Access) is
      Node : Node_Access;
      Tail : Tags.Tagged_Access;
      Tail_Again : Tags.Tagged_Access;
      Next : Tags.Tagged_Access;
      Success : Boolean;
   begin
      Allocate (Node);
      Node.Trigger := Trigger;

      declare
         N : Tags.Tagged_Access;
      begin
         N := Node_Access_Atomics.From (Node.Next);
         Node.Next := Node_Access_Atomics.To (Tags.To (null, Tags.Tag (N)));
      end;

      loop
         loop
            Tail := Node_Access_Atomics.From (W.Tail);
            Next := Node_Access_Atomics.From (Tags.From (Tail).Next);
            Tail_Again := Node_Access_Atomics.From (W.Tail);
            exit when Tail = Tail_Again;
            Sched.Backoff (W.Tail_Contention);
         end loop;
         Sched.Success (W.Tail_Contention);

         if Tags.From (Next) = null then
            Node_Access_Atomics.Compare_And_Swap
              (Tags.From (Tail).Next,
               Next,
               Tags.To (Node, Tags.Tag (Next) + 1),
               Success);
            exit when Success;
         else
            Node_Access_Atomics.Compare_And_Swap
              (W.Tail,
               Tail,
               Tags.To (Tags.From (Next), Tags.Tag (Tail) + 1));
         end if;
	 Sched.Backoff (W.Tail_Contention);
      end loop;
      Sched.Success (W.Tail_Contention);

      Node_Access_Atomics.Compare_And_Swap
        (W.Tail,
         Tail,
         Tags.To (Node, Tags.Tag (Tail) + 1));
   end Enqueue;

   procedure Dequeue (W : in out Wait_List; Trigger : out Suspend_Access) is
      Head : Tags.Tagged_Access;
      Head_Again : Tags.Tagged_Access;
      Tail : Tags.Tagged_Access;
      Next : Tags.Tagged_Access;
      Success : Boolean;
      Dequeued : Node_Access;
   begin
      loop
         loop
            Head := Node_Access_Atomics.From (W.Head);
            Tail := Node_Access_Atomics.From (W.Tail);
            Next := Node_Access_Atomics.From (Tags.From (Head).Next);
            Head_Again := Node_Access_Atomics.From (W.Head);
            exit when Head = Head_Again;
            Sched.Backoff (W.Head_Contention);
         end loop;
         Sched.Success (W.Head_Contention);

         if Tags.From (Head) = Tags.From (Tail) then
            if Tags.From (Next) = null then
               Dequeued := null;
               Trigger := null;
               exit;
            end if;
            Node_Access_Atomics.Compare_And_Swap
              (W.Tail,
               Tail,
               Tags.To (Tags.From (Next), Tags.Tag (Tail) + 1));
         else
            Trigger := Tags.From (Next).Trigger;
            Node_Access_Atomics.Compare_And_Swap
              (W.Head,
               Head,
               Tags.To (Tags.From (Next), Tags.Tag (Head) + 1),
               Success);
	    if Success then
	       Dequeued := Tags.From (Head);
	       exit;
	    end if;
         end if;
	 Sched.Backoff (W.Head_Contention);
      end loop;
      Sched.Success (W.Head_Contention);

      if Dequeued /= null then
	 Deallocate (Dequeued);
      end if;
   end Dequeue;

   procedure Allocate (N : out Node_Access) is
      Head : Tags.Tagged_Access;
      Next : Tags.Tagged_Access;
      Success : Boolean;
   begin
      loop
         Head := Node_Access_Atomics.From (Free_List);
         if null = Tags.From (Head) then
            N := new Node;
            Sched.Success (Free_List_Contention);
            return;
         end if;
         N := Tags.From (Head);
         --  .Next is safe because nodes are never deallocated
         Next := Node_Access_Atomics.From (N.Next);
         Node_Access_Atomics.Compare_And_Swap
           (Free_List,
            Head,
            Tags.To (Tags.From (Next), Tags.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (Free_List_Contention);
      end loop;
      Sched.Success (Free_List_Contention);
      N.Next := Node_Access_Atomics.To (Tags.To (null, 0));
   end Allocate;

   procedure Deallocate (N : Node_Access) is
      Head : Tags.Tagged_Access;
      Success : Boolean;
   begin
      N.Next := Node_Access_Atomics.To (Tags.To (null, 0));
      loop
         Head := Node_Access_Atomics.From (Free_List);
         N.Next := Node_Access_Atomics.To (Tags.To (Tags.From (Head), 0));
         Node_Access_Atomics.Compare_And_Swap
           (Free_List,
            Head,
            Tags.To (N, Tags.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (Free_List_Contention);
      end loop;
      Sched.Success (Free_List_Contention);
   end Deallocate;

end Linted.Wait_Lists;
