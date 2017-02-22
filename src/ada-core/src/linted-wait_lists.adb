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

   use Linted.GCC_Atomics;
   use type Tags.Tagged_Access;
   use type Tags.Tag_Bits;
   use type Sched.Backoff_State;

   type Suspend is record
      Suspend : STC.Suspension_Object;
   end record;

   type Suspend_Access is access Suspend;

   type Node is record
      Trigger : Suspend_Access with
         Independent;
      Next : Node_Access_Atomics.Atomic;
   end record;

   Local_Trigger : Suspend_Access := null;
   pragma Thread_Local_Storage (Local_Trigger);

   Free_List : Node_Access_Atomics.Atomic;
   Free_List_Contention : Sched.Contention;

   procedure Enqueue (Q : in out Queue; Trigger : Suspend_Access);
   procedure Dequeue (Q : in out Queue; Trigger : out Suspend_Access);
   procedure Allocate (N : out Node_Access);
   procedure Deallocate (N : Node_Access);

   procedure Wait (W : in out Wait_List) is
      Is_Triggered : Default_False;
   begin
      Is_Triggered :=
        Boolean_Atomics.Exchange (W.Triggered, False, Memory_Order_Acq_Rel);
      if Is_Triggered then
         return;
      end if;

      if null = Local_Trigger then
         --  In this case allocation is okay because it is bounded by
         --  the number of tasks
         pragma Warnings (Off);
         Local_Trigger := new Suspend;
         pragma Warnings (On);
      end if;

      Enqueue (W.Q, Local_Trigger);
      STC.Suspend_Until_True (Local_Trigger.Suspend);
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
      Trigger : Suspend_Access;
   begin
      Boolean_Atomics.Store (W.Triggered, True, Memory_Order_Release);

      loop
         Dequeue (W.Q, Trigger);
         exit when null = Trigger;
         STC.Set_True (Trigger.Suspend);
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Trigger : Suspend_Access;
   begin
      Boolean_Atomics.Store (W.Triggered, True, Memory_Order_Release);

      Dequeue (W.Q, Trigger);
      if Trigger /= null then
         STC.Set_True (Trigger.Suspend);
      end if;
   end Signal;

   procedure Initialize (Q : in out Queue) is
      N : Node_Access;
   begin
      --  Can't use the free list. Allocation is bounded by the number
      --  of Wait_Lists
      pragma Warnings (Off);
      N := new Node;
      pragma Warnings (On);
      Node_Access_Atomics.Store
        (N.Next,
         Tags.To (null, 0),
         Memory_Order_Relaxed);
      Node_Access_Atomics.Store (Q.Head, Tags.To (N, 0), Memory_Order_Relaxed);
      Node_Access_Atomics.Store (Q.Tail, Tags.To (N, 0), Memory_Order_Relaxed);
   end Initialize;

   procedure Enqueue (Q : in out Queue; Trigger : Suspend_Access) is
      Node : Node_Access;
      Tail : Tags.Tagged_Access;
      Tail_Again : Tags.Tagged_Access;
      Next : Tags.Tagged_Access;
      Success : Boolean;
      Unused : Boolean;
   begin
      Allocate (Node);
      Node.Trigger := Trigger;

      declare
         N : Tags.Tagged_Access;
      begin
         N := Node_Access_Atomics.Load (Node.Next, Memory_Order_Acquire);
         Node_Access_Atomics.Store
           (Node.Next,
            Tags.To (null, Tags.Tag (N)),
            Memory_Order_Release);
      end;

      loop
         loop
            Tail := Node_Access_Atomics.Load (Q.Tail, Memory_Order_Relaxed);
            --  Do not load .Next before Tail is loaded
            Next :=
              Node_Access_Atomics.Load
                (Tags.From (Tail).Next,
                 Memory_Order_Acquire);
            Tail_Again :=
              Node_Access_Atomics.Load (Q.Tail, Memory_Order_Acquire);
            exit when Tail = Tail_Again;
            Sched.Backoff (Q.Tail_Contention);
         end loop;
         Sched.Success (Q.Tail_Contention);

         if Tags.From (Next) = null then
            Success :=
              Node_Access_Atomics.Compare_Exchange_Weak
                (Tags.From (Tail).Next,
                 Next,
                 Tags.To (Node, Tags.Tag (Next) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
            exit when Success;
         else
            Unused :=
              Node_Access_Atomics.Compare_Exchange_Weak
                (Q.Tail,
                 Tail,
                 Tags.To (Tags.From (Next), Tags.Tag (Tail) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
         end if;
         Sched.Backoff (Q.Tail_Contention);
      end loop;
      Sched.Success (Q.Tail_Contention);

      Unused :=
        Node_Access_Atomics.Compare_Exchange_Strong
          (Q.Tail,
           Tail,
           Tags.To (Node, Tags.Tag (Tail) + 1),
           Memory_Order_Acq_Rel,
           Memory_Order_Acquire);
   end Enqueue;

   procedure Dequeue (Q : in out Queue; Trigger : out Suspend_Access) is
      Head : Tags.Tagged_Access;
      Head_Again : Tags.Tagged_Access;
      Tail : Tags.Tagged_Access;
      Next : Tags.Tagged_Access;
      Success : Boolean;
      Dequeued : Node_Access;
      Unused : Boolean;
   begin
      loop
         loop
            Head := Node_Access_Atomics.Load (Q.Head, Memory_Order_Relaxed);
            --  Do not load .Tail before .Head is loaded
            Tail := Node_Access_Atomics.Load (Q.Tail, Memory_Order_Acquire);
            Next :=
              Node_Access_Atomics.Load
                (Tags.From (Head).Next,
                 Memory_Order_Acquire);
            Head_Again := Node_Access_Atomics.Load (Q.Head);
            exit when Head = Head_Again;
            Sched.Backoff (Q.Head_Contention);
         end loop;
         Sched.Success (Q.Head_Contention);

         if Tags.From (Head) = Tags.From (Tail) then
            if Tags.From (Next) = null then
               Dequeued := null;
               Trigger := null;
               exit;
            end if;
            Unused :=
              Node_Access_Atomics.Compare_Exchange_Weak
                (Q.Tail,
                 Tail,
                 Tags.To (Tags.From (Next), Tags.Tag (Tail) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
         else
            Trigger := Tags.From (Next).Trigger;
            Success :=
              Node_Access_Atomics.Compare_Exchange_Weak
                (Q.Head,
                 Head,
                 Tags.To (Tags.From (Next), Tags.Tag (Head) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
            if Success then
               Dequeued := Tags.From (Head);
               exit;
            end if;
         end if;
         Sched.Backoff (Q.Head_Contention);
      end loop;
      Sched.Success (Q.Head_Contention);

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
         Head := Node_Access_Atomics.Load (Free_List);
         if null = Tags.From (Head) then
            --  Allocation is bounded by the number of tasks
            pragma Warnings (Off);
            N := new Node;
            pragma Warnings (On);
            Sched.Success (Free_List_Contention);
            return;
         end if;
         N := Tags.From (Head);
         --  .Next is safe because nodes are never deallocated
         Next := Node_Access_Atomics.Load (N.Next);
         Success :=
           Node_Access_Atomics.Compare_Exchange_Strong
             (Free_List,
              Head,
              Tags.To (Tags.From (Next), Tags.Tag (Head) + 1));
         exit when Success;
         Sched.Backoff (Free_List_Contention);
      end loop;
      Sched.Success (Free_List_Contention);
      Node_Access_Atomics.Store (N.Next, Tags.To (null, 0));
   end Allocate;

   procedure Deallocate (N : Node_Access) is
      Head : Tags.Tagged_Access;
      Success : Boolean;
   begin
      Node_Access_Atomics.Store (N.Next, Tags.To (null, 0));
      loop
         Head := Node_Access_Atomics.Load (Free_List);
         Node_Access_Atomics.Store (N.Next, Tags.To (Tags.From (Head), 0));
         Success :=
           Node_Access_Atomics.Compare_Exchange_Strong
             (Free_List,
              Head,
              Tags.To (N, Tags.Tag (Head) + 1));
         exit when Success;
         Sched.Backoff (Free_List_Contention);
      end loop;
      Sched.Success (Free_List_Contention);
   end Deallocate;
end Linted.Wait_Lists;
