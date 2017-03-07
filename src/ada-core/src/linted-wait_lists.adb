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

with Linted.Sched;

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
   for Node'Alignment use Cache_Line_Size;

   Local_Trigger : Suspend_Access := null;
   pragma Thread_Local_Storage (Local_Trigger);

   Free_List : Node_Access_Atomics.Atomic;
   Free_List_Contention : Sched.Contention;

   procedure Enqueue (Q : in out Queue; Trigger : Suspend_Access);
   procedure Dequeue (Q : in out Queue; Trigger : out Suspend_Access);
   procedure Allocate (N : out Node_Access);
   procedure Deallocate (N : Node_Access);

   procedure Wait (W : in out Wait_List) is
      Count : Waiter_Count;
   begin
      Count :=
        Waiter_Atomics.Load (W.Waiter_Count.Value, Memory_Order_Acquire);
      loop
         if Waiter_Atomics.Compare_Exchange_Weak
             (W.Waiter_Count.Value,
              Count,
              Count - 1,
              Memory_Order_Acq_Rel,
              Memory_Order_Acquire)
         then
            Count := Count - 1;
            exit;
         end if;
         Sched.Pause;
      end loop;
      if Count >= 0 then
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

   procedure Signal (W : in out Wait_List) is
      Trigger : Suspend_Access;
      Count : Waiter_Count;
   begin
      Count :=
        Waiter_Atomics.Load (W.Waiter_Count.Value, Memory_Order_Acquire);

      loop
         if Count = 1 then
            return;
         end if;

         if Waiter_Atomics.Compare_Exchange_Weak
             (W.Waiter_Count.Value,
              Count,
              Count + 1,
              Memory_Order_Acq_Rel,
              Memory_Order_Acquire)
         then
            Count := Count + 1;
            exit;
         end if;
         Sched.Pause;
      end loop;
      if Count > 0 then
         return;
      end if;

      loop
         Dequeue (W.Q, Trigger);
         exit when Trigger /= null;
         Sched.Pause;
      end loop;

      STC.Set_True (Trigger.Suspend);
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
      Node_Access_Atomics.Store
        (Q.Head.Value,
         Tags.To (N, 0),
         Memory_Order_Relaxed);
      Node_Access_Atomics.Store
        (Q.Tail.Value,
         Tags.To (N, 0),
         Memory_Order_Relaxed);
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
            Tail :=
              Node_Access_Atomics.Load (Q.Tail.Value, Memory_Order_Relaxed);
            --  Do not load .Next before Tail is loaded
            Next :=
              Node_Access_Atomics.Load
                (Tags.From (Tail).Next,
                 Memory_Order_Acquire);
            Tail_Again :=
              Node_Access_Atomics.Load (Q.Tail.Value, Memory_Order_Acquire);
            exit when Tail = Tail_Again;
            Sched.Pause;
         end loop;

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
                (Q.Tail.Value,
                 Tail,
                 Tags.To (Tags.From (Next), Tags.Tag (Tail) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
         end if;
         Sched.Pause;
      end loop;

      Unused :=
        Node_Access_Atomics.Compare_Exchange_Strong
          (Q.Tail.Value,
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
            Head :=
              Node_Access_Atomics.Load (Q.Head.Value, Memory_Order_Relaxed);
            --  Do not load .Tail before .Head is loaded
            Tail :=
              Node_Access_Atomics.Load (Q.Tail.Value, Memory_Order_Acquire);
            Next :=
              Node_Access_Atomics.Load
                (Tags.From (Head).Next,
                 Memory_Order_Acquire);
            Head_Again := Node_Access_Atomics.Load (Q.Head.Value);
            exit when Head = Head_Again;
            Sched.Pause;
         end loop;

         if Tags.From (Head) = Tags.From (Tail) then
            if Tags.From (Next) = null then
               Dequeued := null;
               Trigger := null;
               exit;
            end if;
            Unused :=
              Node_Access_Atomics.Compare_Exchange_Weak
                (Q.Tail.Value,
                 Tail,
                 Tags.To (Tags.From (Next), Tags.Tag (Tail) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
         else
            Trigger := Tags.From (Next).Trigger;
            Success :=
              Node_Access_Atomics.Compare_Exchange_Weak
                (Q.Head.Value,
                 Head,
                 Tags.To (Tags.From (Next), Tags.Tag (Head) + 1),
                 Memory_Order_Acq_Rel,
                 Memory_Order_Acquire);
            if Success then
               Dequeued := Tags.From (Head);
               exit;
            end if;
         end if;
         Sched.Pause;
      end loop;

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
