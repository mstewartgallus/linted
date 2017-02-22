-- Copyright 2017 Steven Stewart-Gallus
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
with Linted.Atomics;
with Linted.ABAs;
with Linted.Sched;

--  Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
--  Queue Algorithms from
--  http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html
package body Linted.Lock_Free_Queue with
     Refined_State =>
     (State =>
        (Buf_Nodes,
         Buf_Contents,
         Buf_Head,
         Buf_Tail,
         Head_Contention,
         Tail_Contention,
         Free_List_Contention,
         Free_List))
is
   pragma Compile_Time_Error (Ix'Last <= 1, "Queue will be empty");
   type My_Ix is new Ix with
        Default_Value => 0;
   type Valid_Ix is new My_Ix range 1 .. My_Ix'Last with
        Default_Value => 1;

   package ABA is new ABAs (My_Ix);
   package Valid_ABA is new ABAs (Valid_Ix);
   package Aba_Atomics is new Atomics (ABA.ABA);
   package Valid_Aba_Atomics is new Atomics (Valid_ABA.ABA);

   use type ABA.ABA;
   use type ABA.Tag_T;

   use type Valid_ABA.ABA;
   use type Valid_ABA.Tag_T;

   type Element_Array is array (Valid_Ix) of Element_T with
        Independent_Components;
   type Node_Array is array (Valid_Ix) of Aba_Atomics.Atomic with
        Independent_Components;

   Buf_Contents : Element_Array;
   Buf_Nodes : Node_Array;

   Buf_Head : Valid_Aba_Atomics.Atomic;
   Buf_Tail : Valid_Aba_Atomics.Atomic;

   Head_Contention : Sched.Contention;
   Tail_Contention : Sched.Contention;

   Free_List : Aba_Atomics.Atomic;
   Free_List_Contention : Sched.Contention;

   procedure Enqueue (Node : Valid_Ix; Element : Element_T) with
      Global =>
      (In_Out => (Buf_Nodes, Buf_Contents, Buf_Tail, Tail_Contention)),
      Depends =>
      (Buf_Nodes =>+ (Buf_Tail, Node),
       Buf_Contents =>+ (Node, Element),
       Buf_Tail =>+ (Buf_Nodes, Node),
       Tail_Contention =>+ (Node, Buf_Nodes, Buf_Tail));
   procedure Try_Dequeue (Dequeued : out My_Ix; Element : out Element_T) with
      Global =>
      (Input => Buf_Contents,
       In_Out => (Buf_Nodes, Buf_Head, Buf_Tail, Head_Contention)),
      Depends =>
      (Buf_Nodes =>+ (Buf_Head, Buf_Tail),
       Buf_Head =>+ (Buf_Nodes, Buf_Tail),
       Buf_Tail =>+ (Buf_Nodes, Buf_Head),
       Head_Contention =>+ (Buf_Head, Buf_Nodes, Buf_Tail),
       Element => (Buf_Contents, Buf_Head, Buf_Nodes, Buf_Tail),
       Dequeued => (Buf_Head, Buf_Nodes, Buf_Tail));

   procedure Try_Allocate (N : out My_Ix) with
      Global => (In_Out => (Buf_Nodes, Free_List, Free_List_Contention)),
      Depends =>
      (N => (Free_List, Buf_Nodes),
       Buf_Nodes =>+ Free_List,
       Free_List =>+ (Buf_Nodes, Free_List),
       Free_List_Contention =>+ (Free_List, Buf_Nodes));

   procedure Deallocate (N : Valid_Ix) with
      Global => (In_Out => (Buf_Nodes, Free_List, Free_List_Contention)),
      Depends =>
      (Buf_Nodes =>+ (N, Free_List),
       Free_List =>+ N,
       Free_List_Contention =>+ (Free_List, N));

   procedure Try_Enqueue (Element : Element_T; Success : out Boolean) is
      Free : My_Ix;
   begin
      Try_Allocate (Free);

      if 0 = Free then
         Success := False;
         return;
      end if;

      Enqueue (Valid_Ix (Free), Element);
      Success := True;
   end Try_Enqueue;

   procedure Try_Dequeue (Element : out Element_T; Success : out Boolean) is
      Head : My_Ix;
   begin
      Try_Dequeue (Head, Element);
      if Head /= 0 then
         Success := True;
         Deallocate (Valid_Ix (Head));
      else
         declare
            Dummy : Element_T;
         begin
            Element := Dummy;
         end;
         Success := False;
      end if;
   end Try_Dequeue;

   procedure Try_Allocate (N : out My_Ix) is
      Head : ABA.ABA;
      Next : ABA.ABA;
      Success : Boolean;
   begin
      loop
         Aba_Atomics.Get (Free_List, Head);
         if 0 = ABA.Element (Head) then
            N := 0;
            Sched.Success (Free_List_Contention);
            return;
         end if;
         N := ABA.Element (Head);
         --  the next is safe because nodes are never deallocated
         Aba_Atomics.Get (Buf_Nodes (Valid_Ix (N)), Next);
         Aba_Atomics.Compare_And_Swap
           (Free_List,
            Head,
            ABA.Initialize (ABA.Element (Next), ABA.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (Free_List_Contention);
      end loop;
      Sched.Success (Free_List_Contention);
      Aba_Atomics.Set (Buf_Nodes (Valid_Ix (N)), ABA.Initialize (0, 0));
   end Try_Allocate;

   procedure Deallocate (N : Valid_Ix) is
      Head : ABA.ABA;
      Success : Boolean;
   begin
      Aba_Atomics.Set (Buf_Nodes (N), ABA.Initialize (0, 0));
      loop
         Aba_Atomics.Get (Free_List, Head);
         Aba_Atomics.Set
           (Buf_Nodes (N),
            ABA.Initialize (ABA.Element (Head), 0));
         Aba_Atomics.Compare_And_Swap
           (Free_List,
            Head,
            ABA.Initialize (My_Ix (N), ABA.Tag (Head) + 1),
            Success);
         exit when Success;
         Sched.Backoff (Free_List_Contention);
      end loop;
      Sched.Success (Free_List_Contention);
   end Deallocate;

   procedure Enqueue (Node : Valid_Ix; Element : Element_T) is
      Tail : Valid_ABA.ABA;
      Tail_Again : Valid_ABA.ABA;
      Next : ABA.ABA;
      Success : Boolean;
   begin
      Buf_Contents (Node) := Element;
      declare
         N : ABA.ABA;
      begin
         Aba_Atomics.Get (Buf_Nodes (Node), N);
         Aba_Atomics.Set (Buf_Nodes (Node), ABA.Initialize (0, ABA.Tag (N)));
      end;

      loop
         loop
            Valid_Aba_Atomics.Get (Buf_Tail, Tail);
            Aba_Atomics.Get (Buf_Nodes (Valid_ABA.Element (Tail)), Next);
            Valid_Aba_Atomics.Get (Buf_Tail, Tail_Again);
            exit when Tail = Tail_Again;
            Sched.Backoff (Tail_Contention);
         end loop;
         Sched.Success (Tail_Contention);

         if ABA.Element (Next) = 0 then
            Aba_Atomics.Compare_And_Swap
              (Buf_Nodes (Valid_ABA.Element (Tail)),
               Next,
               ABA.Initialize (My_Ix (Node), ABA.Tag (Next) + 1),
               Success);
            exit when Success;
         else
            Valid_Aba_Atomics.Compare_And_Swap
              (Buf_Tail,
               Tail,
               Valid_ABA.Initialize
                 (Valid_Ix (ABA.Element (Next)),
                  Valid_ABA.Tag (Tail) + 1));
         end if;
         Sched.Backoff (Tail_Contention);
      end loop;
      Sched.Success (Tail_Contention);

      Valid_Aba_Atomics.Compare_And_Swap
        (Buf_Tail,
         Tail,
         Valid_ABA.Initialize (Node, Valid_ABA.Tag (Tail) + 1));
   end Enqueue;

   procedure Try_Dequeue (Dequeued : out My_Ix; Element : out Element_T) is
      Head : Valid_ABA.ABA;
      Head_Again : Valid_ABA.ABA;
      Tail : Valid_ABA.ABA;
      Next : ABA.ABA;
      Success : Boolean;
   begin
      loop
         loop
            Valid_Aba_Atomics.Get (Buf_Head, Head);
            Valid_Aba_Atomics.Get (Buf_Tail, Tail);
            Aba_Atomics.Get
              (Buf_Nodes (Valid_Ix (Valid_ABA.Element (Head))),
               Next);
            Valid_Aba_Atomics.Get (Buf_Head, Head_Again);
            exit when Head = Head_Again;
            Sched.Backoff (Head_Contention);
         end loop;
         Sched.Success (Head_Contention);

         if Valid_ABA.Element (Head) = Valid_ABA.Element (Tail) then
            if ABA.Element (Next) = 0 then
               Sched.Success (Head_Contention);
               declare
                  Dummy : Element_T;
               begin
                  Element := Dummy;
               end;
               Dequeued := 0;
               return;
            end if;
            Valid_Aba_Atomics.Compare_And_Swap
              (Buf_Tail,
               Tail,
               Valid_ABA.Initialize
                 (Valid_Ix (ABA.Element (Next)),
                  Valid_ABA.Tag (Tail) + 1));
         else
            Element := Buf_Contents (Valid_Ix (ABA.Element (Next)));
            Valid_Aba_Atomics.Compare_And_Swap
              (Buf_Head,
               Head,
               Valid_ABA.Initialize
                 (Valid_Ix (ABA.Element (Next)),
                  Valid_ABA.Tag (Head) + 1),
               Success);
            exit when Success;
         end if;
         Sched.Backoff (Head_Contention);
      end loop;
      Sched.Success (Head_Contention);
      Dequeued := My_Ix (Valid_ABA.Element (Head));

      declare
         N : ABA.ABA;
      begin
         Aba_Atomics.Get (Buf_Nodes (Valid_Ix (Dequeued)), N);
         Aba_Atomics.Set
           (Buf_Nodes (Valid_Ix (Dequeued)),
            ABA.Initialize (0, ABA.Tag (N)));
      end;
   end Try_Dequeue;

begin
   for II in 1 .. Valid_Ix'Last loop
      Deallocate (II);
   end loop;

   declare
      Free : My_Ix;
      Valid_Free : Valid_Ix;
   begin
      Try_Allocate (Free);
      Valid_Free := Valid_Ix (Free);
      Valid_Aba_Atomics.Set (Buf_Head, Valid_ABA.Initialize (Valid_Free, 0));
      Valid_Aba_Atomics.Set (Buf_Tail, Valid_ABA.Initialize (Valid_Free, 0));
      Aba_Atomics.Set (Buf_Nodes (Valid_Free), ABA.Initialize (0, 0));
   end;
end Linted.Lock_Free_Queue;
