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
with Linted.Lock_Free_Stack;
--  Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
--  Queue Algorithms from
--  http://www.cs.rochester.edu/research/synchronization/pseudocode/queues.html
package body Linted.Lock_Free_Queue with
     Refined_State => (State => (Buf_Contents, Tip.State, Free_List.State)) is
   pragma Compile_Time_Error (Ix'Last <= 1, "Queue will be empty");
   type My_Ix is new Ix with
        Default_Value => 0;

   package ABA is new ABAs (My_Ix);
   package Aba_Atomics is new Atomics (ABA.ABA);

   use type ABA.ABA;
   use type ABA.Tag_T;

   type Element_Array is array (My_Ix range 1 .. My_Ix (Ix'Last)) of Element_T;

   Buf_Contents : Element_Array;

   type Node_Array is
     array (My_Ix range 1 .. My_Ix (Ix'Last)) of Aba_Atomics.Atomic;

   package Tip with
        Abstract_State => (State with External) is
      procedure Enqueue (Node : My_Ix) with
         Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (State)),
         Depends => (State => (State, Node), null => Ada.Real_Time.Clock_Time);
      procedure Try_Dequeue (Dequeued : out My_Ix) with
         Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (State)),
         Depends =>
         (Dequeued => (State),
          State => (State),
          null => Ada.Real_Time.Clock_Time);
   end Tip;

   package Free_List with
        Abstract_State => (State with External) is
      procedure Allocate (Head : out My_Ix) with
         Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (State)),
         Depends =>
         (Head => (State),
          State => (State),
          null => Ada.Real_Time.Clock_Time);

      procedure Deallocate (Head : My_Ix) with
         Pre => Head /= 0,
         Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (State)),
         Depends => (State => (State, Head), null => Ada.Real_Time.Clock_Time);
   end Free_List;

   procedure Try_Enqueue (Element : Element_T; Success : out Boolean) is
      Free : My_Ix;
   begin
      Free_List.Allocate (Free);

      if 0 = Free then
         Success := False;
         return;
      end if;

      Buf_Contents (Free) := Element;
      Tip.Enqueue (Free);
      Success := True;
   end Try_Enqueue;

   procedure Try_Dequeue (Element : out Element_T; Success : out Boolean) is
      Head : My_Ix;
   begin
      Tip.Try_Dequeue (Head);
      if Head /= 0 then
         Element := Buf_Contents (Head);
         declare
            Dummy : Element_T;
         begin
            Buf_Contents (Head) := Dummy;
         end;
         Success := True;
         Free_List.Deallocate (Head);
      else
         declare
            Dummy : Element_T;
         begin
            Element := Dummy;
         end;
         Success := False;
      end if;
   end Try_Dequeue;

   package body Free_List with
        Refined_State => (State => (Stack.State)) is
      function Is_Valid (M : My_Ix) return Boolean is (True);
      package Stack is new Lock_Free_Stack (My_Ix, My_Ix, Is_Valid);

      procedure Allocate (Head : out My_Ix) is
         Success : Boolean;
      begin
         Stack.Try_Pop (Head, Success);
         if not Success then
            Head := 0;
         end if;
      end Allocate;

      procedure Deallocate (Head : My_Ix) is
         Success : Boolean;
      begin
         Stack.Try_Push (Head, Success);
         pragma Assert (Success);
      end Deallocate;

   begin
      for II in 1 .. My_Ix (Ix'Last) loop
         Deallocate (II);
      end loop;
   end Free_List;

   package body Tip with
        Refined_State =>
        (State =>
           (Buf_Nodes, Buf_Head, Buf_Tail, Head_Contention, Tail_Contention))
   is
      Buf_Nodes : Node_Array;

      Buf_Head : Aba_Atomics.Atomic;
      Buf_Tail : Aba_Atomics.Atomic;

      Head_Contention : Sched.Contention;
      Tail_Contention : Sched.Contention;

      procedure Enqueue (Node : My_Ix) is
         Tail : ABA.ABA;
         Tail_Again : ABA.ABA;
         Next : ABA.ABA;
         Success : Boolean;
      begin
         declare
            N : ABA.ABA;
         begin
            Aba_Atomics.Get (Buf_Nodes (Node), N);
            Aba_Atomics.Set
              (Buf_Nodes (Node),
               ABA.Initialize (0, ABA.Tag (N)));
         end;

         loop
            loop
               Aba_Atomics.Get (Buf_Tail, Tail);
               Aba_Atomics.Get (Buf_Nodes (ABA.Element (Tail)), Next);
               Aba_Atomics.Get (Buf_Tail, Tail_Again);
               exit when Tail = Tail_Again;
               Sched.Backoff (Tail_Contention);
            end loop;
            Sched.Success (Tail_Contention);

            if ABA.Element (Next) = 0 then
               Aba_Atomics.Compare_And_Swap
                 (Buf_Nodes (ABA.Element (Tail)),
                  Next,
                  ABA.Initialize (Node, ABA.Tag (Next) + 1),
                  Success);
               exit when Success;
               Sched.Backoff (Tail_Contention);
            else
               Aba_Atomics.Compare_And_Swap
                 (Buf_Tail,
                  Tail,
                  ABA.Initialize (ABA.Element (Next), ABA.Tag (Tail) + 1));
            end if;
         end loop;
         Sched.Success (Tail_Contention);

         Aba_Atomics.Compare_And_Swap
           (Buf_Tail,
            Tail,
            ABA.Initialize (Node, ABA.Tag (Tail) + 1));
      end Enqueue;

      procedure Try_Dequeue (Dequeued : out My_Ix) is
         Head : ABA.ABA;
         Head_Again : ABA.ABA;
         Tail : ABA.ABA;
         Next : ABA.ABA;
         Success : Boolean;
      begin
         loop
            loop
               Aba_Atomics.Get (Buf_Head, Head);
               Aba_Atomics.Get (Buf_Tail, Tail);
               Aba_Atomics.Get (Buf_Nodes (ABA.Element (Head)), Next);
               Aba_Atomics.Get (Buf_Head, Head_Again);
               exit when Head = Head_Again;
               Sched.Backoff (Head_Contention);
            end loop;
            Sched.Success (Head_Contention);

            if ABA.Element (Head) = ABA.Element (Tail) then
               if ABA.Element (Next) = 0 then
                  Sched.Success (Head_Contention);
                  Dequeued := 0;
                  return;
               end if;
               Aba_Atomics.Compare_And_Swap
                 (Buf_Tail,
                  Tail,
                  ABA.Initialize (ABA.Element (Next), ABA.Tag (Tail) + 1));
            else
               Aba_Atomics.Compare_And_Swap
                 (Buf_Head,
                  Head,
                  ABA.Initialize (ABA.Element (Next), ABA.Tag (Head) + 1),
                  Success);
               exit when Success;
               Sched.Backoff (Head_Contention);
            end if;
         end loop;
         Sched.Success (Head_Contention);
         Dequeued := ABA.Element (Next);
      end Try_Dequeue;

   begin
      declare
         Free : My_Ix;
      begin
         Free_List.Allocate (Free);
         Aba_Atomics.Set (Buf_Head, ABA.Initialize (Free, 0));
         Aba_Atomics.Set (Buf_Tail, ABA.Initialize (Free, 0));
         Aba_Atomics.Set (Buf_Nodes (Free), ABA.Initialize (0, 0));
      end;
   end Tip;
end Linted.Lock_Free_Queue;
