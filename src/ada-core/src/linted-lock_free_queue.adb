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
        (Buf_Contents,
         Buf_Nodes,
         Buf_Head,
         Buf_Tail,
         Buf_Free,
         Head_Contention,
         Tail_Contention,
         Free_Contention))
is

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

   Buf_Nodes : Node_Array;

   Buf_Head : Aba_Atomics.Atomic;
   Buf_Tail : Aba_Atomics.Atomic;

   Head_Contention : Sched.Contention;
   Tail_Contention : Sched.Contention;

   Buf_Free : Aba_Atomics.Atomic;
   Free_Contention : Sched.Contention;

   procedure Allocate (Free : out My_Ix) with
      Global =>
      (Input => Ada.Real_Time.Clock_Time,
       In_Out => (Buf_Nodes, Buf_Free, Free_Contention)),
      Depends =>
      (Free => (Buf_Free, Buf_Nodes),
       Buf_Nodes => (Buf_Nodes, Buf_Free),
       Free_Contention => (Buf_Free, Buf_Nodes, Free_Contention),
       Buf_Free => (Buf_Nodes, Buf_Free),
       null => Ada.Real_Time.Clock_Time);

   procedure Deallocate (Head : My_Ix) with
      Pre => Head /= 0,
      Global =>
      (Input => Ada.Real_Time.Clock_Time,
       In_Out => (Buf_Nodes, Buf_Free, Free_Contention)),
      Depends =>
      (Buf_Nodes => (Buf_Nodes, Buf_Free, Head),
       Free_Contention => (Buf_Free, Head, Free_Contention),
       Buf_Free => (Buf_Free, Head),
       null => Ada.Real_Time.Clock_Time);

   procedure Push_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Free : My_Ix) with
      Pre => Free /= 0,
      Global => (Input => Ada.Real_Time.Clock_Time, In_Out => Buf_Nodes),
      Depends =>
      (N => (Free, N),
       Contention => (Free, N, Contention),
       Buf_Nodes => (Free, Buf_Nodes, N),
       null => Ada.Real_Time.Clock_Time);

   procedure Pop_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Head : out My_Ix) with
      Global => (Input => Ada.Real_Time.Clock_Time, In_Out => Buf_Nodes),
      Depends =>
      (Head => (Buf_Nodes, N),
       Contention => (N, Buf_Nodes, Contention),
       N => (Buf_Nodes, N),
       Buf_Nodes => (N, Buf_Nodes),
       null => Ada.Real_Time.Clock_Time);

   procedure Allocate (Free : out My_Ix) is
   begin
      Pop_Node (Buf_Free, Free_Contention, Free);
   end Allocate;

   procedure Deallocate (Head : My_Ix) is
   begin
      Push_Node (Buf_Free, Free_Contention, Head);
   end Deallocate;

   procedure Try_Enqueue (Element : Element_T; Success : out Boolean) with
      Refined_Global =>
      (Input => Ada.Real_Time.Clock_Time,
       In_Out =>
         (Buf_Contents,
          Buf_Nodes,
          Buf_Free,
          Buf_Tail,
          Tail_Contention,
          Free_Contention))
   is
      Free : My_Ix;
      Tail : ABA.ABA;
      Tail_Again : ABA.ABA;
      Next : ABA.ABA;
   begin
      Allocate (Free);

      if 0 = Free then
         Success := False;
         return;
      end if;

      Buf_Contents (Free) := Element;
      declare
         Next : ABA.ABA;
      begin
         Aba_Atomics.Get (Buf_Nodes (Free), Next);
         Aba_Atomics.Set
           (Buf_Nodes (Free),
            ABA.Initialize (0, ABA.Tag (Next)));
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
               ABA.Initialize (Free, ABA.Tag (Next) + 1),
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
         ABA.Initialize (Free, ABA.Tag (Tail) + 1));
      Success := True;
   end Try_Enqueue;

   procedure Try_Dequeue (Element : out Element_T; Success : out Boolean) with
      Refined_Global =>
      (Input => (Ada.Real_Time.Clock_Time, Buf_Contents),
       In_Out =>
         (Buf_Nodes,
          Buf_Head,
          Buf_Tail,
          Buf_Free,
          Head_Contention,
          Free_Contention))
   is
      Head : ABA.ABA;
      Head_Again : ABA.ABA;
      Tail : ABA.ABA;
      Next : ABA.ABA;
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
               declare
                  Dummy : Element_T;
               begin
                  Element := Dummy;
               end;
               Success := False;
               return;
            end if;
            Aba_Atomics.Compare_And_Swap
              (Buf_Tail,
               Tail,
               ABA.Initialize (ABA.Element (Next), ABA.Tag (Tail) + 1));
         else
            Element := Buf_Contents (ABA.Element (Next));
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
      Deallocate (ABA.Element (Head));
      Success := True;
   end Try_Dequeue;

   procedure Push_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Free : My_Ix)
   is
      Head : ABA.ABA;
      Swapped : Boolean;
   begin
      loop
         Aba_Atomics.Get (N, Head);
         Aba_Atomics.Set
           (Buf_Nodes (Free),
            ABA.Initialize (ABA.Element (Head), 0));
         Aba_Atomics.Compare_And_Swap
           (N,
            Head,
            ABA.Initialize (Free, ABA.Tag (Head) + 1),
            Swapped);
         exit when Swapped;
         Sched.Backoff (Contention);
      end loop;
      Sched.Success (Contention);
   end Push_Node;

   procedure Pop_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Head : out My_Ix)
   is
      New_Head : ABA.ABA;
      Swapped : Boolean;
      ABA_Head : ABA.ABA;
   begin
      loop
         Aba_Atomics.Get (N, ABA_Head);
         if ABA.Element (ABA_Head) = 0 then
            exit;
         end if;
         Aba_Atomics.Get (Buf_Nodes (ABA.Element (ABA_Head)), New_Head);
         Aba_Atomics.Compare_And_Swap
           (N,
            ABA_Head,
            ABA.Initialize (ABA.Element (New_Head), ABA.Tag (ABA_Head) + 1),
            Swapped);
         exit when Swapped;
         Sched.Backoff (Contention);
      end loop;
      Sched.Success (Contention);
      Head := ABA.Element (ABA_Head);
   end Pop_Node;

begin
   declare
      Old_Node : My_Ix := 0;
   begin
      for II in 1 .. My_Ix (Ix'Last) loop
         Buf_Nodes (II).Set (ABA.Initialize (Old_Node, 0));
         Old_Node := II;
      end loop;
      Buf_Free.Set (ABA.Initialize (Old_Node, 0));
   end;

   declare
      Free : My_Ix;
   begin
      Allocate (Free);
      Aba_Atomics.Set (Buf_Head, ABA.Initialize (Free, 0));
      Aba_Atomics.Set (Buf_Tail, ABA.Initialize (Free, 0));
      Aba_Atomics.Set (Buf_Nodes (Free), ABA.Initialize (0, 0));
   end;
end Linted.Lock_Free_Queue;
