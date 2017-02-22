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

package body Linted.Lock_Free_Stack with
     Refined_State =>
     (State =>
        (Buf_Contents,
         Buf_Nodes,
         Buf_Head,
         Buf_Free,
         Head_Contention,
         Free_Contention))
is

   type My_Ix is new Ix with
        Default_Value => 0;

   package ABA is new ABAs (My_Ix);
   package Aba_Atomics is new Atomics (ABA.ABA);
   package Access_Atomics is new Atomics (My_Ix);

   use type ABA.Tag_T;

   type Element_Array is
     array (My_Ix range 1 .. My_Ix (Ix'Last)) of Element_T with
        Independent_Components;

   Buf_Contents : Element_Array;

   type Node_Array is
     array (My_Ix range 1 .. My_Ix (Ix'Last)) of Access_Atomics.Atomic with
        Independent_Components;

   Buf_Nodes : Node_Array;

   Buf_Head : Aba_Atomics.Atomic;
   Head_Contention : Sched.Contention;

   Buf_Free : Aba_Atomics.Atomic;
   Free_Contention : Sched.Contention;

   procedure Allocate (Free : out My_Ix) with
      Global => (In_Out => (Buf_Nodes, Buf_Free, Free_Contention)),
      Depends =>
      (Free => (Buf_Free, Buf_Nodes),
       Buf_Nodes => (Buf_Nodes, Buf_Free),
       Free_Contention => (Buf_Free, Buf_Nodes, Free_Contention),
       Buf_Free => (Buf_Nodes, Buf_Free));

   procedure Deallocate (Head : My_Ix) with
      Pre => Head /= 0,
      Global => (In_Out => (Buf_Nodes, Buf_Free, Free_Contention)),
      Depends =>
      (Buf_Nodes => (Buf_Nodes, Buf_Free, Head),
       Free_Contention => (Buf_Free, Head, Free_Contention),
       Buf_Free => (Buf_Free, Head));

   procedure Push_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Free : My_Ix) with
      Pre => Free /= 0,
      Global => (In_Out => Buf_Nodes),
      Depends =>
      (N => (Free, N),
       Contention => (Free, N, Contention),
       Buf_Nodes => (Free, Buf_Nodes, N));

   procedure Pop_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Head : out My_Ix) with
      Global => (In_Out => Buf_Nodes),
      Depends =>
      (Head => (Buf_Nodes, N),
       Contention => (N, Buf_Nodes, Contention),
       N => (Buf_Nodes, N),
       Buf_Nodes => (N, Buf_Nodes));

   procedure Allocate (Free : out My_Ix) is
   begin
      Pop_Node (Buf_Free, Free_Contention, Free);
   end Allocate;

   procedure Deallocate (Head : My_Ix) is
   begin
      Push_Node (Buf_Free, Free_Contention, Head);
   end Deallocate;

   procedure Try_Push (Element : Element_T; Success : out Boolean) with
      Refined_Global =>
      (In_Out =>
         (Buf_Contents,
          Buf_Nodes,
          Buf_Head,
          Buf_Free,
          Head_Contention,
          Free_Contention))
   is
      Free : My_Ix;
   begin
      Allocate (Free);

      if 0 = Free then
         Success := False;
      else
         Buf_Contents (Free) := Element;
         Push_Node (Buf_Head, Head_Contention, Free);
         Success := True;
      end if;
   end Try_Push;

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
         Access_Atomics.Set (Buf_Nodes (Free), ABA.Element (Head));
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

   procedure Try_Pop (Element : out Element_T; Success : out Boolean) with
      Refined_Global =>
      (In_Out =>
         (Buf_Contents,
          Buf_Nodes,
          Buf_Head,
          Buf_Free,
          Head_Contention,
          Free_Contention))
   is
      Head : My_Ix;
   begin
      Pop_Node (Buf_Head, Head_Contention, Head);

      if 0 = Head then
         declare
            Dummy : Element_T;
         begin
            Element := Dummy;
         end;
         Success := False;
      else
         Element := Buf_Contents (Head);
         declare
            Dummy : Element_T;
         begin
            Buf_Contents (Head) := Dummy;
         end;

         Deallocate (Head);
         Success := True;
      end if;
   end Try_Pop;

   procedure Pop_Node
     (N : in out Aba_Atomics.Atomic;
      Contention : in out Sched.Contention;
      Head : out My_Ix)
   is
      New_Head : My_Ix;
      Swapped : Boolean;
      ABA_Head : ABA.ABA;
   begin
      loop
         Aba_Atomics.Get (N, ABA_Head);
         if ABA.Element (ABA_Head) = 0 then
            exit;
         end if;
         Access_Atomics.Get (Buf_Nodes (ABA.Element (ABA_Head)), New_Head);
         Aba_Atomics.Compare_And_Swap
           (N,
            ABA_Head,
            ABA.Initialize (New_Head, ABA.Tag (ABA_Head) + 1),
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
         Buf_Nodes (II).Set (Old_Node);
         Old_Node := II;
      end loop;
      Buf_Free.Set (ABA.Initialize (Old_Node, 0));
   end;
end Linted.Lock_Free_Stack;
