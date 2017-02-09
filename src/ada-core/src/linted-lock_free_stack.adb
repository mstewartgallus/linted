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
with Interfaces;

with Libc.Sched;
with Linted.Atomics;

package body Linted.Lock_Free_Stack with
     Refined_State => (State => (Buf_Contents, Buf_Nodes, Buf_Head, Buf_Free))
is
   use type Interfaces.Unsigned_32;

   type My_Ix is new Ix with
        Default_Value => 0;
   type ABA is mod 2**16 with
        Default_Value => 0;
   subtype U32 is Interfaces.Unsigned_32;

   type ABA_Ix is new U32 with
        Default_Value => 0,
        Dynamic_Predicate => U32 (ABA_Ix and 16#FFFF#) <= U32 (My_Ix'Last);

   function Make_ABA_Ix (My_Index : My_Ix; My_Tag : ABA) return ABA_Ix;
   function Index (X : ABA_Ix) return My_Ix;
   function Tag (X : ABA_Ix) return ABA;

   package Aba_Atomics is new Atomics (ABA_Ix);
   package Access_Atomics is new Atomics (My_Ix);

   type Element_Array is array (My_Ix range 1 .. My_Ix (Ix'Last)) of Element_T;

   Buf_Contents : Element_Array;

   type Node_Array is
     array (My_Ix range 1 .. My_Ix (Ix'Last)) of Access_Atomics.Atomic;

   Buf_Nodes : Node_Array;
   Buf_Head : Aba_Atomics.Atomic;
   Buf_Free : Aba_Atomics.Atomic;

   function Make_ABA_Ix (My_Index : My_Ix; My_Tag : ABA) return ABA_Ix is
   begin
      return ABA_Ix (My_Index) or
        ABA_Ix (Interfaces.Shift_Left (Interfaces.Unsigned_32 (My_Tag), 16));
   end Make_ABA_Ix;

   function Index (X : ABA_Ix) return My_Ix is
   begin
      return My_Ix (X and 16#FFFF#);
   end Index;

   function Tag (X : ABA_Ix) return ABA is
   begin
      return ABA (Interfaces.Shift_Right (Interfaces.Unsigned_32 (X), 16));
   end Tag;

   procedure Allocate (Free : out My_Ix) with
      Global => (In_Out => (Buf_Nodes, Buf_Free)),
      Depends =>
      (Free => (Buf_Free, Buf_Nodes),
       Buf_Nodes => (Buf_Nodes, Buf_Free),
       Buf_Free => (Buf_Nodes, Buf_Free));

   procedure Deallocate (Head : My_Ix) with
      Pre => Head /= 0,
      Global => (In_Out => (Buf_Nodes, Buf_Free)),
      Depends =>
      (Buf_Nodes => (Buf_Nodes, Buf_Free, Head),
       Buf_Free => (Buf_Free, Head));

   procedure Push_Node (N : in out Aba_Atomics.Atomic; Free : My_Ix) with
      Pre => Free /= 0,
      Global => (In_Out => Buf_Nodes),
      Depends => (N => (Free, N), Buf_Nodes => (Free, Buf_Nodes, N));

   procedure Pop_Node (N : in out Aba_Atomics.Atomic; Head : out My_Ix) with
      Global => (In_Out => Buf_Nodes),
      Depends =>
      (Head => (Buf_Nodes, N),
       N => (Buf_Nodes, N),
       Buf_Nodes => (N, Buf_Nodes));

   procedure Allocate (Free : out My_Ix) is
   begin
      Pop_Node (Buf_Free, Free);
   end Allocate;

   procedure Deallocate (Head : My_Ix) is
   begin
      Push_Node (Buf_Free, Head);
   end Deallocate;

   procedure Try_Push (Element : Element_T; Success : out Boolean) with
      Refined_Global =>
      (In_Out => (Buf_Contents, Buf_Nodes, Buf_Head, Buf_Free))
   is
      Free : My_Ix;
   begin
      Allocate (Free);

      if 0 = Free then
         Success := False;
      else
         Buf_Contents (Free) := Element;
         Push_Node (Buf_Head, Free);
         Success := True;
      end if;
   end Try_Push;

   procedure Push_Node (N : in out Aba_Atomics.Atomic; Free : My_Ix) is
      Head : ABA_Ix;
      Swapped : Boolean;
   begin
      loop
         Aba_Atomics.Get (N, Head);
         Access_Atomics.Set (Buf_Nodes (Free), Index (Head));
         Aba_Atomics.Compare_And_Swap
           (N,
            Head,
            Make_ABA_Ix (Free, Tag (Head) + 1),
            Swapped);
         exit when Swapped;
         Libc.Sched.sched_yield;
      end loop;
   end Push_Node;

   procedure Try_Pop (Element : out Element_T; Success : out Boolean) with
      Refined_Global =>
      (In_Out => (Buf_Contents, Buf_Nodes, Buf_Head, Buf_Free))
   is
      Head : My_Ix;
   begin
      Pop_Node (Buf_Head, Head);

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

   procedure Pop_Node (N : in out Aba_Atomics.Atomic; Head : out My_Ix) is
      New_Head : My_Ix;
      Swapped : Boolean;
      ABA_Head : ABA_Ix;
   begin
      loop
         Aba_Atomics.Get (N, ABA_Head);
         if Index (ABA_Head) = 0 then
            exit;
         end if;
         Access_Atomics.Get (Buf_Nodes (Index (ABA_Head)), New_Head);
         Aba_Atomics.Compare_And_Swap
           (N,
            ABA_Head,
            Make_ABA_Ix (New_Head, Tag (ABA_Head) + 1),
            Swapped);
         exit when Swapped;
         Libc.Sched.sched_yield;
      end loop;
      Head := Index (ABA_Head);
   end Pop_Node;

begin
   declare
      Old_Node : My_Ix := 0;
   begin
      for II in 1 .. My_Ix (Ix'Last) loop
         Buf_Nodes (II).Set (Old_Node);
         Old_Node := II;
      end loop;
      Buf_Free.Set (Make_ABA_Ix (Old_Node, 0));
   end;
end Linted.Lock_Free_Stack;
