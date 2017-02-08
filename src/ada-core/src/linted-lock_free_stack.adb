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

package body Linted.Lock_Free_Stack with
     Refined_State => (State => (Buf_Contents, Buf_Nodes, Buf_Head, Buf_Free))
is
   type My_Ix is new Ix with
        Default_Value => 0;

   package My_Atomics is new Atomics (My_Ix);

   type Element_Array is array (My_Ix range 1 .. My_Ix (Ix'Last)) of Element_T;

   Buf_Contents : Element_Array;

   type Node_Array is
     array (My_Ix range 1 .. My_Ix (Ix'Last)) of My_Atomics.Atomic;

   Buf_Nodes : Node_Array;
   Buf_Head : My_Atomics.Atomic;
   Buf_Free : My_Atomics.Atomic;

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

   procedure Push_Node (N : in out My_Atomics.Atomic; Free : My_Ix) with
      Pre => Free /= 0,
      Global => (In_Out => Buf_Nodes),
      Depends => (N => (Free, N), Buf_Nodes => (Free, Buf_Nodes, N));

   procedure Pop_Node (N : in out My_Atomics.Atomic; Head : out My_Ix) with
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

   procedure Push_Node (N : in out My_Atomics.Atomic; Free : My_Ix) is
      Head : My_Ix;
      Swapped : Boolean;
   begin
      loop
         My_Atomics.Get (N, Head);
         My_Atomics.Set (Buf_Nodes (Free), Head);
         My_Atomics.Compare_And_Swap (N, Head, Free, Swapped);
         exit when Swapped;
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

   procedure Pop_Node (N : in out My_Atomics.Atomic; Head : out My_Ix) is
      New_Head : My_Ix;
      Swapped : Boolean;
   begin
      loop
         My_Atomics.Get (N, Head);
         if Head = 0 then
            exit;
         end if;
	 My_Atomics.Get (Buf_Nodes (Head), New_Head);
	 My_Atomics.Compare_And_Swap (N, Head, New_Head, Swapped);
	 exit when Swapped;
      end loop;
   end Pop_Node;

begin
   declare
      Old_Node : My_Ix := 0;
   begin
      for II in 1 .. My_Ix (Ix'Last) loop
         Buf_Nodes (II).Set (Old_Node);
         Old_Node := II;
      end loop;
      Buf_Free.Set (Old_Node);
   end;
end Linted.Lock_Free_Stack;
