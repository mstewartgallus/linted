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
package body Linted.Stack with
     Refined_State => (State => (Buf_Contents, Buf_Nodes, Buf_Head, Buf_Free))
is

   subtype My_Ix is Natural range 0 .. Natural (Ix'Last);

   type Element_Array is array (My_Ix range 1 .. My_Ix (Ix'Last)) of Element_T;

   Buf_Contents : Element_Array;

   protected type Node_Access with
      Lock_Free is
      procedure Set (New_Ptr : My_Ix) with
         Global => null,
         Depends => (Node_Access => New_Ptr, null => Node_Access);
      function Get return My_Ix with
         Global => null,
         Depends => (Get'Result => Node_Access);
      procedure Compare_And_Swap
        (Old_Ptr : My_Ix;
         New_Ptr : My_Ix;
         Success : in out Boolean) with
         Global => null,
         Depends =>
         (Success => (Old_Ptr, Node_Access),
          Node_Access => (Node_Access, Old_Ptr, New_Ptr),
          null => Success);
   private
      Ptr : My_Ix := 0;
   end Node_Access;

   type Node_Array is array (My_Ix range 1 .. My_Ix (Ix'Last)) of Node_Access;

   Buf_Nodes : Node_Array;
   Buf_Head : Node_Access;
   Buf_Free : Node_Access;

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

   procedure Enqueue_Node (N : in out Node_Access; Free : My_Ix) with
      Global => (In_Out => Buf_Nodes),
      Depends => (N => (Free, N), Buf_Nodes => (Free, Buf_Nodes, N));

   procedure Dequeue_Node (N : in out Node_Access; Head : out My_Ix) with
      Global => (In_Out => Buf_Nodes),
      Depends =>
      (Head => (Buf_Nodes, N),
       N => (Buf_Nodes, N),
       Buf_Nodes => (N, Buf_Nodes));

   protected body Node_Access is
      procedure Set (New_Ptr : My_Ix) is
      begin
         Ptr := New_Ptr;
      end Set;

      function Get return My_Ix is
      begin
         return Ptr;
      end Get;

      procedure Compare_And_Swap
        (Old_Ptr : My_Ix;
         New_Ptr : My_Ix;
         Success : in out Boolean)
      is
      begin
         if Old_Ptr = Ptr then
            Ptr := New_Ptr;
            Success := True;
         else
            Success := False;
         end if;
      end Compare_And_Swap;
   end Node_Access;

   procedure Allocate (Free : out My_Ix) is
   begin
      Dequeue_Node (Buf_Free, Free);
   end Allocate;

   procedure Deallocate (Head : My_Ix) is
   begin
      Enqueue_Node (Buf_Free, Head);
   end Deallocate;

   procedure Try_Enqueue (Element : Element_T; Success : out Boolean) is
      Free : My_Ix;
   begin
      Allocate (Free);

      if 0 = Free then
         Success := False;
      else
         Buf_Contents (Free) := Element;
         Enqueue_Node (Buf_Head, Free);
         Success := True;
      end if;
   end Try_Enqueue;

   procedure Enqueue_Node (N : in out Node_Access; Free : My_Ix) is
      Head : My_Ix;
      Swapped : Boolean := False;
   begin
      loop
         Head := N.Get;
         Buf_Nodes (Free).Set (Head);
         N.Compare_And_Swap (Head, Free, Swapped);
         exit when Swapped;
      end loop;
   end Enqueue_Node;

   procedure Try_Dequeue (Element : out Element_T; Success : out Boolean) is
      Head : My_Ix;
   begin
      Dequeue_Node (Buf_Head, Head);

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
   end Try_Dequeue;

   procedure Dequeue_Node (N : in out Node_Access; Head : out My_Ix) is
      New_Head : My_Ix;
      Swapped : Boolean := False;
   begin
      loop
         Head := N.Get;
         if Head = 0 then
            exit;
         else
            New_Head := Buf_Nodes (Head).Get;
            N.Compare_And_Swap (Head, New_Head, Swapped);
            exit when Swapped;
         end if;
      end loop;
   end Dequeue_Node;

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
end Linted.Stack;
