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
with Libc.Sched;

with Ada.Unchecked_Conversion;

package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   package body Tagged_Accessors is
      type My_Access is access all Node;

      function To (Ptr : access Node) return Node_Access is
      begin
         return To (Ptr, Normal);
      end To;

      function To (Ptr : access Node; My_Tag : Tag_Type) return Node_Access is
         function Convert is new Ada.Unchecked_Conversion
           (Source => My_Access,
            Target => Node_Access);
         Tag_N : Node_Access;
         Converted : Node_Access;
      begin
         Converted := Convert (Ptr);
         pragma Assert ((Converted and 2#11#) = 0);
         case My_Tag is
            when Normal =>
               Tag_N := 0;
            when Signal =>
               Tag_N := 1;
            when Broadcast =>
               Tag_N := 2;
         end case;
         return Converted or Tag_N;
      end To;

      function From (Ptr : Node_Access) return access Node is
         function Convert is new Ada.Unchecked_Conversion
           (Source => Node_Access,
            Target => My_Access);
      begin
         return Convert (Ptr and not 2#11#);
      end From;

      function Tag (Ptr : Node_Access) return Tag_Type is
      begin
         case Ptr and 2#11# is
            when 0 =>
               return Normal;
            when 1 =>
               return Signal;
            when 2 =>
               return Broadcast;
            when others =>
               raise Constraint_Error;
         end case;
      end Tag;
   end Tagged_Accessors;

   procedure Collect (W : in out Wait_List);
   procedure Insert (W : in out Wait_List; N : access Node);

   procedure Insert (W : in out Wait_List; N : access Node) is
      Head : Tagged_Accessors.Node_Access;
      Success : Boolean;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         N.Next := Head;
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            Head,
            Tagged_Accessors.To (N),
            Success);
         exit when Success;
         Libc.Sched.sched_yield;
      end loop;
   end Insert;

   procedure Wait (W : in out Wait_List) is
      Is_Triggered : Default_False;
   begin
      Boolean_Atomics.Swap (W.Triggered, Is_Triggered, False);
      if Is_Triggered then
         return;
      end if;

      declare
         N : aliased Node;
      begin
         Insert (W, N'Unchecked_Access);
         STC.Suspend_Until_True (N.Trigger);
      end;
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
      Head : Tagged_Accessors.Node_Access;
      New_Head : Tagged_Accessors.Node_Access;
      Success : Boolean;
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         New_Head :=
           Tagged_Accessors.To
             (Tagged_Accessors.From (Head),
              Tagged_Accessors.Broadcast);
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            Head,
            New_Head,
            Success);
         exit when Success;
         Libc.Sched.sched_yield;
      end loop;
      Collect (W);
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Head : Tagged_Accessors.Node_Access;
      New_Head : Tagged_Accessors.Node_Access;
      Success : Boolean;
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         case Tagged_Accessors.Tag (Head) is
            when Tagged_Accessors.Normal =>
               New_Head :=
                 Tagged_Accessors.To
                   (Tagged_Accessors.From (Head),
                    Tagged_Accessors.Signal);
            when Tagged_Accessors.Signal | Tagged_Accessors.Broadcast =>
               New_Head :=
                 Tagged_Accessors.To
                   (Tagged_Accessors.From (Head),
                    Tagged_Accessors.Broadcast);
         end case;
         Node_Access_Atomics.Compare_And_Swap
           (W.Root,
            Head,
            New_Head,
            Success);
         exit when Success;
         Libc.Sched.sched_yield;
      end loop;
      Collect (W);
   end Signal;

   procedure Collect (W : in out Wait_List) is
      Root : Tagged_Accessors.Node_Access;
      Do_Broadcast : Boolean := False;
   begin
      Node_Access_Atomics.Swap (W.Root, Root, Tagged_Accessors.To (null));

      while Tagged_Accessors.From (Root) /= null loop
         declare
            Next : Tagged_Accessors.Node_Access;
         begin
            Next := Tagged_Accessors.From (Root).Next;
            Tagged_Accessors.From (Root).Next := Tagged_Accessors.To (null);

            case Tagged_Accessors.Tag (Root) is
               when Tagged_Accessors.Normal =>
                  if Do_Broadcast then
                     STC.Set_True (Tagged_Accessors.From (Root).Trigger);
                  else
                     Insert (W, Tagged_Accessors.From (Root));
                  end if;
               when Tagged_Accessors.Signal =>
                  STC.Set_True (Tagged_Accessors.From (Root).Trigger);
               when Tagged_Accessors.Broadcast =>
                  STC.Set_True (Tagged_Accessors.From (Root).Trigger);
                  Do_Broadcast := True;
            end case;
            Root := Next;
         end;
      end loop;
   end Collect;
end Linted.Wait_Lists;
