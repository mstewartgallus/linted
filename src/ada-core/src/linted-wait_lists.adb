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
with Linted.Sched;

with Ada.Unchecked_Conversion;

package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   package body Tagged_Accessors is
      function To (Ptr : Node_Ptr) return Node_Access is
      begin
         return To (Ptr, Normal);
      end To;

      function To (Ptr : Node_Ptr; My_Tag : Tag_Type) return Node_Access is
         function Convert is new Ada.Unchecked_Conversion
           (Source => Node_Ptr,
            Target => Node_Access);
         Tag_N : Node_Access;
         Converted : Node_Access;
      begin
         Converted := Convert (Ptr);
         pragma Assert ((Converted and 2#1#) = 0);
         case My_Tag is
            when Normal =>
               Tag_N := 0;
            when Pinned =>
               Tag_N := 1;
         end case;
         return Converted or Tag_N;
      end To;

      function From (Ptr : Node_Access) return Node_Ptr is
         function Convert is new Ada.Unchecked_Conversion
           (Source => Node_Access,
            Target => Node_Ptr);
      begin
         return Convert (Ptr and not 2#1#);
      end From;

      function Tag (Ptr : Node_Access) return Tag_Type is
      begin
         case Ptr and 2#1# is
            when 0 =>
               return Normal;
            when 1 =>
               return Pinned;
            when others =>
               raise Constraint_Error;
         end case;
      end Tag;
   end Tagged_Accessors;

   use type Tagged_Accessors.Tag_Type;

   procedure Insert (W : in out Wait_List; N : Node_Ptr);
   procedure Pop (W : in out Wait_List; N : out Node_Ptr);

   procedure Insert (W : in out Wait_List; N : Node_Ptr) is
      Head : Tagged_Accessors.Node_Access;
      Success : Boolean;
      Backoff : Sched.Backoff_State;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         if Tagged_Accessors.Tag (Head) /= Tagged_Accessors.Pinned then
            N.Next := Tagged_Accessors.From (Head);
            Node_Access_Atomics.Compare_And_Swap
              (W.Root,
               Head,
               Tagged_Accessors.To (N),
               Success);
            exit when Success;
         end if;
         Sched.Backoff (Backoff);
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

   procedure Pop (W : in out Wait_List; N : out Node_Ptr) is
      Head : Tagged_Accessors.Node_Access;
      New_Head : Tagged_Accessors.Node_Access;
      Success : Boolean;
      Backoff : Sched.Backoff_State;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         if null = Tagged_Accessors.From (Head) then
            N := null;
            return;
         end if;
         if Tagged_Accessors.Tag (Head) /= Tagged_Accessors.Pinned then
            New_Head :=
              Tagged_Accessors.To
                (Tagged_Accessors.From (Head),
                 Tagged_Accessors.Pinned);
            Node_Access_Atomics.Compare_And_Swap
              (W.Root,
               Head,
               New_Head,
               Success);
            exit when Success;
         end if;
         Sched.Backoff (Backoff);
      end loop;

      Node_Access_Atomics.Set
        (W.Root,
         Tagged_Accessors.To (Tagged_Accessors.From (Head).Next));
      Tagged_Accessors.From (Head).Next := null;
      N := Tagged_Accessors.From (Head);
   end Pop;

   procedure Broadcast (W : in out Wait_List) is
      Head : Node_Ptr;
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      loop
         Pop (W, Head);
         exit when Head = null;
         STC.Set_True (Head.Trigger);
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Head : Node_Ptr;
   begin
      Boolean_Atomics.Set (W.Triggered, True);

      Pop (W, Head);
      if Head /= null then
         STC.Set_True (Head.Trigger);
      end if;
   end Signal;
end Linted.Wait_Lists;
