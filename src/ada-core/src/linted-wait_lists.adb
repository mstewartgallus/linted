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
package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   use type Tags.Tag_Bits;
   use type Sched.Backoff_State;

   procedure Insert (W : in out Wait_List; N : Node_Access);
   procedure Pop (W : in out Wait_List; N : out Node_Access);

   procedure Insert (W : in out Wait_List; N : Node_Access) is
      Head : Tags.Tagged_Access;
      Success : Boolean;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         if Tags.Tag (Head) /= 1 then
            N.Next := Tags.From (Head);
            Node_Access_Atomics.Compare_And_Swap
              (W.Root,
               Head,
               Tags.To (N),
               Success);
            exit when Success;
         end if;
         Sched.Backoff (W.Head_Contention);
      end loop;
      Sched.Success (W.Head_Contention);
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

   procedure Pop (W : in out Wait_List; N : out Node_Access) is
      Head : Tags.Tagged_Access;
      New_Head : Tags.Tagged_Access;
      Success : Boolean;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         if null = Tags.From (Head) then
            N := null;
            Sched.Success (W.Head_Contention);
            return;
         end if;
         if Tags.Tag (Head) /= 1 then
            New_Head := Tags.To (Tags.From (Head), 1);
            Node_Access_Atomics.Compare_And_Swap
              (W.Root,
               Head,
               New_Head,
               Success);
            exit when Success;
         end if;
         Sched.Backoff (W.Head_Contention);
      end loop;
      Sched.Success (W.Head_Contention);

      Node_Access_Atomics.Set (W.Root, Tags.To (Tags.From (Head).Next));
      Tags.From (Head).Next := null;
      N := Tags.From (Head);
   end Pop;

   procedure Broadcast (W : in out Wait_List) is
      Head : Node_Access;
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      loop
         Pop (W, Head);
         exit when Head = null;
         STC.Set_True (Head.Trigger);
      end loop;
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Head : Node_Access;
   begin
      Boolean_Atomics.Set (W.Triggered, True);

      Pop (W, Head);
      if Head /= null then
         STC.Set_True (Head.Trigger);
      end if;
   end Signal;
end Linted.Wait_Lists;
