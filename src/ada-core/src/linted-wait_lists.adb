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
with Ada.Synchronous_Task_Control;

package body Linted.Wait_Lists with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   type Node (T : Node_Type := Normal_Type) is record
      Trigger : Ada.Synchronous_Task_Control.Suspension_Object;
      Next : Node_Access;
      case T is
	 when Normal_Type =>
	    null;
	 when Signal_Type =>
	    null;
	 when Broadcast_Type =>
	    null;
      end case;
   end record;

   procedure Collect (W : in out Wait_List);
   procedure Insert (W : in out Wait_List; N : Node_Access);

   procedure Insert (W : in out Wait_List; N : Node_Access) is
      Head : Node_Access;
      Success : Boolean;
   begin
      loop
         Node_Access_Atomics.Get (W.Root, Head);
         N.Next := Head;
         Node_Access_Atomics.Compare_And_Swap (W.Root, Head, N, Success);
         exit when Success;
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
         N : aliased Node (T => Normal_Type);
      begin
         Insert (W, N'Unchecked_Access);
         STC.Suspend_Until_True (N.Trigger);
      end;
   end Wait;

   procedure Broadcast (W : in out Wait_List) is
      Broadcast_Node : aliased Node (T => Broadcast_Type);
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      Insert (W, Broadcast_Node'Unchecked_Access);
      Collect (W);
      STC.Suspend_Until_True (Broadcast_Node.Trigger);
   end Broadcast;

   procedure Signal (W : in out Wait_List) is
      Signal_Node : aliased Node (T => Signal_Type);
   begin
      Boolean_Atomics.Set (W.Triggered, True);
      Insert (W, Signal_Node'Unchecked_Access);
      Collect (W);
      STC.Suspend_Until_True (Signal_Node.Trigger);
   end Signal;

   procedure Collect (W : in out Wait_List) is
      Root : Node_Access;
      Signals : Natural := 0;
   begin
      loop
         Node_Access_Atomics.Swap (W.Root, Root, null);
         while Root /= null loop
            declare
               Next : Node_Access;
            begin
               Next := Root.Next;
               Root.Next := null;

               case Root.T is
                  when Normal_Type =>
                     if Signals > 0 then
                        Signals := Signals - 1;
                        STC.Set_True (Root.Trigger);
                     else
                        Insert (W, Root);
                     end if;
                  when Signal_Type =>
                     STC.Set_True (Root.Trigger);
                     Signals := Signals + 1;
                  when Broadcast_Type =>
                     STC.Set_True (Root.Trigger);
                     Root := Next;
                     goto Do_Broadcast;
               end case;
               Root := Next;
            end;
         end loop;
         exit when null = Root or Signals = 0;
      end loop;
      return;
      <<Do_Broadcast>>
      while Root /= null loop
         declare
            Next : Node_Access;
         begin
            Next := Root.Next;
            Root.Next := null;
            STC.Set_True (Root.Trigger);
            Root := Next;
         end;
      end loop;
   end Collect;
end Linted.Wait_Lists;
