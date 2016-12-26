-- Copyright 2016 Steven Stewart-Gallus
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

package body Linted.Queues with
     Spark_Mode => Off is
   package STC renames Ada.Synchronous_Task_Control;

   function Is_Null (N : Node_Access) return Boolean is
   begin
      return N = null;
   end Is_Null;

   function Is_Free (N : Node_Access) return Boolean is
   begin
      if N = null then
         return True;
      end if;
      return not Boolean (N.In_Queue);
   end Is_Free;

   protected body Queue is
      procedure Insert (C : Element_T; N : in out Node_Access) is
         Input : Node_Access;
      begin
         Input := N;
         N := null;
         pragma Assert (not Input.In_Queue);
         pragma Assert (Input.Tail = null);

         Input.In_Queue := True;
         Input.Contents := C;

         if First = null or Last = null then
            First := Input;
         else
            Last.Tail := Atomic_Node_Access (Input);
         end if;
         Last := Input;
      end Insert;

      procedure Remove (C : out Element_T; N : out Node_Access) is
      begin
         if First = null or Last = null then
            N := null;
         else
            declare
               Removed : Node_Access;
            begin
               Removed := First;
               pragma Assert (Removed.In_Queue);
               First := Node_Access (Removed.Tail);
               Removed.In_Queue := False;
               Removed.Tail := null;

               C := Removed.Contents;
               N := Removed;
            end;
         end if;
      end Remove;
   end Queue;

   type STC_Node is record
      Trigger : STC.Suspension_Object;
      Next_Trigger : access STC_Node;
   end record;

   protected type STC_List is
      procedure Insert (N : access STC_Node);
      procedure Remove (N : access STC_Node);
      procedure Broadcast;
   private
      Head : access STC_Node;
   end STC_List;

   protected body STC_List is
      procedure Insert (N : access STC_Node) is
      begin
	 N.Next_Trigger := Head;
         Head := N;
      end Insert;

      procedure Remove (N : access STC_Node) is
	 Last : access STC_Node;
         Current_Trigger : access STC_Node;
      begin
	 Last := null;
	 Current_Trigger := Head;

	 pragma Assert (Current_Trigger /= null);

	 if Current_Trigger = N then
	    Head := Current_Trigger.Next_Trigger;
	 else
	    loop
	       Last := Current_Trigger;
	       Current_Trigger := Current_Trigger.Next_Trigger;

	       pragma Assert (Current_Trigger /= null);

	       if Current_Trigger = N then
		  Last.Next_Trigger := Current_Trigger.Next_Trigger;
		  exit;
	       end if;
	    end loop;
	 end if;
      end Remove;

      procedure Broadcast is
         Current_Trigger : access STC_Node;
      begin
	 Current_Trigger := Head;
         loop
            if null = Current_Trigger then
               exit;
            end if;
            STC.Set_True (Current_Trigger.Trigger);
            Current_Trigger := Current_Trigger.Next_Trigger;
         end loop;
      end Broadcast;
   end STC_List;

   package body Pool with
        Refined_State => (State => (Triggers, Spare_Nodes, Contents)),
        Spark_Mode => Off is

      Contents : array (1 .. Initial_Count) of aliased Node;
      Spare_Nodes : Queue;
      Triggers : STC_List;

      procedure Wait (N : access STC_Node);
      procedure Broadcast;

      procedure Wait (N : access STC_Node) is
      begin
	 Triggers.Insert (N);
	 STC.Suspend_Until_True (N.Trigger);
	 Triggers.Remove (N);
      end Wait;

      procedure Broadcast is
      begin
	 Triggers.Broadcast;
      end Broadcast;

      package body User with
           Spark_Mode => Off,
           Refined_State => (User_State => (Free_Objects_Trigger)) is
         Free_Objects_Trigger : aliased STC_Node;

         procedure Allocate (N : out Node_Access) is
            Dummy : Element_T;
         begin
            loop
               Spare_Nodes.Remove (Dummy, N);
               if N /= null then
                  exit;
               end if;
               Wait (Free_Objects_Trigger'Unchecked_Access);
            end loop;
         end Allocate;

         procedure Free (N : in out Node_Access) is
            Dummy : Element_T;
         begin
            Spare_Nodes.Insert (Dummy, N);
            Broadcast;
         end Free;
      end User;
   begin
      for II in Contents'Range loop
         declare
            N : Node_Access := Contents (II)'Unchecked_Access;
            Dummy : Element_T;
         begin
            Spare_Nodes.Insert (Dummy, N);
         end;
      end loop;
   end Pool;
end Linted.Queues;
