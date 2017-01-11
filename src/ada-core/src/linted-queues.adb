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
with Linted.Wait_Lists;

package body Linted.Queues with
     Refined_State => (State => (Contents, Spare_Nodes, Triggers)) is

   type Node is record
      Contents : Element_T;
      Tail : Node_Access := 0;
      In_Queue : Boolean := False;
   end record;

   type Node_Array is array (Node_Not_Null_Access range <>) of aliased Node;

   Contents : Node_Array (1 .. Node_Access (Max_Nodes_In_Flight));
   Spare_Nodes : Queue;
   Triggers : Wait_Lists.Wait_List;

   procedure Allocate (N : out Node_Not_Null_Access) with
      Post => Is_Free (N);
   procedure Free (N : Node_Not_Null_Access) with
      Pre => Is_Free (N);

   function Is_Free (N : Node_Not_Null_Access) return Boolean is
   begin
      return not Contents (N).In_Queue;
   end Is_Free;

   protected body Queue is
      procedure Insert (N : Node_Not_Null_Access) is
      begin
         pragma Assert (not Contents (N).In_Queue);
         pragma Assert (Contents (N).Tail = 0);

         Contents (N).In_Queue := True;

         if First = 0 or Last = 0 then
            First := Node_Access (N);
         else
            Contents (Last).Tail := Node_Access (N);
         end if;
         Last := Node_Access (N);
      end Insert;

      procedure Remove (N : out Node_Access) is
      begin
         if First = 0 or Last = 0 then
            N := 0;
         else
            declare
               Removed : Node_Access;
            begin
               Removed := First;
               pragma Assert (Contents (Removed).In_Queue);
               First := Contents (Removed).Tail;
               Contents (Removed).In_Queue := False;
               Contents (Removed).Tail := 0;

               N := Removed;
            end;
         end if;
      end Remove;
   end Queue;

   procedure Free (N : Node_Not_Null_Access) is
      Dummy : Element_T;
   begin
      Contents (N).Contents := Dummy;
      Spare_Nodes.Insert (N);
      Wait_Lists.Signal (Triggers);
   end Free;

   procedure Allocate (N : out Node_Not_Null_Access) is
      Dummy : Element_T;
      Removed : Node_Access;
   begin
      loop
         Spare_Nodes.Remove (Removed);
         if Removed /= 0 then
            exit;
         end if;
	 Wait_Lists.Wait (Triggers);
      end loop;
      N := Node_Not_Null_Access (Removed);
      Contents (N).Contents := Dummy;
   end Allocate;

   procedure Insert (Q : in out Queue; C : Element_T) is
      --  FAKE!
      N : Node_Not_Null_Access := 1;
   begin
      Allocate (N);
      Contents (N).Contents := C;
      Q.Insert (N);
   end Insert;

   procedure Remove
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean)
   is
      Removed : Node_Access;
   begin
      Q.Remove (Removed);
      if Removed = 0 then
         Init := False;
      else
	 declare
	    N : Node_Not_Null_Access := Node_Not_Null_Access (Removed);
	 begin
	    C := Contents (N).Contents;
	    Free (N);
	    Init := True;
	 end;
      end if;
   end Remove;

begin
   for II in Contents'Range loop
      declare
         N : Node_Not_Null_Access := II;
      begin
         Spare_Nodes.Insert (N);
      end;
   end loop;
end Linted.Queues;
