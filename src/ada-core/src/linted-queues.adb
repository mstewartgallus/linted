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
     Spark_Mode => Off,
     Refined_State => (State => (Contents, Spare_Nodes, Triggers)) is

   type Node_Array is array (Positive range <>) of aliased Node;

   Contents : Node_Array (1 .. Max_Nodes_In_Flight);
   Spare_Nodes : Queue;
   Triggers : Wait_Lists.Wait_List;

   procedure Allocate (N : out Node_Access) with
      Post => Is_Free (N) and not Is_Null (N);
   procedure Free (N : in out Node_Access) with
      Pre => Is_Free (N) and not Is_Null (N),
      Post => Is_Null (N);

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

   procedure Free (N : in out Node_Access) is
      Dummy : Element_T;
   begin
      Spare_Nodes.Insert (Dummy, N);
      Wait_Lists.Signal (Triggers);
   end Free;

   procedure Allocate (N : out Node_Access) is
      Dummy : Element_T;
   begin
      loop
         Spare_Nodes.Remove (Dummy, N);
         if N /= null then
            exit;
         end if;
	 Wait_Lists.Wait (Triggers);
      end loop;
   end Allocate;

   procedure Insert (Q : in out Queue; C : Element_T) is
      N : Node_Access;
   begin
      Allocate (N);
      Q.Insert (C, N);
   end Insert;

   procedure Remove
     (Q : in out Queue;
      C : out Element_T;
      Init : out Boolean)
   is
      N : Node_Access;
   begin
      Q.Remove (C, N);
      if N = null then
         Init := False;
      else
         Free (N);
         Init := True;
      end if;
   end Remove;

begin
   for II in Contents'Range loop
      declare
         N : Node_Access := Contents (II)'Unchecked_Access;
         Dummy : Element_T;
      begin
         Spare_Nodes.Insert (Dummy, N);
      end;
   end loop;
end Linted.Queues;
