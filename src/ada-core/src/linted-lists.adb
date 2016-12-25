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
package body Linted.Lists with
     Spark_Mode => Off is
   package body Pool with
        Refined_State => (State => (Count, Contents)) is
      protected type Lock is
         procedure Allocate_Impl (N : out Node_Access);
      end Lock;

      Count : Natural := Initial_Count;
      Contents : array (1 .. Initial_Count) of aliased Node;

      protected body Lock is
         procedure Allocate_Impl (N : out Node_Access) is
         begin
            if Count > 0 then
               N := Contents (Count)'Unchecked_Access;
               Count := Count - 1;
            else
               N := null;
            end if;
         end Allocate_Impl;
      end Lock;

      My_Lock : Lock;

      procedure Allocate (N : out Node_Access) is
      begin
         My_Lock.Allocate_Impl (N);
      end Allocate;
   end Pool;

   function Is_Null (N : Node_Access) return Boolean is
   begin
      return N = null;
   end Is_Null;

   function Is_Free (N : Node_Access) return Boolean is
   begin
      if N = null then
         return True;
      end if;
      return not Boolean (N.In_List);
   end Is_Free;

   protected body List is
      procedure Insert (C : Element_T; N : Node_Access) is
      begin
         pragma Assert (not N.In_List);
         N.In_List := True;
         pragma Assert (N.Tail = null);
         N.Tail := Atomic_Node_Access (Head);
         N.Contents := C;
         Head := N;
      end Insert;

      procedure Remove (C : out Element_T; N : out Node_Access) is
         Removed : Node_Access;
      begin
         Removed := Head;
         if null = Removed then
            N := null;
         else
            pragma Assert (Removed.In_List);
            Removed.In_List := False;
            Removed.Tail := null;
            Head := Node_Access (Removed.Tail);
            N := Removed;
            C := Removed.Contents;
         end if;
      end Remove;
   end List;
end Linted.Lists;
