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
   function Is_Null (N : Node_Access) return Boolean is
   begin
      return N = null;
   end Is_Null;

   function Is_Free (N : Node_Access) return Boolean is
   begin
      if N = null then
         return True;
      end if;
      return N.Tail = null;
   end Is_Free;

   protected body List is
      procedure Insert (C : Element_T; N : Node_Access) is
      begin
         pragma Assert (N.Tail = null);
         N.Contents := C;
         N.Tail := Head;
         Head := N;
      end Insert;

      procedure Remove (C : out Element_T; N : out Node_Access) is
         M : Node_Access;
      begin
         if Count > 0 then
            N := Initial_Capacity.Contents (Count)'Unchecked_Access;
            Count := Count - 1;
         else
            M := Head;
            if null = Head then
               N := null;
            else
               Head := M.Tail;
               M.Tail := null;
               N := M;
               C := M.Contents;
            end if;
         end if;
      end Remove;
   end List;
end Linted.Lists;
