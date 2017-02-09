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
package body Linted.ABAs is
   function Initialize (Element : Element_T; Tag : Tag_T) return ABA is
   begin
      return ABA
          (Interfaces.Shift_Left (Interfaces.Unsigned_32 (Element), 16)) or
        ABA (Tag);
   end Initialize;

   function Element (X : ABA) return Element_T is
   begin
      return Element_T
          (Interfaces.Shift_Right (Interfaces.Unsigned_32 (X), 16));
   end Element;

   function Tag (X : ABA) return Tag_T is
   begin
      return Tag_T (X and 16#FFFF#);
   end Tag;
end Linted.ABAs;
