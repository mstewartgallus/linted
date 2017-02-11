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
   function Is_Valid_ABA
     (X : ABA) return Boolean is
     (Interfaces.Unsigned_32 (X) <=
      (Interfaces.Shift_Left (Interfaces.Unsigned_32 (Element_T'Last), 16) or
       16#FFFF#));

   function Shift
     (X : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 with
      Pre => X <=
      (Interfaces.Shift_Left (Interfaces.Unsigned_32 (Element_T'Last), 16) or
       16#FFFF#),
      Post => Shift'Result <= 16#FFFF#
      and then Shift'Result <= Interfaces.Unsigned_32 (Element_T'Last);

   function Initialize
     (Element : Element_T;
      Tag : Tag_T) return ABA is
     (ABA (Interfaces.Shift_Left (Interfaces.Unsigned_32 (Element), 16)) or
      ABA (Tag));

   function Shift
     (X : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
     (Interfaces.Shift_Right (X, 16));

   function Element (X : ABA) return Element_T with
      Refined_Post => Element'Result =
      Element_T (Shift (Interfaces.Unsigned_32 (X)))
   is
   begin
      -- By private encapsulation this is assured
      pragma Assume (Is_Valid_ABA (X));
      return Element_T (Shift (Interfaces.Unsigned_32 (X)));
   end Element;

   function Tag (X : ABA) return Tag_T is (Tag_T (X and 16#FFFF#));

   procedure Lemma_Identity (E : Element_T; T : Tag_T) is null;
end Linted.ABAs;
