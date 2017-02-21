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
private with Interfaces;

generic
   type Element_T is mod <>;
package Linted.ABAs is
   pragma Pure;

   type Tag_T is mod 2**16 with
        Default_Value => 0;
   type ABA is private with
      Preelaborable_Initialization;

   function Is_Valid_ABA (X : ABA) return Boolean with
      Ghost;

   function Initialize (Element : Element_T; Tag : Tag_T) return ABA with
      Inline_Always,
      Depends => (Initialize'Result => (Element, Tag)),
      Post => Is_Valid_ABA (Initialize'Result);
   function Element (X : ABA) return Element_T with
      Inline_Always,
      Depends => (Element'Result => X);
   function Tag (X : ABA) return Tag_T with
      Inline_Always,
      Depends => (Tag'Result => X);

   procedure Lemma_Identity (E : Element_T; T : Tag_T) with
      Ghost,
      Pre => Is_Valid_ABA (Initialize (E, T)),
      Post => E = Element (Initialize (E, T)) and T = Tag (Initialize (E, T));
private
   subtype U32 is Interfaces.Unsigned_32;
   use type U32;

   type ABA is new U32 with
        Default_Value => 0;
end Linted.ABAs;
