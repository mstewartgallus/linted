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
private with System;

generic
   type Element_T (<>) is limited private;
   type Access_T is access all Element_T;
package Linted.Tagged_Accessors is
   type Tagged_Access is private with
      Preelaborable_Initialization;

   type Tag_Bits is mod 2**1 with
        Default_Value => 0;

   function To (Ptr : Access_T) return Tagged_Access;
   function To (Ptr : Access_T; Tag : Tag_Bits) return Tagged_Access;
   function From (Ptr : Tagged_Access) return Access_T;
   function Tag (Ptr : Tagged_Access) return Tag_Bits;
private
   type Uintptr is mod System.Memory_Size;
   type Tagged_Access is new Uintptr with
        Default_Value => 0;
end Linted.Tagged_Accessors;
