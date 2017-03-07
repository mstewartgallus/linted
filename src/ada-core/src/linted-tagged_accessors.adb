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
with Ada.Unchecked_Conversion;

with Interfaces;

package body Linted.Tagged_Accessors is
   use type Interfaces.Unsigned_64;

   --  Warnings are off because of a spurious warning about strict
   --  aliasing
   pragma Warnings (Off);
   function Convert is new Ada.Unchecked_Conversion
     (Source => Access_T,
      Target => Interfaces.Unsigned_64);
   function Convert is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_64,
      Target => Access_T);
   pragma Warnings (On);

   function To (Ptr : Access_T) return Tagged_Access is
   begin
      return To (Ptr, 0);
   end To;

   function To (Ptr : Access_T; Tag : Tag_Bits) return Tagged_Access is
      Converted : Interfaces.Unsigned_64;
   begin
      Converted := Convert (Ptr);
      pragma Assert
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Converted), 48) = 0);
      return Tagged_Access
          (Interfaces.Shift_Right (Converted, 6) or
           Interfaces.Shift_Left (Interfaces.Unsigned_64 (Tag), 42));
   end To;

   function From (Ptr : Tagged_Access) return Access_T is
   begin
      return Convert
          (Interfaces.Shift_Left
             (Interfaces.Unsigned_64 (Ptr) and
              2#11_11111111_11111111_11111111_11111111_11111111#,
              6));
   end From;

   function Tag (Ptr : Tagged_Access) return Tag_Bits is
   begin
      return Tag_Bits
          (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Ptr), 42));
   end Tag;
end Linted.Tagged_Accessors;
