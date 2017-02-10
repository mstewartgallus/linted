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

package body Linted.Tagged_Accessors is
   function To (Ptr : Access_T) return Tagged_Access is
   begin
      return To (Ptr, 0);
   end To;

   function To (Ptr : Access_T; Tag : Tag_Bits) return Tagged_Access is
      function Convert is new Ada.Unchecked_Conversion
	(Source => Access_T,
	 Target => Tagged_Access);
      Converted : Tagged_Access;
   begin
      Converted := Convert (Ptr);
      pragma Assert ((Converted and 2#11#) = 0);
      return Converted or Tagged_Access (Tag);
   end To;

   function From (Ptr : Tagged_Access) return Access_T is
      function Convert is new Ada.Unchecked_Conversion
	(Source => Tagged_Access,
	 Target => Access_T);
   begin
      return Convert (Ptr and not 2#11#);
   end From;

   function Tag (Ptr : Tagged_Access) return Tag_Bits is
   begin
      return Tag_Bits (Ptr and 2#11#);
   end Tag;
end Linted.Tagged_Accessors;
