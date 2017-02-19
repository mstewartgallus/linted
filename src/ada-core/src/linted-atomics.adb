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
package body Linted.Atomics is
   protected body Atomic is
      procedure Set (New_Ptr : Element_T) is
      begin
         Ptr := New_Ptr;
      end Set;

      function Get return Element_T is
      begin
         return Ptr;
      end Get;

      procedure Compare_And_Swap
        (Old_Ptr : Element_T;
         New_Ptr : Element_T;
         Success : in out Boolean)
      is
      begin
         if Old_Ptr = Ptr then
            Ptr := New_Ptr;
            Success := True;
         else
            Success := False;
         end if;
      end Compare_And_Swap;

      procedure Compare_And_Swap (Old_Ptr : Element_T; New_Ptr : Element_T) is
      begin
         if Old_Ptr = Ptr then
            Ptr := New_Ptr;
         end if;
      end Compare_And_Swap;

      procedure Swap (Old_Ptr : in out Element_T; New_Ptr : Element_T) is
      begin
         Old_Ptr := Ptr;
         Ptr := New_Ptr;
      end Swap;
   end Atomic;

   procedure Set (A : in out Atomic; Element : Element_T) is
   begin
      A.Set (Element);
   end Set;

   procedure Get (A : in out Atomic; Element : out Element_T) is
   begin
      Element := A.Get;
   end Get;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean)
   is
   begin
      Success := False;
      A.Compare_And_Swap (Old_Element, New_Element, Success);
   end Compare_And_Swap;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T)
   is
   begin
      A.Compare_And_Swap (Old_Element, New_Element);
   end Compare_And_Swap;

   procedure Swap
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T)
   is
      Dummy : Element_T;
   begin
      Old_Element := Dummy;
      A.Swap (Old_Element, New_Element);
   end Swap;

end Linted.Atomics;
