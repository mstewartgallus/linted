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
generic
   type Element_T is mod <>;
package Linted.Mod_Atomics is
   pragma Pure;

   protected type Atomic with
      Lock_Free is
   private
      procedure Set (New_Ptr : Element_T) with
         Global => null,
         Depends => (Atomic => New_Ptr, null => Atomic);
      function Get return Element_T with
         Global => null,
         Depends => (Get'Result => Atomic);
      procedure Compare_And_Swap
        (Old_Ptr : Element_T;
         New_Ptr : Element_T;
         Success : in out Boolean) with
         Global => null,
         Depends =>
         (Success => (Old_Ptr, Atomic),
          Atomic => (Atomic, Old_Ptr, New_Ptr),
          null => Success);
      procedure Swap (Old_Ptr : in out Element_T; New_Ptr : Element_T) with
         Global => null,
         Depends =>
         (Old_Ptr => Atomic,
          Atomic => (Atomic, New_Ptr),
          null => Old_Ptr);
      procedure Saturating_Increment with
         Global => null,
         Depends => (Atomic => Atomic);
      procedure Saturating_Decrement with
         Global => null,
         Depends => (Atomic => Atomic);

      Ptr : Element_T;
   end Atomic;

   procedure Set (A : in out Atomic; Element : Element_T) with
      Global => null,
      Depends => (A => (A, Element));
   procedure Get (A : in out Atomic; Element : out Element_T) with
      Global => null,
      Depends => (Element => A, A => A);
   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean) with
      Global => null,
      Depends =>
      (Success => (A, Old_Element),
       A => (A, Old_Element, New_Element));
   procedure Swap
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T) with
      Global => null,
      Depends => (Old_Element => A, A => (A, New_Element));

   procedure Saturating_Increment (A : in out Atomic) with
      Global => null,
      Depends => (A => A);

   procedure Saturating_Decrement (A : in out Atomic) with
      Global => null,
      Depends => (A => A);

end Linted.Mod_Atomics;
