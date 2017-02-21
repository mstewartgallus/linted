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
   type Element_T is private;
package Linted.XAtomics with
   Spark_Mode is
   pragma Pure;

   type Atomic is limited private;

   procedure Set (A : in out Atomic; Element : Element_T) with
      Inline_Always,
      Global => null,
      Depends => (A =>+ Element);
   procedure Get (A : in out Atomic; Element : out Element_T) with
      Inline_Always,
      Global => null,
      Depends => (Element => A, A => A);
   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean) with
      Inline_Always,
      Global => null,
      Depends =>
      (Success => (A, Old_Element),
       A =>+ (Old_Element, New_Element));
   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T) with
      Inline_Always,
      Global => null,
      Depends => (A =>+ (Old_Element, New_Element));
   procedure Swap
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T) with
      Inline_Always,
      Global => null,
      Depends => (Old_Element => A, A =>+ New_Element);
private
   subtype Dummy is Element_T;

   -- A record has to be used so that it is passed by pointer
   type Atomic is record
      Value : Dummy with
         Atomic;
   end record;
   pragma Convention (C, Atomic);
end Linted.XAtomics;
