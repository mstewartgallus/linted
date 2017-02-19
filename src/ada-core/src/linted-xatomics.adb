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
with Interfaces.C;

package body Linted.XAtomics with
     Spark_Mode => Off is
   use type Interfaces.C.int;

   function Real_To is new Ada.Unchecked_Conversion
     (Source => Element_T,
      Target => Atomic);
   function Real_From is new Ada.Unchecked_Conversion
     (Source => Atomic,
      Target => Element_T);

   function To (Element : Element_T) return Atomic is (Real_To (Element));
   function From (A : Atomic) return Element_T is (Real_From (A));

   procedure Atomic_Store (A : in out Atomic; Element : Atomic);
   pragma Import (C, Atomic_Store, "atomic_store");

   function Atomic_Load (A : in out Atomic) return Atomic;
   pragma Import (C, Atomic_Load, "atomic_load");

   function Atomic_Compare_Exchange
     (A : in out Atomic;
      Expected : Atomic;
      Desired : Atomic) return Interfaces.C.int;
   pragma Import (C, Atomic_Compare_Exchange, "atomic_compare_exchange");

   function Atomic_Exchange
     (A : in out Atomic;
      New_Element : Atomic) return Atomic;
   pragma Import (C, Atomic_Exchange, "atomic_exchange");

   procedure Set (A : in out Atomic; Element : Element_T) is
   begin
      Atomic_Store (A, To (Element));
   end Set;

   procedure Get (A : in out Atomic; Element : out Element_T) is
   begin
      Element := From (Atomic_Load (A));
   end Get;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean)
   is
   begin
      Success :=
        Atomic_Compare_Exchange (A, To (Old_Element), To (New_Element)) /= 0;
   end Compare_And_Swap;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T)
   is
      Unused : Interfaces.C.int;
   begin
      Unused :=
        Atomic_Compare_Exchange (A, To (Old_Element), To (New_Element));
      pragma Unused (Unused);
   end Compare_And_Swap;

   procedure Swap
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T)
   is
   begin
      Old_Element := From (Atomic_Exchange (A, To (New_Element)));
   end Swap;
end Linted.XAtomics;
