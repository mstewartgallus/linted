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
with System;
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

   ATOMIC_SEQ_CST : constant := 5;

   function Atomic_Compare_Exchange
     (Ptr : System.Address;
      Expected : System.Address;
      Desired : Atomic;
      Weak : Boolean;
      Success_Memmodel : Interfaces.C.int;
      Failure_Memmodel : Interfaces.C.int) return Boolean;
   pragma Import
     (Intrinsic,
      Atomic_Compare_Exchange,
      "__atomic_compare_exchange_8");

   function Atomic_Exchange
     (Ptr : System.Address;
      New_Value : Atomic;
      Memmodel : Interfaces.C.int) return Atomic;
   pragma Import (Intrinsic, Atomic_Exchange, "__atomic_exchange_8");

   procedure Set (A : in out Atomic; Element : Element_T) is
   begin
      A := To (Element);
   end Set;

   procedure Get (A : in out Atomic; Element : out Element_T) is
   begin
      Element := From (A);
   end Get;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean)
   is
      Expected : Atomic := To (Old_Element);
   begin
      Success :=
        Atomic_Compare_Exchange
          (A'Address,
           Expected'Address,
           To (New_Element),
           False,
           ATOMIC_SEQ_CST,
           ATOMIC_SEQ_CST);
   end Compare_And_Swap;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T)
   is
      Expected : Atomic := To (Old_Element);
      Unused : Boolean;
   begin
      Unused :=
        Atomic_Compare_Exchange
          (A'Address,
           Expected'Address,
           To (New_Element),
           False,
           ATOMIC_SEQ_CST,
           ATOMIC_SEQ_CST);
      pragma Unused (Unused);
   end Compare_And_Swap;

   procedure Swap
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T)
   is
   begin
      Old_Element :=
        From (Atomic_Exchange (A'Address, To (New_Element), ATOMIC_SEQ_CST));
   end Swap;
end Linted.XAtomics;
