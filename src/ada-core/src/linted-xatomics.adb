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
with Interfaces;

package body Linted.XAtomics with
     Spark_Mode => Off is
   use Interfaces;
   use type Interfaces.C.int;

   pragma Compile_Time_Error
     (not
      (Element_T'Size = 8 or
       Element_T'Size = 16 or
       Element_T'Size = 32 or
       Element_T'Size = 64),
      "Nonatomic size");

   function To_8 is new Ada.Unchecked_Conversion (Element_T, Unsigned_8);
   function From_8 is new Ada.Unchecked_Conversion (Unsigned_8, Element_T);
   function To_16 is new Ada.Unchecked_Conversion (Element_T, Unsigned_16);
   function From_16 is new Ada.Unchecked_Conversion (Unsigned_16, Element_T);
   function To_32 is new Ada.Unchecked_Conversion (Element_T, Unsigned_32);
   function From_32 is new Ada.Unchecked_Conversion (Unsigned_32, Element_T);
   function To_64 is new Ada.Unchecked_Conversion (Element_T, Unsigned_64);
   function From_64 is new Ada.Unchecked_Conversion (Unsigned_64, Element_T);

   ATOMIC_SEQ_CST : constant := 5;

   function Atomic_Compare_Exchange_1
     (Ptr : System.Address;
      Expected : System.Address;
      Desired : Unsigned_8;
      Weak : Boolean;
      Success_Memmodel : Interfaces.C.int;
      Failure_Memmodel : Interfaces.C.int) return Boolean;
   pragma Import
     (Intrinsic,
      Atomic_Compare_Exchange_1,
      "__atomic_compare_exchange_1");

   function Atomic_Compare_Exchange_2
     (Ptr : System.Address;
      Expected : System.Address;
      Desired : Unsigned_16;
      Weak : Boolean;
      Success_Memmodel : Interfaces.C.int;
      Failure_Memmodel : Interfaces.C.int) return Boolean;
   pragma Import
     (Intrinsic,
      Atomic_Compare_Exchange_2,
      "__atomic_compare_exchange_2");

   function Atomic_Compare_Exchange_4
     (Ptr : System.Address;
      Expected : System.Address;
      Desired : Unsigned_32;
      Weak : Boolean;
      Success_Memmodel : Interfaces.C.int;
      Failure_Memmodel : Interfaces.C.int) return Boolean;
   pragma Import
     (Intrinsic,
      Atomic_Compare_Exchange_4,
      "__atomic_compare_exchange_4");

   function Atomic_Compare_Exchange_8
     (Ptr : System.Address;
      Expected : System.Address;
      Desired : Unsigned_64;
      Weak : Boolean;
      Success_Memmodel : Interfaces.C.int;
      Failure_Memmodel : Interfaces.C.int) return Boolean;
   pragma Import
     (Intrinsic,
      Atomic_Compare_Exchange_8,
      "__atomic_compare_exchange_8");

   function Atomic_Exchange_1
     (Ptr : System.Address;
      New_Value : Unsigned_8;
      Memmodel : Interfaces.C.int) return Unsigned_8;
   pragma Import (Intrinsic, Atomic_Exchange_1, "__atomic_exchange_1");

   function Atomic_Exchange_2
     (Ptr : System.Address;
      New_Value : Unsigned_16;
      Memmodel : Interfaces.C.int) return Unsigned_16;
   pragma Import (Intrinsic, Atomic_Exchange_2, "__atomic_exchange_2");

   function Atomic_Exchange_4
     (Ptr : System.Address;
      New_Value : Unsigned_32;
      Memmodel : Interfaces.C.int) return Unsigned_32;
   pragma Import (Intrinsic, Atomic_Exchange_4, "__atomic_exchange_4");

   function Atomic_Exchange_8
     (Ptr : System.Address;
      New_Value : Unsigned_64;
      Memmodel : Interfaces.C.int) return Unsigned_64;
   pragma Import (Intrinsic, Atomic_Exchange_8, "__atomic_exchange_8");

   procedure Set (A : in out Atomic; Element : Element_T) is
   begin
      A.Value := Element;
   end Set;

   procedure Get (A : in out Atomic; Element : out Element_T) is
   begin
      Element := A.Value;
   end Get;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean)
   is
      New_Value : Element_T := New_Element;
      Expected : Element_T := Old_Element;
   begin
      case A.Value'Size is
         when 8 =>
            Success :=
              Atomic_Compare_Exchange_1
                (A.Value'Address,
                 Expected'Address,
                 To_8 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when 16 =>
            Success :=
              Atomic_Compare_Exchange_2
                (A.Value'Address,
                 Expected'Address,
                 To_16 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when 32 =>
            Success :=
              Atomic_Compare_Exchange_4
                (A.Value'Address,
                 Expected'Address,
                 To_32 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when 64 =>
            Success :=
              Atomic_Compare_Exchange_8
                (A.Value'Address,
                 Expected'Address,
                 To_64 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when others =>
            raise Program_Error;
      end case;
   end Compare_And_Swap;

   procedure Compare_And_Swap
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T)
   is
      New_Value : Dummy := New_Element;
      Expected : Dummy := Old_Element;
      Success : Boolean;
   begin
      case A.Value'Size is
         when 8 =>
            Success :=
              Atomic_Compare_Exchange_1
                (A.Value'Address,
                 Expected'Address,
                 To_8 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when 16 =>
            Success :=
              Atomic_Compare_Exchange_2
                (A.Value'Address,
                 Expected'Address,
                 To_16 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when 32 =>
            Success :=
              Atomic_Compare_Exchange_4
                (A.Value'Address,
                 Expected'Address,
                 To_32 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when 64 =>
            Success :=
              Atomic_Compare_Exchange_8
                (A.Value'Address,
                 Expected'Address,
                 To_64 (New_Value),
                 False,
                 ATOMIC_SEQ_CST,
                 ATOMIC_SEQ_CST);
         when others =>
            raise Program_Error;
      end case;
      pragma Unused (Success);
   end Compare_And_Swap;

   procedure Swap
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T)
   is
      Result : Dummy;
      New_Value : Dummy := New_Element;
   begin
      case A.Value'Size is
         when 8 =>
            Result :=
              From_8
                (Atomic_Exchange_1
                   (A.Value'Address,
                    To_8 (New_Value),
                    ATOMIC_SEQ_CST));
         when 16 =>
            Result :=
              From_16
                (Atomic_Exchange_2
                   (A.Value'Address,
                    To_16 (New_Value),
                    ATOMIC_SEQ_CST));
         when 32 =>
            Result :=
              From_32
                (Atomic_Exchange_4
                   (A.Value'Address,
                    To_32 (New_Value),
                    ATOMIC_SEQ_CST));
         when 64 =>
            Result :=
              From_64
                (Atomic_Exchange_8
                   (A.Value'Address,
                    To_64 (New_Value),
                    ATOMIC_SEQ_CST));
         when others =>
            raise Program_Error;
      end case;
      Old_Element := Result;
   end Swap;
end Linted.XAtomics;
