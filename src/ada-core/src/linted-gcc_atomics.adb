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

package body Linted.GCC_Atomics with
     Spark_Mode => Off is
   use Interfaces;
   use type Interfaces.C.int;

   procedure Atomic_Thread_Fence (Memmodel : Interfaces.C.int);
   pragma Import (Intrinsic, Atomic_Thread_Fence, "__atomic_thread_fence");

   procedure Atomic_Signal_Fence (Memmodel : Interfaces.C.int);
   pragma Import (Intrinsic, Atomic_Signal_Fence, "__atomic_signal_fence");

   procedure Atomic_Store_1
     (Ptr : System.Address;
      Val : Unsigned_8;
      Memmodel : Interfaces.C.int);
   pragma Import (Intrinsic, Atomic_Store_1, "__atomic_store_1");

   procedure Atomic_Store_2
     (Ptr : System.Address;
      Val : Unsigned_16;
      Memmodel : Interfaces.C.int);
   pragma Import (Intrinsic, Atomic_Store_2, "__atomic_store_2");

   procedure Atomic_Store_4
     (Ptr : System.Address;
      Val : Unsigned_32;
      Memmodel : Interfaces.C.int);
   pragma Import (Intrinsic, Atomic_Store_4, "__atomic_store_4");

   procedure Atomic_Store_8
     (Ptr : System.Address;
      Val : Unsigned_64;
      Memmodel : Interfaces.C.int);
   pragma Import (Intrinsic, Atomic_Store_8, "__atomic_store_8");

   function Atomic_Load_1
     (Ptr : System.Address;
      Memmodel : Interfaces.C.int) return Unsigned_8;
   pragma Import (Intrinsic, Atomic_Load_1, "__atomic_load_1");

   function Atomic_Load_2
     (Ptr : System.Address;
      Memmodel : Interfaces.C.int) return Unsigned_16;
   pragma Import (Intrinsic, Atomic_Load_2, "__atomic_load_2");

   function Atomic_Load_4
     (Ptr : System.Address;
      Memmodel : Interfaces.C.int) return Unsigned_32;
   pragma Import (Intrinsic, Atomic_Load_4, "__atomic_load_4");

   function Atomic_Load_8
     (Ptr : System.Address;
      Memmodel : Interfaces.C.int) return Unsigned_64;
   pragma Import (Intrinsic, Atomic_Load_8, "__atomic_load_8");

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

   procedure Thread_Fence (Order : Memory_Order) is
   begin
      Atomic_Thread_Fence (Memory_Order'Pos (Order));
   end Thread_Fence;

   procedure Signal_Fence (Order : Memory_Order) is
   begin
      Atomic_Signal_Fence (Memory_Order'Pos (Order));
   end Signal_Fence;

   package body Atomic_Ts with
        Spark_Mode => Off is
      pragma Compile_Time_Error
        (not
         (Element_T'Size = 8 or
          Element_T'Size = 16 or
          Element_T'Size = 32 or
          Element_T'Size = 64),
         "Nonatomic size");

      --  We only use this when the element is the right size so it is safe.
      pragma Warnings (Off);
      function To_8 is new Ada.Unchecked_Conversion (Element_T, Unsigned_8);
      function From_8 is new Ada.Unchecked_Conversion (Unsigned_8, Element_T);

      function To_16 is new Ada.Unchecked_Conversion (Element_T, Unsigned_16);
      function From_16 is new Ada.Unchecked_Conversion
        (Unsigned_16,
         Element_T);

      function To_32 is new Ada.Unchecked_Conversion (Element_T, Unsigned_32);
      function From_32 is new Ada.Unchecked_Conversion
        (Unsigned_32,
         Element_T);

      function To_64 is new Ada.Unchecked_Conversion (Element_T, Unsigned_64);
      function From_64 is new Ada.Unchecked_Conversion
        (Unsigned_64,
         Element_T);
      pragma Warnings (On);

      procedure Store
        (A : in out Atomic;
         Element : Element_T;
         Order : Memory_Order := Memory_Order_Seq_Cst)
      is
         M : constant Interfaces.C.int := Memory_Order'Pos (Order);
      begin
         case A.Value'Size is
            when 8 =>
               Atomic_Store_1 (A.Value'Address, To_8 (Element), M);
            when 16 =>
               Atomic_Store_2 (A.Value'Address, To_16 (Element), M);
            when 32 =>
               Atomic_Store_4 (A.Value'Address, To_32 (Element), M);
            when 64 =>
               Atomic_Store_8 (A.Value'Address, To_64 (Element), M);
            when others =>
               raise Program_Error;
         end case;
      end Store;

      function Load
        (A : Atomic;
         Order : Memory_Order := Memory_Order_Seq_Cst) return Element_T
      is
         M : constant Interfaces.C.int := Memory_Order'Pos (Order);
      begin
         case A.Value'Size is
            when 8 =>
               return From_8 (Atomic_Load_1 (A.Value'Address, M));
            when 16 =>
               return From_16 (Atomic_Load_2 (A.Value'Address, M));
            when 32 =>
               return From_32 (Atomic_Load_4 (A.Value'Address, M));
            when 64 =>
               return From_64 (Atomic_Load_8 (A.Value'Address, M));
            when others =>
               raise Program_Error;
         end case;
      end Load;

      function Compare_Exchange_Weak
        (A : in out Atomic;
         Old_Element : Element_T;
         New_Element : Element_T;
         Success_Order : Memory_Order := Memory_Order_Seq_Cst;
         Failure_Order : Memory_Order := Memory_Order_Seq_Cst) return Boolean
      is
         M : constant Interfaces.C.int := Memory_Order'Pos (Success_Order);
         N : constant Interfaces.C.int := Memory_Order'Pos (Failure_Order);
         Expected : Element_T := Old_Element;
         Success : Boolean;
      begin
         case A.Value'Size is
            when 8 =>
               Success :=
                 Atomic_Compare_Exchange_1
                   (A.Value'Address,
                    Expected'Address,
                    To_8 (New_Element),
                    True,
                    M,
                    N);
            when 16 =>
               Success :=
                 Atomic_Compare_Exchange_2
                   (A.Value'Address,
                    Expected'Address,
                    To_16 (New_Element),
                    True,
                    M,
                    N);
            when 32 =>
               Success :=
                 Atomic_Compare_Exchange_4
                   (A.Value'Address,
                    Expected'Address,
                    To_32 (New_Element),
                    True,
                    M,
                    N);
            when 64 =>
               Success :=
                 Atomic_Compare_Exchange_8
                   (A.Value'Address,
                    Expected'Address,
                    To_64 (New_Element),
                    True,
                    M,
                    N);
            when others =>
               raise Program_Error;
         end case;
         return Success;
      end Compare_Exchange_Weak;

      function Compare_Exchange_Strong
        (A : in out Atomic;
         Old_Element : Element_T;
         New_Element : Element_T;
         Success_Order : Memory_Order := Memory_Order_Seq_Cst;
         Failure_Order : Memory_Order := Memory_Order_Seq_Cst) return Boolean
      is
         M : constant Interfaces.C.int := Memory_Order'Pos (Success_Order);
         N : constant Interfaces.C.int := Memory_Order'Pos (Failure_Order);
         Expected : Element_T := Old_Element;
         Success : Boolean;
      begin
         case A.Value'Size is
            when 8 =>
               Success :=
                 Atomic_Compare_Exchange_1
                   (A.Value'Address,
                    Expected'Address,
                    To_8 (New_Element),
                    False,
                    M,
                    N);
            when 16 =>
               Success :=
                 Atomic_Compare_Exchange_2
                   (A.Value'Address,
                    Expected'Address,
                    To_16 (New_Element),
                    False,
                    M,
                    N);
            when 32 =>
               Success :=
                 Atomic_Compare_Exchange_4
                   (A.Value'Address,
                    Expected'Address,
                    To_32 (New_Element),
                    False,
                    M,
                    N);
            when 64 =>
               Success :=
                 Atomic_Compare_Exchange_8
                   (A.Value'Address,
                    Expected'Address,
                    To_64 (New_Element),
                    False,
                    M,
                    N);
            when others =>
               raise Program_Error;
         end case;
         return Success;
      end Compare_Exchange_Strong;

      function Exchange
        (A : in out Atomic;
         New_Element : Element_T;
         Order : Memory_Order := Memory_Order_Seq_Cst) return Element_T
      is
         M : constant Interfaces.C.int := Memory_Order'Pos (Order);
         Old_Element : Element_T;
      begin
         case A.Value'Size is
            when 8 =>
               Old_Element :=
                 From_8
                   (Atomic_Exchange_1
                      (A.Value'Address,
                       To_8 (New_Element),
                       M));
            when 16 =>
               Old_Element :=
                 From_16
                   (Atomic_Exchange_2
                      (A.Value'Address,
                       To_16 (New_Element),
                       M));
            when 32 =>
               Old_Element :=
                 From_32
                   (Atomic_Exchange_4
                      (A.Value'Address,
                       To_32 (New_Element),
                       M));
            when 64 =>
               Old_Element :=
                 From_64
                   (Atomic_Exchange_8
                      (A.Value'Address,
                       To_64 (New_Element),
                       M));
            when others =>
               raise Program_Error;
         end case;
         return Old_Element;
      end Exchange;
   end Atomic_Ts;
end Linted.GCC_Atomics;
