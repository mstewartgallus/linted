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

-- Memory ordering stuff isn't understood by SPARK and can be used to
-- produce unsafe code.
   package Linted.GCC_Atomics with
     Spark_Mode => Off is
   pragma Pure;

   type Memory_Order is
     (Memory_Order_Relaxed,
      Memory_Order_Consume,
      Memory_Order_Acquire,
      Memory_Order_Release,
      Memory_Order_Acq_Rel,
      Memory_Order_Seq_Cst);

   procedure Thread_Fence (Order : Memory_Order) with
      Inline_Always;
   procedure Signal_Fence (Order : Memory_Order) with
      Inline_Always;

   generic
      type Element_T is private;
   package Atomic_Ts with
      Spark_Mode => Off is
      type Atomic is limited private;

      procedure Store
        (A : in out Atomic;
         Element : Element_T;
         Order : Memory_Order := Memory_Order_Seq_Cst) with
         Inline_Always;
      function Load
        (A : Atomic;
         Order : Memory_Order := Memory_Order_Seq_Cst) return Element_T with
         Inline_Always;
      function Compare_Exchange_Strong
        (A : in out Atomic;
         Old_Element : Element_T;
         New_Element : Element_T;
         Success_Order : Memory_Order := Memory_Order_Seq_Cst;
         Failure_Order : Memory_Order :=
           Memory_Order_Seq_Cst)
         return Boolean with
         Inline_Always;
      function Compare_Exchange_Weak
        (A : in out Atomic;
         Old_Element : Element_T;
         New_Element : Element_T;
         Success_Order : Memory_Order := Memory_Order_Seq_Cst;
         Failure_Order : Memory_Order :=
           Memory_Order_Seq_Cst)
         return Boolean with
         Inline_Always;

      function Exchange
        (A : in out Atomic;
         New_Element : Element_T;
         Order : Memory_Order := Memory_Order_Seq_Cst) return Element_T with
         Inline_Always;
   private
      -- A record has to be used so that it is passed by pointer
      type Atomic is record
         Value : Element_T with
            Atomic;
      end record;
      pragma Convention (C, Atomic);
   end Atomic_Ts;
end Linted.GCC_Atomics;
