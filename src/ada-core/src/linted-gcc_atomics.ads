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
package Linted.GCC_Atomics with
     Spark_Mode is
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
      Spark_Mode is
      type Atomic is limited private;

      procedure Store
        (A : in out Atomic;
         Element : Element_T;
         Order : Memory_Order := Memory_Order_Seq_Cst) with
         Inline_Always,
         Global => null,
         Depends => (A =>+ (Element, Order));
      function Load
        (A : Atomic;
         Order : Memory_Order := Memory_Order_Seq_Cst) return Element_T with
         Inline_Always,
         Global => null,
         Depends => (Load'Result => (A, Order));
      procedure Compare_Exchange_Strong
        (A : in out Atomic;
         Old_Element : Element_T;
         New_Element : Element_T;
         Success : out Boolean;
         Success_Order : Memory_Order := Memory_Order_Seq_Cst;
         Failure_Order : Memory_Order := Memory_Order_Seq_Cst) with
         Inline_Always,
         Global => null,
         Depends =>
         (Success => (A, Old_Element, Success_Order),
          A =>+ (Old_Element, New_Element, Failure_Order));
      procedure Compare_Exchange_Weak
        (A : in out Atomic;
         Old_Element : Element_T;
         New_Element : Element_T;
         Success : out Boolean;
         Success_Order : Memory_Order := Memory_Order_Seq_Cst;
         Failure_Order : Memory_Order := Memory_Order_Seq_Cst) with
         Inline_Always,
         Global => null,
         Depends =>
         (Success => (A, Old_Element, Success_Order),
          A =>+ (Old_Element, New_Element, Failure_Order));

      procedure Exchange
        (A : in out Atomic;
         Old_Element : out Element_T;
         New_Element : Element_T;
         Order : Memory_Order := Memory_Order_Seq_Cst) with
         Inline_Always,
         Global => null,
         Depends => (Old_Element => (A, Order), A =>+ (New_Element, Order));
   private
      -- A record has to be used so that it is passed by pointer
      type Atomic is record
         Value : Element_T with
            Atomic;
      end record;
      pragma Convention (C, Atomic);
   end Atomic_Ts;
end Linted.GCC_Atomics;
