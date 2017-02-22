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

   type Memory_Order is
     (Memory_Order_Relaxed,
      Memory_Order_Consume,
      Memory_Order_Acquire,
      Memory_Order_Release,
      Memory_Order_Acq_Rel,
      Memory_Order_Seq_Cst);

   procedure Store
     (A : in out Atomic;
      Element : Element_T;
      Memory_Model : Memory_Order := Memory_Order_Seq_Cst) with
      Inline_Always,
      Global => null,
      Depends => (A =>+ (Element, Memory_Model));
   function Load
     (A : Atomic;
      Memory_Model : Memory_Order :=
        Memory_Order_Seq_Cst)
      return Element_T with
      Inline_Always,
      Global => null,
      Depends => (Load'Result => (A, Memory_Model));
   procedure Compare_Exchange_Strong
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean;
      Success_Memory_Model : Memory_Order := Memory_Order_Seq_Cst;
      Failure_Memory_Model : Memory_Order := Memory_Order_Seq_Cst) with
      Inline_Always,
      Global => null,
      Depends =>
      (Success => (A, Old_Element, Success_Memory_Model),
       A =>+ (Old_Element, New_Element, Failure_Memory_Model));
   procedure Compare_Exchange_Weak
     (A : in out Atomic;
      Old_Element : Element_T;
      New_Element : Element_T;
      Success : out Boolean;
      Success_Memory_Model : Memory_Order := Memory_Order_Seq_Cst;
      Failure_Memory_Model : Memory_Order := Memory_Order_Seq_Cst) with
      Inline_Always,
      Global => null,
      Depends =>
      (Success => (A, Old_Element, Success_Memory_Model),
       A =>+ (Old_Element, New_Element, Failure_Memory_Model));

   procedure Exchange
     (A : in out Atomic;
      Old_Element : out Element_T;
      New_Element : Element_T;
      Memory_Model : Memory_Order := Memory_Order_Seq_Cst) with
      Inline_Always,
      Global => null,
      Depends =>
      (Old_Element => (A, Memory_Model),
       A =>+ (New_Element, Memory_Model));
private
   -- A record has to be used so that it is passed by pointer
   type Atomic is record
      Value : Element_T with
         Atomic;
   end record with
      Volatile;
   pragma Convention (C, Atomic);
end Linted.XAtomics;
