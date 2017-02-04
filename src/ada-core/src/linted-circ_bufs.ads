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
   type Ix is mod <>;
package Linted.Circ_Bufs with
   Spark_Mode => Off is
   pragma Pure;

   type Elements_Array is array (Ix) of Element_T;

   protected type Circ_Buf is
   private
      procedure Try_Enqueue (Element : Element_T; Success : out Boolean) with
         Global => null,
         Depends => (Circ_Buf => (Circ_Buf, Element), Success => Circ_Buf);
      procedure Try_Dequeue
        (Element : out Element_T;
         Success : out Boolean) with
         Global => null,
         Depends =>
         (Circ_Buf => Circ_Buf,
          Element => Circ_Buf,
          Success => Circ_Buf);

      First : Ix := Ix'First;
      Last : Ix := Ix'First;
      Full : Boolean := False;
      Elements : Elements_Array;
   end Circ_Buf;

   procedure Try_Enqueue
     (Buf : in out Circ_Buf;
      Element : Element_T;
      Success : out Boolean) with
      Global => null,
      Depends => (Buf => (Buf, Element), Success => Buf);

   procedure Try_Dequeue
     (Buf : in out Circ_Buf;
      Element : out Element_T;
      Success : out Boolean) with
      Global => null,
      Depends => (Buf => Buf, Element => Buf, Success => Buf);

end Linted.Circ_Bufs;
