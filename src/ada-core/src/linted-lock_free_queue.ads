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
with Ada.Real_Time;

generic
   type Element_T is private;
   type Ix is mod <>;
   with function Is_Valid (Element : Element_T) return Boolean;
package Linted.Lock_Free_Queue with
   Initializes => (State => Ada.Real_Time.Clock_Time),
   Abstract_State => (State with External) is
   pragma Elaborate_Body;

   procedure Try_Enqueue (Element : Element_T; Success : out Boolean) with
      Global => (Input => Ada.Real_Time.Clock_Time, In_Out => State),
      Depends =>
      (State => (State, Element),
       Success => State,
       null => Ada.Real_Time.Clock_Time),
      Pre => Is_Valid (Element);

   procedure Try_Dequeue (Element : out Element_T; Success : out Boolean) with
      Global => (Input => Ada.Real_Time.Clock_Time, In_Out => State),
      Depends =>
      ((State, Element) => State,
       Success => State,
       null => Ada.Real_Time.Clock_Time),
      Post => (if Success then Is_Valid (Element));
end Linted.Lock_Free_Queue;
