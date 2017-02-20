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
   with function Is_Valid (Element : Element_T) return Boolean;
package Linted.Lock_Free_Stack with
   Initializes => State,
   Abstract_State => (State with External) is
   pragma Elaborate_Body;

   procedure Try_Push (Element : Element_T; Success : out Boolean) with
      Global => (In_Out => State),
      Depends => (State =>+ Element, Success => State),
      Pre => Is_Valid (Element);

   procedure Try_Pop (Element : out Element_T; Success : out Boolean) with
      Global => (In_Out => State),
      Depends => (State =>+ null, Element => State, Success => State),
      Post => (if Success then Is_Valid (Element));
end Linted.Lock_Free_Stack;
