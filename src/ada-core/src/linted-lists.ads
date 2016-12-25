-- Copyright 2016 Steven Stewart-Gallus
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
package Linted.Lists with
   Spark_Mode => On is
   type Node is limited private;
   type Node_Access is limited private;

   function Is_Free (N : Node_Access) return Boolean;
   function Is_Null (N : Node_Access) return Boolean;

   generic
      Initial_Count : Positive;
   package Pool with
      Abstract_State => (State with External) is
      procedure Allocate (N : out Node_Access) with
         Global => (In_Out => State),
         Depends => (State => State, N => State),
         Post => Is_Free (N);
   end Pool;

   protected type List is
      procedure Insert (C : Element_T; N : Node_Access) with
         Global => null,
         Depends => (List => (List, C, N)),
         Pre => Is_Free (N) and not Is_Null (N);
      procedure Remove (C : out Element_T; N : out Node_Access) with
         Global => null,
         Depends => (List => List, C => List, N => List),
         Post => Is_Free (N);
   private
      Head : Node_Access;
   end List;
private
   pragma SPARK_Mode (Off);

   type Atomic_Boolean is new Boolean with
        Atomic;
   type Atomic_Node_Access is access all Node with
        Atomic;
   type Node_Access is access all Node;

   type Node is record
      Contents : Element_T;
      Tail : Atomic_Node_Access;
      In_List : Atomic_Boolean := False;
   end record;
end Linted.Lists;
