-- Copyright 2015 Steven Stewart-Gallus
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
with Linted.Options;

generic
   type Element_T is private;
package Linted.MVars is
   pragma Pure;

   package Option_Element_Ts is new Linted.Options (Element_T);

   type MVar is limited private;

   function Poll (This : in out MVar) return Option_Element_Ts.Option;
   procedure Set (This : in out MVar; D : Element_T);

private
   type Atomic_Boolean is new Boolean;
   pragma Atomic (Atomic_Boolean);

   protected type Impl is
      procedure Poll (Option : out Option_Element_Ts.Option; Full : not null access Atomic_Boolean);
      procedure Set (D : Element_T);
   private
      Current : Element_T;
   end Impl;

   type MVar is limited record
      Object : Impl;
      Full : aliased Atomic_Boolean := False;
   end record;
end Linted.MVars;
