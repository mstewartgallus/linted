-- Copyright 2015 Steven Stewart-Gallus
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http ://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
generic
   type Element_T is mod <>;
package Linted.Angles is
   pragma Pure;

   type Angle is private;

   function To_Angle (X : Element_T) return Angle;
   function To_Angle (X : Element_T;
		      Y : Element_T) return Angle;
   function From_Angle (X : Angle) return Element_T;

   function "+" (Theta : Angle; Phi : Angle) return Angle;
   function "-" (Theta : Angle; Phi : Angle) return Angle;

   function Add_Clamped (Min : Angle;
			 Max : Angle;
			 Theta : Angle;
			 Phi : Angle) return Angle;
   function Subtract_Clamped (Min : Angle;
			      Max : Angle;
			      Theta : Angle;
			      Phi : Angle) return Angle;

   generic
      type Element_U is range <>;
   function Sin (X : Angle) return Element_U;

   generic
      type Element_U is range <>;
   function Cos (X : Angle) return Element_U;
private
   type Angle is record
      Value : Element_T;
   end record;

end Linted.Angles;
