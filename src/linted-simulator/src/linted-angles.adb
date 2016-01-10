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
private with Ada.Assertions;
private with Ada.Numerics;
private with Ada.Numerics.Generic_Elementary_Functions;

package body Linted.Angles is
   package Assertions renames Ada.Assertions;

   function To_Angle (X : Element_T) return Angle is
   begin
      return (Value => X);
   end To_Angle;

   function To_Angle (X : Element_T;
		     Y : Element_T) return Angle is
   begin
      return (Value => (Element_T'Last / Y) * X  + X / Y);
   end To_Angle;

   function From_Angle (X : Angle) return Element_T is
   begin
      return X.Value;
   end From_Angle;

   function "+" (Theta : Angle; Phi : Angle) return Angle is
   begin
      return To_Angle (From_Angle (Theta) + From_Angle (Phi));
   end "+";

   function "-" (Theta : Angle; Phi : Angle) return Angle is
   begin
      return To_Angle (From_Angle (Theta) - From_Angle (Phi));
   end "-";

   function Add_Clamped (Min : Angle;
			Max : Angle;
			Theta : Angle;
			Phi : Angle) return Angle is
      X : Angle;
   begin
      Assertions.Assert (From_Angle (Max) <= Element_T'Last / 2);
      Assertions.Assert (From_Angle (Min) <= Element_T'Last / 2);

      X := Theta + Phi;

      if From_Angle (X) > From_Angle (Max) then
	 return Max;
      else
	 return X;
      end if;
   end Add_Clamped;

   function Subtract_Clamped (Min : Angle;
			     Max : Angle;
			     Theta : Angle;
			     Phi : Angle) return Angle is
      X : Angle;
   begin
      Assertions.Assert (From_Angle (Max) <= Element_T'Last / 2);
      Assertions.Assert (From_Angle (Min) <= Element_T'Last / 2);

      X := Theta - Phi;

      if From_Angle (X) > From_Angle (Min) then
	 return X;
      else
	 return Min;
      end if;
   end Subtract_Clamped;

   function Sin (X : Angle) return Element_U is
      type My_Float is digits 18;
      package XX is new Ada.Numerics.Generic_Elementary_Functions (My_Float);
   begin
      return Element_U (XX.Sin (My_Float (From_Angle (X)), Cycle => My_Float (Element_T'Last) + 1.0) * My_Float (Element_U'Last));
   end Sin;

   function Cos (X : Angle) return Element_U is
      type My_Float is digits 18;
      package XX is new Ada.Numerics.Generic_Elementary_Functions (My_Float);
   begin
      return Element_U (XX.Sin (My_Float (From_Angle (X) + Element_T'Last / 4), Cycle => My_Float (Element_T'Last) + 1.0) * My_Float (Element_U'Last));
   end Cos;
end Linted.Angles;
