-- Copyright 2015,2016 Steven Stewart-Gallus
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
package body Linted.Angles is
   function To_Angle (X : Element_T) return Angle is
   begin
      return (Value => X);
   end To_Angle;

   function To_Angle (X : Element_T;
		      Y : Element_T) return Angle is
      Result : Element_T;
   begin
      if 0 = Y then
	 Result := X;
      else
	 Result := (Element_T'Last / Y) * X  + X / Y;
      end if;
      return (Value => Result);
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
      X := Theta - Phi;

      if From_Angle (X) > From_Angle (Min) then
	 return X;
      else
	 return Min;
      end if;
   end Subtract_Clamped;

   function Sin (X : Angle) return Element_U is
      type My_Float is digits 18;

      S : Element_U := 1;
      A : Element_T := From_Angle (X);
   begin
      if A > Element_T'Last / 2 then
   	 A := A - Element_T'Last / 2;
   	 S := -1;
      end if;
      if A > Element_T'Last / 4 then
   	 A := Element_T'Last / 2 - A;
      end if;

      -- Sin (x) = x - x^3/ 6 + x^5 / 120
      declare
   	 F : My_Float := My_Float (A) * (2.0 * 3.1459265359 / (My_Float (Element_T'Last) + 1.0));
   	 F_Cubed : My_Float := F * F * F;
   	 F_5 : My_Float :=  F_Cubed * (F * F);
   	 F_7 : My_Float :=  F_5 * (F * F);
   	 F_9 : My_Float :=  F_7 * (F * F);
   	 M : My_Float := F - F_Cubed / 6.0 + F_5 / 120.0 - F_7 / 5040.0 + F_9 / 362880.0;
   	 -- M : My_Float := F * 2.0 / 3.1459265359;
   	 N : My_Float := My_Float (Element_U'Last) * M;
   	 U : Element_U;
      begin
   	 if N > My_Float (Element_U'Last) then
   	    U := Element_U'Last;
   	 else
   	    U := Element_U (N);
   	 end if;
   	 return S * U;
      end;
   end Sin;

   function Cos (X : Angle) return Element_U is
      function My_Sin is new Sin (Element_U);
   begin
      return My_Sin (To_Angle (From_Angle (X) + Element_T'Last / 4));
   end Cos;
end Linted.Angles;
