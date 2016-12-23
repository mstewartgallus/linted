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

   function To_Angle (X : Element_T; Y : Element_T) return Angle is
      Result : Element_T;
   begin
      if 0 = Y then
         Result := X;
      else
         Result := (Element_T'Last / Y) * X + X / Y;
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

   function Add_Clamped
     (Min : Angle;
      Max : Angle;
      Theta : Angle;
      Phi : Angle) return Angle
   is
      X : Angle;
   begin
      X := Theta + Phi;

      if From_Angle (X) > From_Angle (Max) then
         return Max;
      else
         return X;
      end if;
   end Add_Clamped;

   function Subtract_Clamped
     (Min : Angle;
      Max : Angle;
      Theta : Angle;
      Phi : Angle) return Angle
   is
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

      Tau : constant My_Float := 2.0 * 3.1459265358979324;

      function Core (F : My_Float) return My_Float with
         Pre => F >= 0.0 and F <= Tau / 4.0,
         Post => Core'Result >= 0.0 and Core'Result <= 1.0;

         -- Sin (x) = x - x^3/ 6 + x^5 / 120
      function Core (F : My_Float) return My_Float is
         F_2 : My_Float;
         X : My_Float;
         Y : My_Float;
         Z : My_Float;
         W : My_Float;
      begin
         F_2 := F * F;
         pragma Assert (F_2 >= 0.0);
         pragma Assert (F_2 <= 2.47421344232);

         X := 1.0 / 5040.0 + F_2 / 362880.0;
         pragma Assert (X <= 2.05230967379E-4);
         pragma Assert (X >= 1.98412698412E-4);

         Y := 1.0 / 120.0 - X * F_2;
         pragma Assert (Y >= 7.82554811505E-3);
         pragma Assert (Y <= 8.33333333334E-3);

         Z := 1.0 / 6.0 + Y * F_2;
         pragma Assert (Z >= 0.166666666666);
         pragma Assert (Z <= 0.18728511203);

         W := 1.0 - Z * F_2;
         pragma Assert (W >= 0.536616658268);
         pragma Assert (W <= 1.0);

         return F * W;
         -- M : My_Float := F * 2.0 / 3.1459265359;
      end Core;

      Sign : Element_U := 1;
      My_Input_Angle : Element_T;
      Result_F : My_Float;
      Result : Element_U;
   begin
      My_Input_Angle := From_Angle (X);

      if My_Input_Angle >= Element_T'Last / 2 + 1 then
         My_Input_Angle := My_Input_Angle - (Element_T'Last / 2 + 1);
         Sign := -1;
      end if;
      pragma Assert (My_Input_Angle < Element_T'Last / 2 + 1);

      if My_Input_Angle >= Element_T'Last / 4 + 1 then
         My_Input_Angle := Element_T'Last / 2 - My_Input_Angle;
      end if;
      pragma Assert (My_Input_Angle < Element_T'Last / 4 + 1);

      Result_F :=
        My_Float (Element_U'Last) *
        Core
          (My_Float (My_Input_Angle) *
           (Tau / (My_Float (Element_T'Last) + 1.0)));

      if Result_F > My_Float (Element_U'Last) then
         Result := Element_U'Last;
      else
         Result := Element_U (Result_F);
      end if;
      return Sign * Result;
   end Sin;

   function Cos (X : Angle) return Element_U is
      function My_Sin is new Sin (Element_U);
   begin
      return My_Sin (To_Angle (From_Angle (X) + (Element_T'Last / 4)));
   end Cos;
end Linted.Angles;
