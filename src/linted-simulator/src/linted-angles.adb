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
with SPARK.Mod_Arithmetic_Lemmas;

package body Linted.Angles is
   package Mod_Lemmas is new SPARK.Mod_Arithmetic_Lemmas (Element_T);

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
      Tau : constant Element_U := 2.0 * 3.1459265358979324;

      function Core (F : Element_U) return Element_U with
         Pre => F >= 0.0 and F <= Tau / 4.0,
         Post => Core'Result >= 0.0 and Core'Result <= 1.0;

         -- Sin (x) = x - x^3/ 6 + x^5 / 120
         -- Sin (x) = ax + bx^3 + cx^5
         -- Sin (T/4) = 1 = a T/4 + b T^3/ 64 + c T^5 / 1024
         -- Sin' (T/4) = 0 = a + b 3T^2 / 16 + c 5 T^4 / 256
         -- Sin'' (T/4) = -1 = b 6 T/4 + c 20 T^3 / 64

      function Core (F : Element_U) return Element_U is
         F_2 : Element_U;
         X : Element_U;
         Y : Element_U;
         Z : Element_U;
      begin
         F_2 := F * F;
         pragma Assert (F_2 >= 0.0);
         pragma Assert (F_2 <= 39.4784176044);

         X := Element_U (0.00823993528) - Element_U (0.000172692635) * F_2;
         pragma Assert (X <= 0.00823993528);
         pragma Assert (X >= 1.42230331827E-3);

         Y := Element_U (-0.166514495) + F_2 * X;
         pragma Assert (Y <= 0.158785111017);
         pragma Assert (Y >= -0.166514495);

         Z := Element_U (0.99906670) + F_2 * Y;
         pragma Assert (Z <= 7.26765162209);
         pragma Assert (Z >= -5.6);

         return F * Z;
--       return F * (Element_U (0.99906670) - Element_U (0.166514495) * F_2 + Element_U (0.00823993528) * F_4 - Element_U (0.000172692635) * F_6);
         -- M : Element_U := F * 2.0 / 3.1459265359;
      end Core;

      Sign : Element_U := 1.0;
      My_Input_Angle : Element_T;
      Result : Element_U;
   begin
      My_Input_Angle := From_Angle (X);

      declare
         Half : Element_T := Element_T'Last / 2 + 1;
      begin
         if My_Input_Angle >= Half then
            Mod_Lemmas.Lemma_Div_Is_Monotonic
              (My_Input_Angle,
               Element_T'Last,
               2);
            My_Input_Angle := My_Input_Angle - Half;
            Sign := -1.0;
         end if;
         pragma Assert (My_Input_Angle < Half);
      end;

      if My_Input_Angle >= Element_T'Last / 4 + 1 then
         My_Input_Angle := Element_T'Last / 2 - My_Input_Angle;
      end if;
      pragma Assert (My_Input_Angle < Element_T'Last / 4 + 1);

      Result :=
        Core
          (Element_U (My_Input_Angle) *
           Element_U (Tau / Element_U (Element_U (Element_T'Last) + 1.0)));
      return Sign * Result;
   end Sin;

   function Cos (X : Angle) return Element_U is
      function My_Sin is new Sin (Element_U);
   begin
      return My_Sin (To_Angle (From_Angle (X) + (Element_T'Last / 4)));
   end Cos;
end Linted.Angles;
