-- Copyright 2015,2016 Steven Stewart-Gallus
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
with Interfaces;

package body Linted.Update with
     Spark_Mode => Off is
   package Storage_Elements renames System.Storage_Elements;

   use type Interfaces.Unsigned_32;
   use type Storage_Elements.Storage_Offset;

   subtype Tuple is Storage_Elements.Storage_Array (1 .. 4);

   function To_Nat (Number : Int) return Nat;
   function To_Bytes (Number : Nat) return Tuple;
   function To_Int (T : Tuple) return Int;
   function From_Bytes (T : Tuple) return Nat;

   -- Big Endian
   function To_Bytes (Number : Nat) return Tuple is
   begin
      return
        (Storage_Elements.Storage_Element
           (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 24) and
            16#FF#),
         Storage_Elements.Storage_Element
           (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 16) and
            16#FF#),
         Storage_Elements.Storage_Element
           (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Number), 8) and
            16#FF#),
         Storage_Elements.Storage_Element (Number and 16#FF#));
   end To_Bytes;

   function From_Bytes (T : Tuple) return Nat is
   begin
      return Nat
          (Interfaces.Unsigned_32 (T (4)) or
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (3)), 8) or
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (2)), 16) or
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (T (1)), 24));
   end From_Bytes;

   function To_Int (T : Tuple) return Int is
      X : Nat;
      Y : Int;
   begin
      X := From_Bytes (T);
      if X <= Nat (Int'Last) then
         Y := Int (X);
      else
         Y := -Int (not X) - 1;
      end if;
      return Y;
   end To_Int;

   function To_Nat (Number : Int) return Nat is
      Y : Nat;
   begin
      if Number < 0 then
         Y := Nat (Number - Int'First) - (Nat (Int'Last) + 1);
      else
         Y := Nat (Number);
      end if;
      return Y;
   end To_Nat;

   procedure From_Storage (S : Storage; U : out Update.Packet) is
   begin
      U.X_Position := To_Int (S (1 .. 4));
      U.Y_Position := To_Int (S (5 .. 8));
      U.Z_Position := To_Int (S (9 .. 12));
      U.MX_Position := To_Int (S (13 .. 16));
      U.MY_Position := To_Int (S (17 .. 20));
      U.MZ_Position := To_Int (S (21 .. 24));
      U.Z_Rotation := From_Bytes (S (25 .. 28));
      U.X_Rotation := From_Bytes (S (29 .. 32));
   end From_Storage;

   procedure To_Storage (U : Update.Packet; S : out Storage) is
   begin
      S (1 .. 4) := To_Bytes (To_Nat (U.X_Position));
      S (5 .. 8) := To_Bytes (To_Nat (U.Y_Position));
      S (9 .. 12) := To_Bytes (To_Nat (U.Z_Position));

      S (13 .. 16) := To_Bytes (To_Nat (U.MX_Position));
      S (17 .. 20) := To_Bytes (To_Nat (U.MY_Position));
      S (21 .. 24) := To_Bytes (To_Nat (U.MZ_Position));

      S (25 .. 28) := To_Bytes (U.Z_Rotation);
      S (29 .. 32) := To_Bytes (U.X_Rotation);
   end To_Storage;

end Linted.Update;
