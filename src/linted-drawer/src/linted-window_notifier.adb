-- Copyright 2016, 2017 Steven Stewart-Gallus
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
with Interfaces.C;

with System;
with System.Storage_Elements;

with Linted.Errors;
with Linted.Reader;
with Linted.Queue;

package body Linted.Window_Notifier is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;
   use type Errors.Error;

   type Live_Future is new Future range 1 .. Future'Last with
        Default_Value => 1;

   Read_Futures : array (Live_Future) of Reader.Future;
   Data_Being_Read : array
   (Live_Future) of aliased Storage_Elements.Storage_Array (1 .. 1) :=
     (others => (others => 16#7F#));

   type Ix is mod Max_Nodes + 1;
   package Spare_Futures is new Queue (Live_Future, Ix);

   function Is_Live (F : Future) return Boolean is (F /= 0);

   procedure Read
     (Object : KOs.KO;
      Signaller : Triggers.Signaller;
      F : out Future) with
      Spark_Mode => Off is
      Live : Live_Future;
   begin
      Spare_Futures.Dequeue (Live);
      Reader.Read
        (Object,
         Data_Being_Read (Live) (1)'Address,
         1,
         Signaller,
         Read_Futures (Live));
      F := Future (Live);
   end Read;

   procedure Read_Wait (F : in out Future) is
      R_Event : Reader.Event;
      Live : Live_Future := Live_Future (F);
   begin
      Reader.Read_Wait (Read_Futures (Live), R_Event);
      pragma Assert (R_Event.Err = Errors.Success);
      Spare_Futures.Enqueue (Live);
      F := 0;
   end Read_Wait;

   procedure Read_Poll (F : in out Future; Init : out Boolean) is
      R_Event : Reader.Event;
      Live : Live_Future := Live_Future (F);
   begin
      Reader.Read_Poll (Read_Futures (Live), R_Event, Init);
      if Init then
         pragma Assert (R_Event.Err = Errors.Success);
         Spare_Futures.Enqueue (Live);
         F := 0;
      end if;
   end Read_Poll;
begin
   for II in 1 .. Max_Nodes loop
      Spare_Futures.Enqueue (Live_Future (Future (II)));
   end loop;
end Linted.Window_Notifier;
