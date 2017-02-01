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
with Linted.Queues;
with Linted.Wait_Lists;

package body Linted.Window_Notifier with
     Spark_Mode => Off is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;
   use type Errors.Error;

   Max_Nodes : constant := 1;

   Read_Futures : array (Future range 1 .. Max_Nodes) of Reader.Future;
   Data_Being_Read : array
   (Future range
      1 ..
        Max_Nodes) of aliased Storage_Elements.Storage_Array (1 .. 1) :=
     (others => (others => 16#7F#));

   package Future_Queues is new Queues (Future, Max_Nodes);

   Spare_Futures : Future_Queues.Queue;
   Future_Wait_List : Wait_Lists.Wait_List;
   function Is_Live (F : Future) return Boolean is (F /= 0);

   procedure Read
     (Object : KOs.KO;
      Signaller : Triggers.Signaller;
      F : out Future)
   is
      Init : Boolean;
   begin
      loop
         Future_Queues.Try_Dequeue (Spare_Futures, F, Init);
         if Init then
            exit;
         end if;
         Wait_Lists.Wait (Future_Wait_List);
      end loop;

      Reader.Read
        (Object,
         Data_Being_Read (F) (1)'Address,
         1,
         Signaller,
         Read_Futures (F));
   end Read;

   procedure Read_Wait (F : in out Future) is
      R_Event : Reader.Event;
   begin
      Reader.Read_Wait (Read_Futures (F), R_Event);
      pragma Assert (R_Event.Err = Errors.Success);
      Future_Queues.Enqueue (Spare_Futures, F);
      Wait_Lists.Signal (Future_Wait_List);
      F := 0;
   end Read_Wait;

   procedure Read_Poll (F : in out Future; Init : out Boolean) is
      R_Event : Reader.Event;
   begin
      Reader.Read_Poll (Read_Futures (F), R_Event, Init);
      if Init then
         pragma Assert (R_Event.Err = Errors.Success);
         Future_Queues.Enqueue (Spare_Futures, F);
         Wait_Lists.Signal (Future_Wait_List);
         F := 0;
      end if;
   end Read_Poll;
begin
   for II in 1 .. Max_Nodes loop
      Future_Queues.Enqueue (Spare_Futures, Future (II));
   end loop;
end Linted.Window_Notifier;
