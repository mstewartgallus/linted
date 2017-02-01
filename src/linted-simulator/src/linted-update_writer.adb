-- Copyright 2015,2016,2017 Steven Stewart-Gallus
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

with Linted.Writer;
with Linted.Queues;
with Linted.Wait_Lists;

package body Linted.Update_Writer with
     Spark_Mode => Off is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type Interfaces.Unsigned_32;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type Errors.Error;

   Max_Nodes : constant := 1;

   Write_Futures : array (Future range 1 .. Max_Nodes) of Writer.Future;
   Data_Being_Written : array
   (Future range 1 .. Max_Nodes) of aliased Update.Storage :=
     (others => (others => 16#7F#));

   package Future_Queues is new Queues (Future, Max_Nodes);

   Spare_Futures : Future_Queues.Queue;
   Future_Wait_List : Wait_Lists.Wait_List;

   function Is_Live (F : Future) return Boolean is (F /= 0);

   procedure Write
     (Object : KOs.KO;
      U : Update.Packet;
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
      Update.To_Storage (U, Data_Being_Written (F));
      Writer.Write
        (Object,
         Data_Being_Written (F) (1)'Address,
         Update.Storage_Size,
         Signaller,
         Write_Futures (F));
   end Write;

   procedure Write_Wait (F : in out Future; E : out Errors.Error) is
      W_Event : Writer.Event;
   begin
      Writer.Write_Wait (Write_Futures (F), W_Event);
      E := W_Event.Err;
      Future_Queues.Enqueue (Spare_Futures, F);
      Wait_Lists.Signal (Future_Wait_List);
      F := 0;
   end Write_Wait;

   procedure Write_Poll
     (F : in out Future;
      E : out Errors.Error;
      Init : out Boolean)
   is
      W_Event : Writer.Event;
   begin
      Writer.Write_Poll (Write_Futures (F), W_Event, Init);
      if Init then
         E := W_Event.Err;
         Future_Queues.Enqueue (Spare_Futures, F);
         Wait_Lists.Signal (Future_Wait_List);
         F := 0;
      else
         E := Errors.Success;
      end if;
   end Write_Poll;

begin
   for II in 1 .. Max_Nodes loop
      Future_Queues.Enqueue (Spare_Futures, Future (II));
   end loop;
end Linted.Update_Writer;
