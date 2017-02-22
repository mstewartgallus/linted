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
with Linted.Stack;

package body Linted.Update_Writer is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type Interfaces.Unsigned_32;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type Errors.Error;

   type Live_Future is new Future range 1 .. Future'Last with
        Default_Value => 1;

   Write_Futures : array (Live_Future) of Writer.Future with
      Independent_Components;
   Data_Being_Written : array (Live_Future) of aliased Update.Storage :=
     (others => (others => 16#7F#)) with
      Independent_Components;

   type Ix is mod Max_Nodes + 2;
   function Is_Valid (Element : Live_Future) return Boolean is (True);
   package Spare_Futures is new Stack (Live_Future, Ix, Is_Valid);

   function Is_Live (F : Future) return Boolean is (F /= 0);

   procedure Write
     (Object : KOs.KO;
      U : Update.Packet;
      Signaller : Triggers.Signaller;
      F : out Future) with
      Spark_Mode => Off is
      Live : Live_Future;
   begin
      Spare_Futures.Pop (Live);
      Update.To_Storage (U, Data_Being_Written (Live));
      Writer.Write
        (Object,
         Data_Being_Written (Live) (1)'Address,
         Update.Storage_Size,
         Signaller,
         Write_Futures (Live));
      F := Future (Live);
   end Write;

   procedure Write_Wait (F : in out Future; E : out Errors.Error) is
      W_Event : Writer.Event;
      Live : Live_Future := Live_Future (F);
   begin
      Writer.Write_Wait (Write_Futures (Live), W_Event);
      E := W_Event.Err;
      Spare_Futures.Push (Live);
      F := 0;
   end Write_Wait;

   procedure Write_Poll
     (F : in out Future;
      E : out Errors.Error;
      Init : out Boolean)
   is
      W_Event : Writer.Event;
      Live : Live_Future := Live_Future (F);
   begin
      Writer.Write_Poll (Write_Futures (Live), W_Event, Init);
      if Init then
         E := W_Event.Err;
         Spare_Futures.Push (Live);
         F := 0;
      else
         E := Errors.Success;
      end if;
   end Write_Poll;

begin
   for II in 1 .. Max_Nodes loop
      Spare_Futures.Push (Live_Future (Future (II)));
   end loop;
end Linted.Update_Writer;
