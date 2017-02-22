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
with Interfaces.C;
with Interfaces;

with System.Storage_Elements;
with System;

with Linted.Reader;
with Linted.Stack;

package body Linted.Controls_Reader is
   package C renames Interfaces.C;
   package Storage_Elements renames System.Storage_Elements;

   use type Controls.Int;
   use type C.int;
   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;
   use type Errors.Error;

   type Live_Future is new Future range 1 .. Future'Last with
        Default_Value => 1;

   Read_Futures : array (Live_Future) of Reader.Future with
      Independent_Components;
   Data_Being_Read : array (Live_Future) of aliased Controls.Storage :=
     (others => (others => 16#7F#)) with
      Independent_Components;

   type Ix is mod Max_Nodes + 2;
   function Is_Valid (Element : Live_Future) return Boolean is (True);
   package Spare_Futures is new Stack (Live_Future, Ix, Is_Valid);

   function Is_Live (F : Future) return Boolean is (F /= 0);

   procedure Read
     (Object : KOs.KO;
      Signaller : Triggers.Signaller;
      F : out Future) with
      Spark_Mode => Off is
      Live : Live_Future;
   begin
      Spare_Futures.Pop (Live);
      Reader.Read
        (Object,
         Data_Being_Read (Live) (1)'Address,
         Controls.Storage_Size,
         Signaller,
         Read_Futures (Live));
      F := Future (Live);
   end Read;

   procedure Read_Wait (F : in out Future; E : out Event) is
      R_Event : Reader.Event;
      Live : Live_Future := Live_Future (F);
   begin
      Reader.Read_Wait (Read_Futures (Live), R_Event);
      declare
         N_Event : Event;
      begin
         N_Event.Err := R_Event.Err;
         if N_Event.Err = Errors.Success then
            Controls.From_Storage (Data_Being_Read (Live), N_Event.Data);
         end if;
         E := N_Event;
      end;
      Spare_Futures.Push (Live);
      F := 0;
   end Read_Wait;

   procedure Read_Poll
     (F : in out Future;
      E : out Event;
      Init : out Boolean)
   is
      R_Event : Reader.Event;
      Live : Live_Future := Live_Future (F);
   begin
      Reader.Read_Poll (Read_Futures (Live), R_Event, Init);
      if Init then
         declare
            N_Event : Event;
         begin
            N_Event.Err := R_Event.Err;

            if N_Event.Err = Errors.Success then
               Controls.From_Storage (Data_Being_Read (Live), N_Event.Data);
            end if;
            E := N_Event;
         end;
         Spare_Futures.Push (Live);
         F := 0;
      else
         declare
            Dummy : Event;
         begin
            E := Dummy;
         end;
      end if;
   end Read_Poll;
begin
   for II in 1 .. Max_Nodes loop
      Spare_Futures.Push (Live_Future (Future (II)));
   end loop;
end Linted.Controls_Reader;
