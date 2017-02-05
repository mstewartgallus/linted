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
with Linted.Controls;
with Linted.Errors;
with Linted.KOs;
with Linted.Triggers;

package Linted.Controls_Reader is
   pragma Elaborate_Body;

   type Event is record
      Data : Controls.Packet;
      Err : Errors.Error := 0;
   end record;

   type Future is limited private with
      Preelaborable_Initialization;

   function Is_Live (F : Future) return Boolean;

   procedure Read
     (Object : KOs.KO;
      Signaller : Triggers.Signaller;
      F : out Future) with
      Post => Is_Live (F);

   procedure Read_Wait (F : in out Future; E : out Event) with
      Pre => Is_Live (F),
      Post => not Is_Live (F);

   procedure Read_Poll
     (F : in out Future;
      E : out Event;
      Init : out Boolean) with
      Pre => Is_Live (F),
      Post => (if Init then not Is_Live (F) else Is_Live (F));

private
   Max_Nodes : constant := 1;

   type Future is range 0 .. Max_Nodes + 1 with
        Default_Value => 0;
end Linted.Controls_Reader;
