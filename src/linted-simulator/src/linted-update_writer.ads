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
with Linted.Errors;
with Linted.KOs;
with Linted.Update;
with Linted.Triggers;

package Linted.Update_Writer is
   pragma Elaborate_Body;

   type Future is limited private with
      Preelaborable_Initialization;

   function Is_Live (F : Future) return Boolean;

   procedure Write
     (Object : KOs.KO;
      U : Update.Packet;
      Signaller : Triggers.Signaller;
      F : out Future) with
      Post => Is_Live (F);

   procedure Write_Wait (F : in out Future; E : out Errors.Error) with
      Pre => Is_Live (F),
      Post => not Is_Live (F);

   procedure Write_Poll
     (F : in out Future;
      E : out Errors.Error;
      Init : out Boolean) with
      Pre => Is_Live (F),
      Post => (if Init then not Is_Live (F) else Is_Live (F));

private
   Max_Nodes : constant := 1;

   type Future is range 0 .. Max_Nodes + 1 with
        Default_Value => 0;
end Linted.Update_Writer;
