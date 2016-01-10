-- Copyright 2015 Steven Stewart-Gallus
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
private with Ada.Synchronous_Task_Control;

package Linted.Triggers is
   pragma Preelaborate;

   type Trigger is limited private;

   procedure Wait (This : in out Trigger);
   procedure Signal (This : in out Trigger);

private
   type Trigger is limited record
      Object : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;
end Linted.Triggers;
