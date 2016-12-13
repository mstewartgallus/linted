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
with Ada.Real_Time;
with Ada.Synchronous_Task_Control;

with Linted.Options;

package Linted.Timer with SPARK_Mode => Off Is
   pragma Elaborate_Body;

   type Event is record
      null;
   end record;

   package Option_Events is new Linted.Options (Event);

   generic
   package Worker is
      procedure Wait_Until (Time : Ada.Real_Time.Time);
      function Poll return Option_Events.Option;
      procedure Wait;
   end Worker;
end Linted.Timer;
