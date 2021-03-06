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
with Ada.Real_Time;
with Linted.IO_Pool;
with Linted.Triggers;

package Linted.Timer is
   subtype Event is Linted.IO_Pool.Remind_Me_Event;
   subtype Future is Linted.IO_Pool.Remind_Me_Future;

   function Future_Is_Live
     (F : Future) return Boolean renames
     IO_Pool.Remind_Me_Future_Is_Live;

   procedure Remind_Me
     (Time : Ada.Real_Time.Time;
      Signaller : Triggers.Signaller;
      F : out Future) renames
     IO_Pool.Remind_Me;

   procedure Remind_Me_Wait
     (F : in out Future;
      E : out Event) renames
     IO_Pool.Remind_Me_Wait;

   procedure Remind_Me_Poll
     (F : in out Future;
      E : out Event;
      Init : out Boolean) renames
     IO_Pool.Remind_Me_Poll;
end Linted.Timer;
