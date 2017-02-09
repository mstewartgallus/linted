-- Copyright 2017 Steven Stewart-Gallus
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
with Libc.Sched;

with System.Machine_Code;

package body Linted.Sched is
   procedure Backoff (State : in out Backoff_State) with
      Spark_Mode => Off is
   begin
      if State < 2 then
         null;
      elsif State < 20 then
         System.Machine_Code.Asm ("pause", Volatile => True);
      else
         Libc.Sched.sched_yield;
      end if;

      if State /= Backoff_State'Last then
         State := State + 1;
      end if;
   end Backoff;
end Linted.Sched;
