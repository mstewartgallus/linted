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
package Linted.Sched is
   pragma Preelaborate;

   type Backoff_State is private;

   procedure Backoff (State : in out Backoff_State) with
      Spark_Mode;
private
   type Backoff_State is range 0 .. 2**32 with
        Default_Value => 0;
end Linted.Sched;
