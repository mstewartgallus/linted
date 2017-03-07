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
with Linted.Mod_Atomics;

package Linted.Sched with
     Spark_Mode is
   pragma Preelaborate;

   type Contention_T is mod 2**32 with
        Default_Value => 0;
   package Contention_Atomics is new Mod_Atomics (Contention_T);

   subtype Contention is Contention_Atomics.Atomic;

   type Backoff_State is mod 2**32 with
        Default_Value => 0;

   procedure Backoff (State : in out Backoff_State) with
      Inline_Always,
      Global => null,
      Depends => (State => State);

   procedure Success (C : in out Contention) with
      Inline_Always,
      Global => null,
      Depends => (C => C);

   procedure Backoff (C : in out Contention) with
      Inline_Always,
      Global => (null),
      Depends => (C => C);

   procedure Backoff
     (C : in out Contention;
      Highly_Contended : out Boolean) with
      Inline_Always,
      Global => (null),
      Depends => ((Highly_Contended, C) => C);

   procedure Pause;
   pragma Import (Intrinsic, Pause, "__builtin_ia32_pause");
end Linted.Sched;
