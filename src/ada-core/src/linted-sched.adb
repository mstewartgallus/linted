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
with Ada.Dispatching;

package body Linted.Sched is

   High_Contention_Count : constant := 16;

   procedure Pause;
   pragma Import (Intrinsic, Pause, "__builtin_ia32_pause");

   procedure Backoff (State : in out Backoff_State) is
   begin
      if State < 20 then
         Pause;
      else
         Ada.Dispatching.Yield;
      end if;

      if State /= Backoff_State'Last then
         State := State + 1;
      end if;
   end Backoff;

   procedure Success (C : in out Contention) is
   begin
      Contention_Atomics.Saturating_Decrement (Contention_Atomics.Atomic (C));
   end Success;

   procedure Backoff (C : in out Contention) is
      My_Contention : Contention_T;
   begin
      Contention_Atomics.Get (Contention_Atomics.Atomic (C), My_Contention);

      Contention_Atomics.Saturating_Decrement (Contention_Atomics.Atomic (C));

      if My_Contention < High_Contention_Count then
         Pause;
      else
         Ada.Dispatching.Yield;
      end if;
   end Backoff;

   procedure Backoff (C : in out Contention; Highly_Contended : out Boolean) is
      My_Contention : Contention_T;
   begin
      Contention_Atomics.Get (Contention_Atomics.Atomic (C), My_Contention);
      Contention_Atomics.Saturating_Decrement (Contention_Atomics.Atomic (C));

      if My_Contention < High_Contention_Count then
         Pause;
         Highly_Contended := False;
         return;
      end if;

      Highly_Contended := True;
   end Backoff;
end Linted.Sched;
