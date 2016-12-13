-- Copyright 2016 Steven Stewart-Gallus
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
with Interfaces.C; use Interfaces.C;

limited with Libc.Time;
limited with Libc.Time.GNU;

with Pulse.Sample;

package Pulse.Timeval with SPARK_Mode => Off is

   --  unsupported macro: PA_MSEC_PER_SEC ((pa_usec_t) 1000ULL)
   --  unsupported macro: PA_USEC_PER_SEC ((pa_usec_t) 1000000ULL)
   --  unsupported macro: PA_NSEC_PER_SEC ((unsigned long long) 1000000000ULL)
   --  unsupported macro: PA_USEC_PER_MSEC ((pa_usec_t) 1000ULL)
   --  unsupported macro: PA_NSEC_PER_MSEC ((unsigned long long) 1000000ULL)
   --  unsupported macro: PA_NSEC_PER_USEC ((unsigned long long) 1000ULL)
   --  unsupported macro: PA_USEC_INVALID ((pa_usec_t) -1)
   --  unsupported macro: PA_USEC_MAX ((pa_usec_t) -2)
   function pa_gettimeofday (tv : access Libc.Time.GNU.timeval) return access Libc.Time.GNU.timeval;  -- /usr/include/pulse/timeval.h:63
   pragma Import (C, pa_gettimeofday, "pa_gettimeofday");

   function pa_timeval_diff (a : access constant Libc.Time.GNU.timeval; b : access constant Libc.Time.GNU.timeval) return Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/timeval.h:67
   pragma Import (C, pa_timeval_diff, "pa_timeval_diff");

   function pa_timeval_cmp (a : access constant Libc.Time.GNU.timeval; b : access constant Libc.Time.GNU.timeval) return int;  -- /usr/include/pulse/timeval.h:70
   pragma Import (C, pa_timeval_cmp, "pa_timeval_cmp");

   function pa_timeval_age (tv : access constant Libc.Time.GNU.timeval) return Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/timeval.h:73
   pragma Import (C, pa_timeval_age, "pa_timeval_age");

   function pa_timeval_add (tv : access Libc.Time.GNU.timeval; v : Pulse.Sample.pa_usec_t) return access Libc.Time.GNU.timeval;  -- /usr/include/pulse/timeval.h:76
   pragma Import (C, pa_timeval_add, "pa_timeval_add");

   function pa_timeval_sub (tv : access Libc.Time.GNU.timeval; v : Pulse.Sample.pa_usec_t) return access Libc.Time.GNU.timeval;  -- /usr/include/pulse/timeval.h:79
   pragma Import (C, pa_timeval_sub, "pa_timeval_sub");

   function pa_timeval_store (tv : access Libc.Time.GNU.timeval; v : Pulse.Sample.pa_usec_t) return access Libc.Time.GNU.timeval;  -- /usr/include/pulse/timeval.h:82
   pragma Import (C, pa_timeval_store, "pa_timeval_store");

   function pa_timeval_load (tv : access constant Libc.Time.GNU.timeval) return Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/timeval.h:85
   pragma Import (C, pa_timeval_load, "pa_timeval_load");

end Pulse.Timeval;
