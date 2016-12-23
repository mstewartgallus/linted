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
with Interfaces.C.Strings;
with System;

with Libc.Locale.GNU;
with Libc.Sys.Types;

package Libc.Time.GNU with
     Spark_Mode => Off is
   pragma Preelaborate;

   TIME_UTC : constant := 1;

   subtype clockid_t is int;

   type timer_t is new System.Address;

   type timeval is record
      tv_sec : aliased time_t;
      tv_usec : aliased long;
   end record;
   pragma Convention (C_Pass_By_Copy, timeval);

   type timespec is record
      tv_sec : aliased time_t;  -- /usr/include/time.h:122
      tv_nsec : aliased long;  -- /usr/include/time.h:123
   end record;
   pragma Convention (C_Pass_By_Copy, timespec);  -- /usr/include/time.h:120

   type itimerspec is record
      it_interval : aliased timespec;  -- /usr/include/time.h:163
      it_value : aliased timespec;  -- /usr/include/time.h:164
   end record;
   pragma Convention (C_Pass_By_Copy, itimerspec);  -- /usr/include/time.h:161

   --  skipped empty struct sigevent

   function strptime
     (s : Interfaces.C.Strings.chars_ptr;
      fmt : Interfaces.C.Strings.chars_ptr;
      tp : access tm)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:213
   pragma Import (C, strptime, "strptime");

   function strftime_l
     (s : Interfaces.C.Strings.chars_ptr;
      maxsize : size_t;
      format : Interfaces.C.Strings.chars_ptr;
      tp : access constant tm;
      loc : Libc.Locale.GNU.locale_t)
     return size_t;  -- /usr/include/time.h:223
   pragma Import (C, strftime_l, "strftime_l");

   function strptime_l
     (s : Interfaces.C.Strings.chars_ptr;
      fmt : Interfaces.C.Strings.chars_ptr;
      tp : access tm;
      loc : Libc.Locale.GNU.locale_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:230
   pragma Import (C, strptime_l, "strptime_l");

   function gmtime_r
     (timer : access time_t;
      tp : access tm) return access tm;  -- /usr/include/time.h:249
   pragma Import (C, gmtime_r, "gmtime_r");

   function localtime_r
     (timer : access time_t;
      tp : access tm) return access tm;  -- /usr/include/time.h:254
   pragma Import (C, localtime_r, "localtime_r");

   function asctime_r
     (tp : access constant tm;
      buf : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:272
   pragma Import (C, asctime_r, "asctime_r");

   function ctime_r
     (timer : access time_t;
      buf : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:276
   pragma Import (C, ctime_r, "ctime_r");

   tzname : aliased array
   (0 .. 1) of Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:289
   pragma Import (C, tzname, "tzname");

   procedure tzset;  -- /usr/include/time.h:293
   pragma Import (C, tzset, "tzset");

   daylight : aliased int;  -- /usr/include/time.h:297
   pragma Import (C, daylight, "daylight");

   timezone : aliased long;  -- /usr/include/time.h:298
   pragma Import (C, timezone, "timezone");

   function stime
     (c_when : access time_t) return int;  -- /usr/include/time.h:304
   pragma Import (C, stime, "stime");

   function timegm (tp : access tm) return time_t;  -- /usr/include/time.h:319
   pragma Import (C, timegm, "timegm");

   function timelocal
     (tp : access tm) return time_t;  -- /usr/include/time.h:322
   pragma Import (C, timelocal, "timelocal");

   function dysize (year : int) return int;  -- /usr/include/time.h:325
   pragma Import (C, dysize, "dysize");

   function nanosleep
     (requested_time : access constant timespec;
      remaining : access timespec) return int;  -- /usr/include/time.h:334
   pragma Import (C, nanosleep, "nanosleep");

   function clock_getres
     (clock_id : clockid_t;
      res : access timespec) return int;  -- /usr/include/time.h:339
   pragma Import (C, clock_getres, "clock_getres");

   function clock_gettime
     (clock_id : clockid_t;
      tp : access timespec) return int;  -- /usr/include/time.h:342
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_settime
     (clock_id : clockid_t;
      tp : access constant timespec) return int;  -- /usr/include/time.h:345
   pragma Import (C, clock_settime, "clock_settime");

   function clock_nanosleep
     (clock_id : clockid_t;
      flags : int;
      req : access constant timespec;
      c_rem : access timespec) return int;  -- /usr/include/time.h:353
   pragma Import (C, clock_nanosleep, "clock_nanosleep");

   function clock_getcpuclockid
     (pid : Libc.Sys.Types.pid_t;
      clock_id : access clockid_t) return int;  -- /usr/include/time.h:358
   pragma Import (C, clock_getcpuclockid, "clock_getcpuclockid");

   function timer_create
     (clock_id : clockid_t;
      evp : System.Address;
      timerid : System.Address) return int;  -- /usr/include/time.h:363
   pragma Import (C, timer_create, "timer_create");

   function timer_delete
     (timerid : timer_t) return int;  -- /usr/include/time.h:368
   pragma Import (C, timer_delete, "timer_delete");

   function timer_settime
     (timerid : timer_t;
      flags : int;
      value : access constant itimerspec;
      ovalue : access itimerspec) return int;  -- /usr/include/time.h:371
   pragma Import (C, timer_settime, "timer_settime");

   function timer_gettime
     (timerid : timer_t;
      value : access itimerspec) return int;  -- /usr/include/time.h:376
   pragma Import (C, timer_gettime, "timer_gettime");

   function timer_getoverrun
     (timerid : timer_t) return int;  -- /usr/include/time.h:380
   pragma Import (C, timer_getoverrun, "timer_getoverrun");

   function timespec_get
     (ts : access timespec;
      base : int) return int;  -- /usr/include/time.h:386
   pragma Import (C, timespec_get, "timespec_get");

   getdate_err : aliased int;  -- /usr/include/time.h:403
   pragma Import (C, getdate_err, "getdate_err");

   function getdate
     (string : Interfaces.C.Strings.chars_ptr)
     return access tm;  -- /usr/include/time.h:412
   pragma Import (C, getdate, "getdate");

   function getdate_r
     (string : Interfaces.C.Strings.chars_ptr;
      resbufp : access tm) return int;  -- /usr/include/time.h:426
   pragma Import (C, getdate_r, "getdate_r");
end Libc.Time.GNU;
