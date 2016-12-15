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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Libc.Time with SPARK_Mode => Off is
   pragma Preelaborate;

   subtype clock_t is long;

   subtype time_t is long;

   type tm is record
      tm_sec : aliased int;  -- /usr/include/time.h:135
      tm_min : aliased int;  -- /usr/include/time.h:136
      tm_hour : aliased int;  -- /usr/include/time.h:137
      tm_mday : aliased int;  -- /usr/include/time.h:138
      tm_mon : aliased int;  -- /usr/include/time.h:139
      tm_year : aliased int;  -- /usr/include/time.h:140
      tm_wday : aliased int;  -- /usr/include/time.h:141
      tm_yday : aliased int;  -- /usr/include/time.h:142
      tm_isdst : aliased int;  -- /usr/include/time.h:143
      tm_gmtoff : aliased long;  -- /usr/include/time.h:146
      tm_zone : Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:147
   end record;
   pragma Convention (C_Pass_By_Copy, tm);  -- /usr/include/time.h:133

   function clock return clock_t;  -- /usr/include/time.h:189
   pragma Import (C, clock, "clock");

   function time (timer : access time_t) return time_t;  -- /usr/include/time.h:192
   pragma Import (C, time, "time");

   function difftime (time1 : time_t; time0 : time_t) return double;  -- /usr/include/time.h:195
   pragma Import (C, difftime, "difftime");

   function mktime (tp : access tm) return time_t;  -- /usr/include/time.h:199
   pragma Import (C, mktime, "mktime");

   function strftime
     (s : Interfaces.C.Strings.chars_ptr;
      maxsize : size_t;
      format : Interfaces.C.Strings.chars_ptr;
      tp : access constant tm) return size_t;  -- /usr/include/time.h:205
   pragma Import (C, strftime, "strftime");

   function gmtime (timer : access time_t) return access tm;  -- /usr/include/time.h:239
   pragma Import (C, gmtime, "gmtime");

   function localtime (timer : access time_t) return access tm;  -- /usr/include/time.h:243
   pragma Import (C, localtime, "localtime");

   function asctime (tp : access constant tm) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:261
   pragma Import (C, asctime, "asctime");

   function ctime (timer : access time_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:264
   pragma Import (C, ctime, "ctime");
end Libc.Time;
