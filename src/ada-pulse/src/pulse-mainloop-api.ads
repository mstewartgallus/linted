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
with System;

with Interfaces.C; use Interfaces.C;

limited with Libc.Time.GNU;

package Pulse.Mainloop.API with
     Spark_Mode => Off is

   subtype pa_io_event_flags is unsigned;
   PA_IO_EVENT_NULL : constant pa_io_event_flags := 0;
   PA_IO_EVENT_INPUT : constant pa_io_event_flags := 1;
   PA_IO_EVENT_OUTPUT : constant pa_io_event_flags := 2;
   PA_IO_EVENT_HANGUP : constant pa_io_event_flags := 4;
   PA_IO_EVENT_ERROR : constant pa_io_event_flags :=
     8;  -- /usr/include/pulse/mainloop-api.h:52

   subtype pa_io_event_flags_t is pa_io_event_flags;

   --  skipped empty struct pa_io_event

   type pa_io_event_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : pa_io_event_flags_t;
      arg5 : System.Address);
   pragma Convention
     (C,
      pa_io_event_cb_t);  -- /usr/include/pulse/mainloop-api.h:63

   type pa_io_event_destroy_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_io_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:65

   --  skipped empty struct pa_time_event

   type pa_time_event_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : access constant Libc.Time.GNU.timeval;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_time_event_cb_t);  -- /usr/include/pulse/mainloop-api.h:70

   type pa_time_event_destroy_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_time_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:72

   --  skipped empty struct pa_defer_event

   type pa_defer_event_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_defer_event_cb_t);  -- /usr/include/pulse/mainloop-api.h:77

   type pa_defer_event_destroy_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_defer_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:79

   type pa_mainloop_api is record
      userdata : System.Address;  -- /usr/include/pulse/mainloop-api.h:84
      io_new : access function
        (arg1 : access pa_mainloop_api;
         arg2 : int;
         arg3 : pa_io_event_flags_t;
         arg4 : pa_io_event_cb_t;
         arg5 : System.Address)
        return System.Address;  -- /usr/include/pulse/mainloop-api.h:87
      io_enable : access procedure
        (arg1 : System.Address;
         arg2 : pa_io_event_flags_t);  -- /usr/include/pulse/mainloop-api.h:89
      io_free : access procedure
        (arg1 : System.Address);  -- /usr/include/pulse/mainloop-api.h:91
      io_set_destroy : access procedure
        (arg1 : System.Address;
         arg2 : pa_io_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:93
      time_new : access function
        (arg1 : access pa_mainloop_api;
         arg2 : access constant Libc.Time.GNU.timeval;
         arg3 : pa_time_event_cb_t;
         arg4 : System.Address)
        return System.Address;  -- /usr/include/pulse/mainloop-api.h:96
      time_restart : access procedure
        (arg1 : System.Address;
         arg2 : access constant Libc.Time.GNU
           .timeval);  -- /usr/include/pulse/mainloop-api.h:98
      time_free : access procedure
        (arg1 : System.Address);  -- /usr/include/pulse/mainloop-api.h:100
      time_set_destroy : access procedure
        (arg1 : System.Address;
         arg2 : pa_time_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:102
      defer_new : access function
        (arg1 : access pa_mainloop_api;
         arg2 : pa_defer_event_cb_t;
         arg3 : System.Address)
        return System.Address;  -- /usr/include/pulse/mainloop-api.h:105
      defer_enable : access procedure
        (arg1 : System.Address;
         arg2 : int);  -- /usr/include/pulse/mainloop-api.h:107
      defer_free : access procedure
        (arg1 : System.Address);  -- /usr/include/pulse/mainloop-api.h:109
      defer_set_destroy : access procedure
        (arg1 : System.Address;
         arg2 : pa_defer_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:111
      quit : access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : int);  -- /usr/include/pulse/mainloop-api.h:114
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_mainloop_api);  -- /usr/include/pulse/mainloop-api.h:82

   procedure pa_mainloop_api_once
     (m : access pa_mainloop_api;
      callback : access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : System.Address);
      userdata : System.Address);  -- /usr/include/pulse/mainloop-api.h:118
   pragma Import (C, pa_mainloop_api_once, "pa_mainloop_api_once");

end Pulse.Mainloop.API;
