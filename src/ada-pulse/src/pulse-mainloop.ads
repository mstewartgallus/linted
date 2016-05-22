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

limited with Pulse.Mainloop.Api;

package Pulse.Mainloop is

   --  skipped empty struct pollfd

   type pa_mainloop is limited private;
   type pa_mainloop_access is access all pa_mainloop;

   function pa_mainloop_new return pa_mainloop_access;  -- /usr/include/pulse/mainloop.h:83
   pragma Import (C, pa_mainloop_new, "pa_mainloop_new");

   procedure pa_mainloop_free (m : pa_mainloop_access);  -- /usr/include/pulse/mainloop.h:86
   pragma Import (C, pa_mainloop_free, "pa_mainloop_free");

   function pa_mainloop_prepare (m : pa_mainloop_access; timeout : int) return int;  -- /usr/include/pulse/mainloop.h:91
   pragma Import (C, pa_mainloop_prepare, "pa_mainloop_prepare");

   function pa_mainloop_poll (m : pa_mainloop_access) return int;  -- /usr/include/pulse/mainloop.h:94
   pragma Import (C, pa_mainloop_poll, "pa_mainloop_poll");

   function pa_mainloop_dispatch (m : pa_mainloop_access) return int;  -- /usr/include/pulse/mainloop.h:98
   pragma Import (C, pa_mainloop_dispatch, "pa_mainloop_dispatch");

   function pa_mainloop_get_retval (m : pa_mainloop_access) return int;  -- /usr/include/pulse/mainloop.h:101
   pragma Import (C, pa_mainloop_get_retval, "pa_mainloop_get_retval");

   function pa_mainloop_iterate
     (m : pa_mainloop_access;
      block : int;
      retval : access int) return int;  -- /usr/include/pulse/mainloop.h:109
   pragma Import (C, pa_mainloop_iterate, "pa_mainloop_iterate");

   function pa_mainloop_run (m : pa_mainloop_access; retval : out int) return int;  -- /usr/include/pulse/mainloop.h:112
   pragma Import (C, pa_mainloop_run, "pa_mainloop_run");

   function pa_mainloop_get_api (m : pa_mainloop_access) return access Pulse.Mainloop.Api.pa_mainloop_api;  -- /usr/include/pulse/mainloop.h:117
   pragma Import (C, pa_mainloop_get_api, "pa_mainloop_get_api");

   procedure pa_mainloop_quit (m : pa_mainloop_access; r : int);  -- /usr/include/pulse/mainloop.h:120
   pragma Import (C, pa_mainloop_quit, "pa_mainloop_quit");

   procedure pa_mainloop_wakeup (m : pa_mainloop_access);  -- /usr/include/pulse/mainloop.h:123
   pragma Import (C, pa_mainloop_wakeup, "pa_mainloop_wakeup");

   type pa_poll_func is access function
        (arg1 : System.Address;
         arg2 : unsigned_long;
         arg3 : int;
         arg4 : System.Address) return int;
   pragma Convention (C, pa_poll_func);  -- /usr/include/pulse/mainloop.h:126

   procedure pa_mainloop_set_poll_func
     (m : pa_mainloop_access;
      poll_func : pa_poll_func;
      userdata : System.Address);  -- /usr/include/pulse/mainloop.h:129
   pragma Import (C, pa_mainloop_set_poll_func, "pa_mainloop_set_poll_func");

private
   type pa_mainloop is limited record null; end record;
end Pulse.Mainloop;
