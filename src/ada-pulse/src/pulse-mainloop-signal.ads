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

package Pulse.Mainloop.Signal with SPARK_Mode => Off is

   --  skipped empty struct pa_signal_event

   type pa_signal_cb_t is access procedure
        (arg1 : access Pulse.Mainloop.Api.pa_mainloop_api;
         arg2 : System.Address;
         arg3 : int;
         arg4 : System.Address);
   pragma Convention (C, pa_signal_cb_t);  -- /usr/include/pulse/mainloop-signal.h:44

   type pa_signal_destroy_cb_t is access procedure
        (arg1 : access Pulse.Mainloop.Api.pa_mainloop_api;
         arg2 : System.Address;
         arg3 : System.Address);
   pragma Convention (C, pa_signal_destroy_cb_t);  -- /usr/include/pulse/mainloop-signal.h:47

   function pa_signal_init (api : access Pulse.Mainloop.Api.pa_mainloop_api) return int;  -- /usr/include/pulse/mainloop-signal.h:50
   pragma Import (C, pa_signal_init, "pa_signal_init");

   procedure pa_signal_done;  -- /usr/include/pulse/mainloop-signal.h:53
   pragma Import (C, pa_signal_done, "pa_signal_done");

   function pa_signal_new
     (sig : int;
      callback : pa_signal_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/mainloop-signal.h:56
   pragma Import (C, pa_signal_new, "pa_signal_new");

   procedure pa_signal_free (e : System.Address);  -- /usr/include/pulse/mainloop-signal.h:59
   pragma Import (C, pa_signal_free, "pa_signal_free");

   procedure pa_signal_set_destroy (e : System.Address; callback : pa_signal_destroy_cb_t);  -- /usr/include/pulse/mainloop-signal.h:62
   pragma Import (C, pa_signal_set_destroy, "pa_signal_set_destroy");

end Pulse.Mainloop.Signal;
