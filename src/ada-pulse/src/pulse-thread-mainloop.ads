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
with System;
limited with Pulse.Mainloop.Api;

package Pulse.Thread.Mainloop with SPARK_Mode => Off is

   --  skipped empty struct pa_threaded_mainloop

   function pa_threaded_mainloop_new return System.Address;  -- /usr/include/pulse/thread-mainloop.h:253
   pragma Import (C, pa_threaded_mainloop_new, "pa_threaded_mainloop_new");

   procedure pa_threaded_mainloop_free (m : System.Address);  -- /usr/include/pulse/thread-mainloop.h:258
   pragma Import (C, pa_threaded_mainloop_free, "pa_threaded_mainloop_free");

   function pa_threaded_mainloop_start (m : System.Address) return int;  -- /usr/include/pulse/thread-mainloop.h:261
   pragma Import (C, pa_threaded_mainloop_start, "pa_threaded_mainloop_start");

   procedure pa_threaded_mainloop_stop (m : System.Address);  -- /usr/include/pulse/thread-mainloop.h:265
   pragma Import (C, pa_threaded_mainloop_stop, "pa_threaded_mainloop_stop");

   procedure pa_threaded_mainloop_lock (m : System.Address);  -- /usr/include/pulse/thread-mainloop.h:273
   pragma Import (C, pa_threaded_mainloop_lock, "pa_threaded_mainloop_lock");

   procedure pa_threaded_mainloop_unlock (m : System.Address);  -- /usr/include/pulse/thread-mainloop.h:276
   pragma Import (C, pa_threaded_mainloop_unlock, "pa_threaded_mainloop_unlock");

   procedure pa_threaded_mainloop_wait (m : System.Address);  -- /usr/include/pulse/thread-mainloop.h:287
   pragma Import (C, pa_threaded_mainloop_wait, "pa_threaded_mainloop_wait");

   procedure pa_threaded_mainloop_signal (m : System.Address; wait_for_accept : int);  -- /usr/include/pulse/thread-mainloop.h:294
   pragma Import (C, pa_threaded_mainloop_signal, "pa_threaded_mainloop_signal");

   procedure pa_threaded_mainloop_accept (m : System.Address);  -- /usr/include/pulse/thread-mainloop.h:300
   pragma Import (C, pa_threaded_mainloop_accept, "pa_threaded_mainloop_accept");

   function pa_threaded_mainloop_get_retval (m : System.Address) return int;  -- /usr/include/pulse/thread-mainloop.h:304
   pragma Import (C, pa_threaded_mainloop_get_retval, "pa_threaded_mainloop_get_retval");

   function pa_threaded_mainloop_get_api (m : System.Address) return access Pulse.Mainloop.Api.pa_mainloop_api;  -- /usr/include/pulse/thread-mainloop.h:309
   pragma Import (C, pa_threaded_mainloop_get_api, "pa_threaded_mainloop_get_api");

   function pa_threaded_mainloop_in_thread (m : System.Address) return int;  -- /usr/include/pulse/thread-mainloop.h:312
   pragma Import (C, pa_threaded_mainloop_in_thread, "pa_threaded_mainloop_in_thread");

end Pulse.Thread.Mainloop;
