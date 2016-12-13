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

with Libc.Stdint;

with Pulse.Def;
with Pulse.Context;

package Pulse.Subscribe with SPARK_Mode => Off is

   type pa_context_subscribe_cb_t is access procedure
        (arg1 : System.Address;
         arg2 : Pulse.Def.pa_subscription_event_type_t;
         arg3 : Libc.Stdint.uint32_t;
         arg4 : System.Address);
   pragma Convention (C, pa_context_subscribe_cb_t);  -- /usr/include/pulse/subscribe.h:76

   function pa_context_subscribe
     (c : System.Address;
      m : Pulse.Def.pa_subscription_mask_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/subscribe.h:79
   pragma Import (C, pa_context_subscribe, "pa_context_subscribe");

   procedure pa_context_set_subscribe_callback
     (c : System.Address;
      cb : pa_context_subscribe_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/subscribe.h:82
   pragma Import (C, pa_context_set_subscribe_callback, "pa_context_set_subscribe_callback");

end Pulse.Subscribe;
