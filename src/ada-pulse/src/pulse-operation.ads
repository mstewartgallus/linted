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

with Pulse.Def;

package Pulse.Operation is

   --  skipped empty struct pa_operation

   type pa_operation_notify_cb_t is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, pa_operation_notify_cb_t);  -- /usr/include/pulse/operation.h:38

   function pa_operation_ref (o : System.Address) return System.Address;  -- /usr/include/pulse/operation.h:41
   pragma Import (C, pa_operation_ref, "pa_operation_ref");

   procedure pa_operation_unref (o : System.Address);  -- /usr/include/pulse/operation.h:44
   pragma Import (C, pa_operation_unref, "pa_operation_unref");

   procedure pa_operation_cancel (o : System.Address);  -- /usr/include/pulse/operation.h:51
   pragma Import (C, pa_operation_cancel, "pa_operation_cancel");

   function pa_operation_get_state (o : System.Address) return Pulse.Def.pa_operation_state_t;  -- /usr/include/pulse/operation.h:54
   pragma Import (C, pa_operation_get_state, "pa_operation_get_state");

   procedure pa_operation_set_state_callback
     (o : System.Address;
      cb : pa_operation_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/operation.h:62
   pragma Import (C, pa_operation_set_state_callback, "pa_operation_set_state_callback");

end Pulse.Operation;
