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
with Interfaces.C.Strings;

with Libc.Stdint;
with Libc.Stddef;

with Pulse.Mainloop.Api;
with Pulse.Def;
with Pulse.Proplist;
with Pulse.Sample;

package Pulse.Context with SPARK_Mode => Off is

   type pa_context is limited private;
   type pa_context_access is access all pa_context;

   type pa_context_notify_cb_t is access procedure (c : pa_context_access; userdata : System.Address);
   pragma Convention (C, pa_context_notify_cb_t);  -- /usr/include/pulse/context.h:159

   type pa_context_success_cb_t is access procedure
        (arg1 : System.Address;
         arg2 : int;
         arg3 : System.Address);
   pragma Convention (C, pa_context_success_cb_t);  -- /usr/include/pulse/context.h:162

   type pa_context_event_cb_t is access procedure
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address;
         arg4 : System.Address);
   pragma Convention (C, pa_context_event_cb_t);  -- /usr/include/pulse/context.h:169

   function pa_context_new (mainloop : access Pulse.Mainloop.Api.pa_mainloop_api; name : Interfaces.C.Strings.chars_ptr) return pa_context_access;  -- /usr/include/pulse/context.h:174
   pragma Import (C, pa_context_new, "pa_context_new");

   function pa_context_new_with_proplist
     (mainloop : access Pulse.Mainloop.Api.pa_mainloop_api;
      name : Interfaces.C.Strings.chars_ptr;
      proplist : Pulse.Proplist.pa_proplist_access) return pa_context_access;  -- /usr/include/pulse/context.h:179
   pragma Import (C, pa_context_new_with_proplist, "pa_context_new_with_proplist");

   procedure pa_context_unref (c : pa_context_access);  -- /usr/include/pulse/context.h:182
   pragma Import (C, pa_context_unref, "pa_context_unref");

   function pa_context_ref (c : pa_context_access) return System.Address;  -- /usr/include/pulse/context.h:185
   pragma Import (C, pa_context_ref, "pa_context_ref");

   procedure pa_context_set_state_callback
     (c : pa_context_access;
      cb : pa_context_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/context.h:188
   pragma Import (C, pa_context_set_state_callback, "pa_context_set_state_callback");

   procedure pa_context_set_event_callback
     (p : System.Address;
      cb : pa_context_event_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/context.h:192
   pragma Import (C, pa_context_set_event_callback, "pa_context_set_event_callback");

   function pa_context_errno (c : pa_context_access) return int;  -- /usr/include/pulse/context.h:195
   pragma Import (C, pa_context_errno, "pa_context_errno");

   function pa_context_is_pending (c : pa_context_access) return int;  -- /usr/include/pulse/context.h:198
   pragma Import (C, pa_context_is_pending, "pa_context_is_pending");

   function pa_context_get_state (c : pa_context_access) return Pulse.Def.pa_context_state_t;  -- /usr/include/pulse/context.h:201
   pragma Import (C, pa_context_get_state, "pa_context_get_state");

   function pa_context_connect
     (c : pa_context_access;
      server : Interfaces.C.Strings.chars_ptr;
      flags : Pulse.Def.pa_context_flags_t;
      api : access constant Pulse.Def.pa_spawn_api) return int;  -- /usr/include/pulse/context.h:211
   pragma Import (C, pa_context_connect, "pa_context_connect");

   procedure pa_context_disconnect (c : pa_context_access);  -- /usr/include/pulse/context.h:214
   pragma Import (C, pa_context_disconnect, "pa_context_disconnect");

   function pa_context_drain
     (c : pa_context_access;
      cb : pa_context_notify_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:217
   pragma Import (C, pa_context_drain, "pa_context_drain");

   function pa_context_exit_daemon
     (c : pa_context_access;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:222
   pragma Import (C, pa_context_exit_daemon, "pa_context_exit_daemon");

   function pa_context_set_default_sink
     (c : pa_context_access;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:225
   pragma Import (C, pa_context_set_default_sink, "pa_context_set_default_sink");

   function pa_context_set_default_source
     (c : pa_context_access;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:228
   pragma Import (C, pa_context_set_default_source, "pa_context_set_default_source");

   function pa_context_is_local (c : pa_context_access) return int;  -- /usr/include/pulse/context.h:231
   pragma Import (C, pa_context_is_local, "pa_context_is_local");

   function pa_context_set_name
     (c : pa_context_access;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:234
   pragma Import (C, pa_context_set_name, "pa_context_set_name");

   function pa_context_get_server (c : pa_context_access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/context.h:237
   pragma Import (C, pa_context_get_server, "pa_context_get_server");

   function pa_context_get_protocol_version (c : pa_context_access) return Libc.Stdint.uint32_t;  -- /usr/include/pulse/context.h:240
   pragma Import (C, pa_context_get_protocol_version, "pa_context_get_protocol_version");

   function pa_context_get_server_protocol_version (c : pa_context_access) return Libc.Stdint.uint32_t;  -- /usr/include/pulse/context.h:243
   pragma Import (C, pa_context_get_server_protocol_version, "pa_context_get_server_protocol_version");

   function pa_context_proplist_update
     (c : pa_context_access;
      mode : Pulse.Proplist.pa_update_mode_t;
      p : System.Address;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:250
   pragma Import (C, pa_context_proplist_update, "pa_context_proplist_update");

   function pa_context_proplist_remove
     (c : pa_context_access;
      keys : System.Address;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:253
   pragma Import (C, pa_context_proplist_remove, "pa_context_proplist_remove");

   function pa_context_get_index (s : System.Address) return Libc.Stdint.uint32_t;  -- /usr/include/pulse/context.h:258
   pragma Import (C, pa_context_get_index, "pa_context_get_index");

   function pa_context_rttime_new
     (c : pa_context_access;
      usec : Pulse.Sample.pa_usec_t;
      cb : Pulse.Mainloop.Api.pa_time_event_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/context.h:262
   pragma Import (C, pa_context_rttime_new, "pa_context_rttime_new");

   procedure pa_context_rttime_restart
     (c : pa_context_access;
      e : System.Address;
      usec : Pulse.Sample.pa_usec_t);  -- /usr/include/pulse/context.h:266
   pragma Import (C, pa_context_rttime_restart, "pa_context_rttime_restart");

   function pa_context_get_tile_size (c : pa_context_access; ss : access constant Pulse.Sample.pa_sample_spec) return Libc.Stddef.size_t;  -- /usr/include/pulse/context.h:281
   pragma Import (C, pa_context_get_tile_size, "pa_context_get_tile_size");

private
   type pa_context is limited record null; end record;
end Pulse.Context;
