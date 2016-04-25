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

with Pulse.Context;
with Pulse.Volume;

package Pulse.Scache is

   type pa_context_play_sample_cb_t is access procedure
        (arg1 : System.Address;
         arg2 : Libc.Stdint.uint32_t;
         arg3 : System.Address);
   pragma Convention (C, pa_context_play_sample_cb_t);  -- /usr/include/pulse/scache.h:87

   function pa_stream_connect_upload (s : System.Address; length : Libc.Stddef.size_t) return int;  -- /usr/include/pulse/scache.h:90
   pragma Import (C, pa_stream_connect_upload, "pa_stream_connect_upload");

   function pa_stream_finish_upload (s : System.Address) return int;  -- /usr/include/pulse/scache.h:95
   pragma Import (C, pa_stream_finish_upload, "pa_stream_finish_upload");

   function pa_context_remove_sample
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/scache.h:98
   pragma Import (C, pa_context_remove_sample, "pa_context_remove_sample");

   function pa_context_play_sample
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      dev : Interfaces.C.Strings.chars_ptr;
      volume : Pulse.Volume.pa_volume_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/scache.h:103
   pragma Import (C, pa_context_play_sample, "pa_context_play_sample");

   function pa_context_play_sample_with_proplist
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      dev : Interfaces.C.Strings.chars_ptr;
      volume : Pulse.Volume.pa_volume_t;
      proplist : System.Address;
      cb : pa_context_play_sample_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/scache.h:115
   pragma Import (C, pa_context_play_sample_with_proplist, "pa_context_play_sample_with_proplist");

end Pulse.Scache;
