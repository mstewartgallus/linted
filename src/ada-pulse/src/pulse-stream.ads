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

with Libc.Stddef;
with Libc.Stdint;

with Pulse.Context;
with Pulse.Sample;
limited with Pulse.Channelmap;
with Pulse.Def;
limited with Pulse.Volume;
limited with Pulse.Format;
with Pulse.Proplist;

package Pulse.Stream with SPARK_Mode => Off is

   type pa_stream is limited private;
   type pa_stream_access is access all pa_stream;

   type pa_stream_success_cb_t is access procedure
        (arg1 : System.Address;
         arg2 : int;
         arg3 : System.Address);
   pragma Convention (C, pa_stream_success_cb_t);  -- /usr/include/pulse/stream.h:325

   type pa_stream_request_cb_t is access procedure
        (s : pa_stream_access;
         nbytes : Libc.Stddef.size_t;
         userdata : System.Address);
   pragma Convention (C, pa_stream_request_cb_t);  -- /usr/include/pulse/stream.h:328

   type pa_stream_notify_cb_t is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, pa_stream_notify_cb_t);  -- /usr/include/pulse/stream.h:331

   type pa_stream_event_cb_t is access procedure
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address;
         arg4 : System.Address);
   pragma Convention (C, pa_stream_event_cb_t);  -- /usr/include/pulse/stream.h:339

   function pa_stream_new
     (c : Pulse.Context.pa_context_access;
      name : Interfaces.C.Strings.chars_ptr;
      ss : access constant Pulse.Sample.pa_sample_spec;
      map : access constant Pulse.Channelmap.pa_channel_map) return pa_stream_access;  -- /usr/include/pulse/stream.h:344
   pragma Import (C, pa_stream_new, "pa_stream_new");

   function pa_stream_new_with_proplist
     (c : Pulse.Context.pa_context_access;
      name : Interfaces.C.Strings.chars_ptr;
      ss : access constant Pulse.Sample.pa_sample_spec;
      map : access constant Pulse.Channelmap.pa_channel_map;
      p : pa_stream_access) return System.Address;  -- /usr/include/pulse/stream.h:353
   pragma Import (C, pa_stream_new_with_proplist, "pa_stream_new_with_proplist");

   function pa_stream_new_extended
     (c : Pulse.Context.pa_context_access;
      name : Interfaces.C.Strings.chars_ptr;
      formats : pa_stream_access;
      n_formats : unsigned;
      p : pa_stream_access) return System.Address;  -- /usr/include/pulse/stream.h:364
   pragma Import (C, pa_stream_new_extended, "pa_stream_new_extended");

   procedure pa_stream_unref (s : pa_stream_access);  -- /usr/include/pulse/stream.h:372
   pragma Import (C, pa_stream_unref, "pa_stream_unref");

   function pa_stream_ref (s : pa_stream_access) return System.Address;  -- /usr/include/pulse/stream.h:375
   pragma Import (C, pa_stream_ref, "pa_stream_ref");

   function pa_stream_get_state (p : pa_stream_access) return Pulse.Def.pa_stream_state_t;  -- /usr/include/pulse/stream.h:378
   pragma Import (C, pa_stream_get_state, "pa_stream_get_state");

   function pa_stream_get_context (p : pa_stream_access) return System.Address;  -- /usr/include/pulse/stream.h:381
   pragma Import (C, pa_stream_get_context, "pa_stream_get_context");

   function pa_stream_get_index (s : pa_stream_access) return Libc.Stdint.uint32_t;  -- /usr/include/pulse/stream.h:387
   pragma Import (C, pa_stream_get_index, "pa_stream_get_index");

   function pa_stream_get_device_index (s : pa_stream_access) return Libc.Stdint.uint32_t;  -- /usr/include/pulse/stream.h:398
   pragma Import (C, pa_stream_get_device_index, "pa_stream_get_device_index");

   function pa_stream_get_device_name (s : pa_stream_access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/stream.h:409
   pragma Import (C, pa_stream_get_device_name, "pa_stream_get_device_name");

   function pa_stream_is_suspended (s : pa_stream_access) return int;  -- /usr/include/pulse/stream.h:415
   pragma Import (C, pa_stream_is_suspended, "pa_stream_is_suspended");

   function pa_stream_is_corked (s : pa_stream_access) return int;  -- /usr/include/pulse/stream.h:419
   pragma Import (C, pa_stream_is_corked, "pa_stream_is_corked");

   function pa_stream_connect_playback
     (s : pa_stream_access;
      dev : Interfaces.C.Strings.chars_ptr;
      attr : access constant Pulse.Def.pa_buffer_attr;
      flags : Pulse.Def.pa_stream_flags_t;
      volume : access constant Pulse.Volume.pa_cvolume;
      sync_stream : System.Address) return int;  -- /usr/include/pulse/stream.h:439
   pragma Import (C, pa_stream_connect_playback, "pa_stream_connect_playback");

   function pa_stream_connect_record
     (s : pa_stream_access;
      dev : Interfaces.C.Strings.chars_ptr;
      attr : access constant Pulse.Def.pa_buffer_attr;
      flags : Pulse.Def.pa_stream_flags_t) return int;  -- /usr/include/pulse/stream.h:448
   pragma Import (C, pa_stream_connect_record, "pa_stream_connect_record");

   function pa_stream_disconnect (s : pa_stream_access) return int;  -- /usr/include/pulse/stream.h:455
   pragma Import (C, pa_stream_disconnect, "pa_stream_disconnect");

   function pa_stream_begin_write
     (p : pa_stream_access;
      data : System.Address;
      nbytes : access Libc.Stddef.size_t) return int;  -- /usr/include/pulse/stream.h:489
   pragma Import (C, pa_stream_begin_write, "pa_stream_begin_write");

   function pa_stream_cancel_write (p : pa_stream_access) return int;  -- /usr/include/pulse/stream.h:503
   pragma Import (C, pa_stream_cancel_write, "pa_stream_cancel_write");

   function pa_stream_write
     (p : pa_stream_access;
      data : System.Address;
      nbytes : Libc.Stddef.size_t;
      free_cb : Pulse.Def.pa_free_cb_t;
      offset : Libc.Stdint.int64_t;
      seek : Pulse.Def.pa_seek_mode_t) return int;  -- /usr/include/pulse/stream.h:528
   pragma Import (C, pa_stream_write, "pa_stream_write");

   function pa_stream_peek
     (p : pa_stream_access;
      data : System.Address;
      nbytes : access Libc.Stddef.size_t) return int;  -- /usr/include/pulse/stream.h:552
   pragma Import (C, pa_stream_peek, "pa_stream_peek");

   function pa_stream_drop (p : pa_stream_access) return int;  -- /usr/include/pulse/stream.h:559
   pragma Import (C, pa_stream_drop, "pa_stream_drop");

   function pa_stream_writable_size (p : pa_stream_access) return Libc.Stddef.size_t;  -- /usr/include/pulse/stream.h:562
   pragma Import (C, pa_stream_writable_size, "pa_stream_writable_size");

   function pa_stream_readable_size (p : pa_stream_access) return Libc.Stddef.size_t;  -- /usr/include/pulse/stream.h:565
   pragma Import (C, pa_stream_readable_size, "pa_stream_readable_size");

   function pa_stream_drain
     (s : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:571
   pragma Import (C, pa_stream_drain, "pa_stream_drain");

   function pa_stream_update_timing_info
     (p : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:577
   pragma Import (C, pa_stream_update_timing_info, "pa_stream_update_timing_info");

   procedure pa_stream_set_state_callback
     (s : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:580
   pragma Import (C, pa_stream_set_state_callback, "pa_stream_set_state_callback");

   procedure pa_stream_set_write_callback
     (p : pa_stream_access;
      cb : pa_stream_request_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:584
   pragma Import (C, pa_stream_set_write_callback, "pa_stream_set_write_callback");

   procedure pa_stream_set_read_callback
     (p : pa_stream_access;
      cb : pa_stream_request_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:587
   pragma Import (C, pa_stream_set_read_callback, "pa_stream_set_read_callback");

   procedure pa_stream_set_overflow_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:590
   pragma Import (C, pa_stream_set_overflow_callback, "pa_stream_set_overflow_callback");

   function pa_stream_get_underflow_index (p : pa_stream_access) return Libc.Stdint.int64_t;  -- /usr/include/pulse/stream.h:596
   pragma Import (C, pa_stream_get_underflow_index, "pa_stream_get_underflow_index");

   procedure pa_stream_set_underflow_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:599
   pragma Import (C, pa_stream_set_underflow_callback, "pa_stream_set_underflow_callback");

   procedure pa_stream_set_started_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:606
   pragma Import (C, pa_stream_set_started_callback, "pa_stream_set_started_callback");

   procedure pa_stream_set_latency_update_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:611
   pragma Import (C, pa_stream_set_latency_update_callback, "pa_stream_set_latency_update_callback");

   procedure pa_stream_set_moved_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:618
   pragma Import (C, pa_stream_set_moved_callback, "pa_stream_set_moved_callback");

   procedure pa_stream_set_suspended_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:628
   pragma Import (C, pa_stream_set_suspended_callback, "pa_stream_set_suspended_callback");

   procedure pa_stream_set_event_callback
     (p : pa_stream_access;
      cb : pa_stream_event_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:632
   pragma Import (C, pa_stream_set_event_callback, "pa_stream_set_event_callback");

   procedure pa_stream_set_buffer_attr_callback
     (p : pa_stream_access;
      cb : pa_stream_notify_cb_t;
      userdata : System.Address);  -- /usr/include/pulse/stream.h:639
   pragma Import (C, pa_stream_set_buffer_attr_callback, "pa_stream_set_buffer_attr_callback");

   function pa_stream_cork
     (s : pa_stream_access;
      b : int;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:651
   pragma Import (C, pa_stream_cork, "pa_stream_cork");

   function pa_stream_flush
     (s : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:657
   pragma Import (C, pa_stream_flush, "pa_stream_flush");

   function pa_stream_prebuf
     (s : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:661
   pragma Import (C, pa_stream_prebuf, "pa_stream_prebuf");

   function pa_stream_trigger
     (s : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:666
   pragma Import (C, pa_stream_trigger, "pa_stream_trigger");

   function pa_stream_set_name
     (s : pa_stream_access;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:669
   pragma Import (C, pa_stream_set_name, "pa_stream_set_name");

   function pa_stream_get_time (s : pa_stream_access; r_usec : access Pulse.Sample.pa_usec_t) return int;  -- /usr/include/pulse/stream.h:702
   pragma Import (C, pa_stream_get_time, "pa_stream_get_time");

   function pa_stream_get_latency
     (s : pa_stream_access;
      r_usec : access Pulse.Sample.pa_usec_t;
      negative : access int) return int;  -- /usr/include/pulse/stream.h:716
   pragma Import (C, pa_stream_get_latency, "pa_stream_get_latency");

   function pa_stream_get_timing_info (s : pa_stream_access) return access constant Pulse.Def.pa_timing_info;  -- /usr/include/pulse/stream.h:732
   pragma Import (C, pa_stream_get_timing_info, "pa_stream_get_timing_info");

   function pa_stream_get_sample_spec (s : pa_stream_access) return access constant Pulse.Sample.pa_sample_spec;  -- /usr/include/pulse/stream.h:735
   pragma Import (C, pa_stream_get_sample_spec, "pa_stream_get_sample_spec");

   function pa_stream_get_channel_map (s : pa_stream_access) return access constant Pulse.Channelmap.pa_channel_map;  -- /usr/include/pulse/stream.h:738
   pragma Import (C, pa_stream_get_channel_map, "pa_stream_get_channel_map");

   function pa_stream_get_format_info (s : pa_stream_access) return access constant Pulse.Format.pa_format_info;  -- /usr/include/pulse/stream.h:741
   pragma Import (C, pa_stream_get_format_info, "pa_stream_get_format_info");

   function pa_stream_get_buffer_attr (s : pa_stream_access) return access constant Pulse.Def.pa_buffer_attr;  -- /usr/include/pulse/stream.h:751
   pragma Import (C, pa_stream_get_buffer_attr, "pa_stream_get_buffer_attr");

   function pa_stream_set_buffer_attr
     (s : pa_stream_access;
      attr : access constant Pulse.Def.pa_buffer_attr;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:761
   pragma Import (C, pa_stream_set_buffer_attr, "pa_stream_set_buffer_attr");

   function pa_stream_update_sample_rate
     (s : pa_stream_access;
      rate : Libc.Stdint.uint32_t;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:768
   pragma Import (C, pa_stream_update_sample_rate, "pa_stream_update_sample_rate");

   function pa_stream_proplist_update
     (s : pa_stream_access;
      mode : Pulse.Proplist.pa_update_mode_t;
      p : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:776
   pragma Import (C, pa_stream_proplist_update, "pa_stream_proplist_update");

   function pa_stream_proplist_remove
     (s : pa_stream_access;
      keys : pa_stream_access;
      cb : pa_stream_success_cb_t;
      userdata : System.Address) return System.Address;  -- /usr/include/pulse/stream.h:780
   pragma Import (C, pa_stream_proplist_remove, "pa_stream_proplist_remove");

   function pa_stream_set_monitor_stream (s : pa_stream_access; sink_input_idx : Libc.Stdint.uint32_t) return int;  -- /usr/include/pulse/stream.h:786
   pragma Import (C, pa_stream_set_monitor_stream, "pa_stream_set_monitor_stream");

   function pa_stream_get_monitor_stream (s : pa_stream_access) return Libc.Stdint.uint32_t;  -- /usr/include/pulse/stream.h:791
   pragma Import (C, pa_stream_get_monitor_stream, "pa_stream_get_monitor_stream");

private
   type pa_stream is limited record null; end record;
end Pulse.Stream;
