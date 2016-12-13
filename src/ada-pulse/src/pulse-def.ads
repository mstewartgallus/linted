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
with Libc.Time;
with Libc.Time.GNU;

with Pulse.Sample;

package Pulse.Def with SPARK_Mode => Off is

   --  unsupported macro: PA_CONTEXT_UNCONNECTED PA_CONTEXT_UNCONNECTED
   --  unsupported macro: PA_CONTEXT_CONNECTING PA_CONTEXT_CONNECTING
   --  unsupported macro: PA_CONTEXT_AUTHORIZING PA_CONTEXT_AUTHORIZING
   --  unsupported macro: PA_CONTEXT_SETTING_NAME PA_CONTEXT_SETTING_NAME
   --  unsupported macro: PA_CONTEXT_READY PA_CONTEXT_READY
   --  unsupported macro: PA_CONTEXT_FAILED PA_CONTEXT_FAILED
   --  unsupported macro: PA_CONTEXT_TERMINATED PA_CONTEXT_TERMINATED
   --  unsupported macro: PA_CONTEXT_IS_GOOD PA_CONTEXT_IS_GOOD
   --  unsupported macro: PA_STREAM_UNCONNECTED PA_STREAM_UNCONNECTED
   --  unsupported macro: PA_STREAM_CREATING PA_STREAM_CREATING
   --  unsupported macro: PA_STREAM_READY PA_STREAM_READY
   --  unsupported macro: PA_STREAM_FAILED PA_STREAM_FAILED
   --  unsupported macro: PA_STREAM_TERMINATED PA_STREAM_TERMINATED
   --  unsupported macro: PA_STREAM_IS_GOOD PA_STREAM_IS_GOOD
   --  unsupported macro: PA_OPERATION_RUNNING PA_OPERATION_RUNNING
   --  unsupported macro: PA_OPERATION_DONE PA_OPERATION_DONE
   --  unsupported macro: PA_OPERATION_CANCELED PA_OPERATION_CANCELLED
   --  unsupported macro: PA_OPERATION_CANCELLED PA_OPERATION_CANCELLED
   --  unsupported macro: PA_INVALID_INDEX ((uint32_t) -1)
   --  unsupported macro: PA_CONTEXT_NOAUTOSPAWN PA_CONTEXT_NOAUTOSPAWN
   --  unsupported macro: PA_CONTEXT_NOFAIL PA_CONTEXT_NOFAIL
   --  unsupported macro: PA_DIRECTION_OUTPUT PA_DIRECTION_OUTPUT
   --  unsupported macro: PA_DIRECTION_INPUT PA_DIRECTION_INPUT
   --  unsupported macro: PA_DEVICE_TYPE_SINK PA_DEVICE_TYPE_SINK
   --  unsupported macro: PA_DEVICE_TYPE_SOURCE PA_DEVICE_TYPE_SOURCE
   --  unsupported macro: PA_STREAM_NODIRECTION PA_STREAM_NODIRECTION
   --  unsupported macro: PA_STREAM_PLAYBACK PA_STREAM_PLAYBACK
   --  unsupported macro: PA_STREAM_RECORD PA_STREAM_RECORD
   --  unsupported macro: PA_STREAM_UPLOAD PA_STREAM_UPLOAD
   --  unsupported macro: PA_STREAM_NOT_MONOTONOUS PA_STREAM_NOT_MONOTONIC
   --  unsupported macro: PA_STREAM_START_CORKED PA_STREAM_START_CORKED
   --  unsupported macro: PA_STREAM_INTERPOLATE_TIMING PA_STREAM_INTERPOLATE_TIMING
   --  unsupported macro: PA_STREAM_NOT_MONOTONIC PA_STREAM_NOT_MONOTONIC
   --  unsupported macro: PA_STREAM_AUTO_TIMING_UPDATE PA_STREAM_AUTO_TIMING_UPDATE
   --  unsupported macro: PA_STREAM_NO_REMAP_CHANNELS PA_STREAM_NO_REMAP_CHANNELS
   --  unsupported macro: PA_STREAM_NO_REMIX_CHANNELS PA_STREAM_NO_REMIX_CHANNELS
   --  unsupported macro: PA_STREAM_FIX_FORMAT PA_STREAM_FIX_FORMAT
   --  unsupported macro: PA_STREAM_FIX_RATE PA_STREAM_FIX_RATE
   --  unsupported macro: PA_STREAM_FIX_CHANNELS PA_STREAM_FIX_CHANNELS
   --  unsupported macro: PA_STREAM_DONT_MOVE PA_STREAM_DONT_MOVE
   --  unsupported macro: PA_STREAM_VARIABLE_RATE PA_STREAM_VARIABLE_RATE
   --  unsupported macro: PA_STREAM_PEAK_DETECT PA_STREAM_PEAK_DETECT
   --  unsupported macro: PA_STREAM_START_MUTED PA_STREAM_START_MUTED
   --  unsupported macro: PA_STREAM_ADJUST_LATENCY PA_STREAM_ADJUST_LATENCY
   --  unsupported macro: PA_STREAM_EARLY_REQUESTS PA_STREAM_EARLY_REQUESTS
   --  unsupported macro: PA_STREAM_DONT_INHIBIT_AUTO_SUSPEND PA_STREAM_DONT_INHIBIT_AUTO_SUSPEND
   --  unsupported macro: PA_STREAM_START_UNMUTED PA_STREAM_START_UNMUTED
   --  unsupported macro: PA_STREAM_FAIL_ON_SUSPEND PA_STREAM_FAIL_ON_SUSPEND
   --  unsupported macro: PA_STREAM_RELATIVE_VOLUME PA_STREAM_RELATIVE_VOLUME
   --  unsupported macro: PA_STREAM_PASSTHROUGH PA_STREAM_PASSTHROUGH
   --  unsupported macro: PA_OK PA_OK
   --  unsupported macro: PA_ERR_ACCESS PA_ERR_ACCESS
   --  unsupported macro: PA_ERR_COMMAND PA_ERR_COMMAND
   --  unsupported macro: PA_ERR_INVALID PA_ERR_INVALID
   --  unsupported macro: PA_ERR_EXIST PA_ERR_EXIST
   --  unsupported macro: PA_ERR_NOENTITY PA_ERR_NOENTITY
   --  unsupported macro: PA_ERR_CONNECTIONREFUSED PA_ERR_CONNECTIONREFUSED
   --  unsupported macro: PA_ERR_PROTOCOL PA_ERR_PROTOCOL
   --  unsupported macro: PA_ERR_TIMEOUT PA_ERR_TIMEOUT
   --  unsupported macro: PA_ERR_AUTHKEY PA_ERR_AUTHKEY
   --  unsupported macro: PA_ERR_INTERNAL PA_ERR_INTERNAL
   --  unsupported macro: PA_ERR_CONNECTIONTERMINATED PA_ERR_CONNECTIONTERMINATED
   --  unsupported macro: PA_ERR_KILLED PA_ERR_KILLED
   --  unsupported macro: PA_ERR_INVALIDSERVER PA_ERR_INVALIDSERVER
   --  unsupported macro: PA_ERR_MODINITFAILED PA_ERR_MODINITFAILED
   --  unsupported macro: PA_ERR_BADSTATE PA_ERR_BADSTATE
   --  unsupported macro: PA_ERR_NODATA PA_ERR_NODATA
   --  unsupported macro: PA_ERR_VERSION PA_ERR_VERSION
   --  unsupported macro: PA_ERR_TOOLARGE PA_ERR_TOOLARGE
   --  unsupported macro: PA_ERR_NOTSUPPORTED PA_ERR_NOTSUPPORTED
   --  unsupported macro: PA_ERR_UNKNOWN PA_ERR_UNKNOWN
   --  unsupported macro: PA_ERR_NOEXTENSION PA_ERR_NOEXTENSION
   --  unsupported macro: PA_ERR_OBSOLETE PA_ERR_OBSOLETE
   --  unsupported macro: PA_ERR_NOTIMPLEMENTED PA_ERR_NOTIMPLEMENTED
   --  unsupported macro: PA_ERR_FORKED PA_ERR_FORKED
   --  unsupported macro: PA_ERR_MAX PA_ERR_MAX
   --  arg-macro: function pa_subscription_match_flags (m, t)
   --    return notnot((m) and (1 << ((t) and PA_SUBSCRIPTION_EVENT_FACILITY_MASK)));
   --  unsupported macro: PA_SUBSCRIPTION_MASK_NULL PA_SUBSCRIPTION_MASK_NULL
   --  unsupported macro: PA_SUBSCRIPTION_MASK_SINK PA_SUBSCRIPTION_MASK_SINK
   --  unsupported macro: PA_SUBSCRIPTION_MASK_SOURCE PA_SUBSCRIPTION_MASK_SOURCE
   --  unsupported macro: PA_SUBSCRIPTION_MASK_SINK_INPUT PA_SUBSCRIPTION_MASK_SINK_INPUT
   --  unsupported macro: PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT
   --  unsupported macro: PA_SUBSCRIPTION_MASK_MODULE PA_SUBSCRIPTION_MASK_MODULE
   --  unsupported macro: PA_SUBSCRIPTION_MASK_CLIENT PA_SUBSCRIPTION_MASK_CLIENT
   --  unsupported macro: PA_SUBSCRIPTION_MASK_SAMPLE_CACHE PA_SUBSCRIPTION_MASK_SAMPLE_CACHE
   --  unsupported macro: PA_SUBSCRIPTION_MASK_SERVER PA_SUBSCRIPTION_MASK_SERVER
   --  unsupported macro: PA_SUBSCRIPTION_MASK_AUTOLOAD PA_SUBSCRIPTION_MASK_AUTOLOAD
   --  unsupported macro: PA_SUBSCRIPTION_MASK_CARD PA_SUBSCRIPTION_MASK_CARD
   --  unsupported macro: PA_SUBSCRIPTION_MASK_ALL PA_SUBSCRIPTION_MASK_ALL
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_SINK PA_SUBSCRIPTION_EVENT_SINK
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_SOURCE PA_SUBSCRIPTION_EVENT_SOURCE
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_SINK_INPUT PA_SUBSCRIPTION_EVENT_SINK_INPUT
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_MODULE PA_SUBSCRIPTION_EVENT_MODULE
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_CLIENT PA_SUBSCRIPTION_EVENT_CLIENT
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_SERVER PA_SUBSCRIPTION_EVENT_SERVER
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_AUTOLOAD PA_SUBSCRIPTION_EVENT_AUTOLOAD
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_CARD PA_SUBSCRIPTION_EVENT_CARD
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_FACILITY_MASK PA_SUBSCRIPTION_EVENT_FACILITY_MASK
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_NEW PA_SUBSCRIPTION_EVENT_NEW
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_CHANGE PA_SUBSCRIPTION_EVENT_CHANGE
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_REMOVE PA_SUBSCRIPTION_EVENT_REMOVE
   --  unsupported macro: PA_SUBSCRIPTION_EVENT_TYPE_MASK PA_SUBSCRIPTION_EVENT_TYPE_MASK
   --  unsupported macro: PA_SEEK_RELATIVE PA_SEEK_RELATIVE
   --  unsupported macro: PA_SEEK_ABSOLUTE PA_SEEK_ABSOLUTE
   --  unsupported macro: PA_SEEK_RELATIVE_ON_READ PA_SEEK_RELATIVE_ON_READ
   --  unsupported macro: PA_SEEK_RELATIVE_END PA_SEEK_RELATIVE_END
   --  unsupported macro: PA_SINK_HW_VOLUME_CTRL PA_SINK_HW_VOLUME_CTRL
   --  unsupported macro: PA_SINK_LATENCY PA_SINK_LATENCY
   --  unsupported macro: PA_SINK_HARDWARE PA_SINK_HARDWARE
   --  unsupported macro: PA_SINK_NETWORK PA_SINK_NETWORK
   --  unsupported macro: PA_SINK_HW_MUTE_CTRL PA_SINK_HW_MUTE_CTRL
   --  unsupported macro: PA_SINK_DECIBEL_VOLUME PA_SINK_DECIBEL_VOLUME
   --  unsupported macro: PA_SINK_FLAT_VOLUME PA_SINK_FLAT_VOLUME
   --  unsupported macro: PA_SINK_DYNAMIC_LATENCY PA_SINK_DYNAMIC_LATENCY
   --  unsupported macro: PA_SINK_SET_FORMATS PA_SINK_SET_FORMATS
   --  unsupported macro: PA_SINK_INVALID_STATE PA_SINK_INVALID_STATE
   --  unsupported macro: PA_SINK_RUNNING PA_SINK_RUNNING
   --  unsupported macro: PA_SINK_IDLE PA_SINK_IDLE
   --  unsupported macro: PA_SINK_SUSPENDED PA_SINK_SUSPENDED
   --  unsupported macro: PA_SINK_INIT PA_SINK_INIT
   --  unsupported macro: PA_SINK_UNLINKED PA_SINK_UNLINKED
   --  unsupported macro: PA_SINK_IS_OPENED PA_SINK_IS_OPENED
   --  unsupported macro: PA_SOURCE_HW_VOLUME_CTRL PA_SOURCE_HW_VOLUME_CTRL
   --  unsupported macro: PA_SOURCE_LATENCY PA_SOURCE_LATENCY
   --  unsupported macro: PA_SOURCE_HARDWARE PA_SOURCE_HARDWARE
   --  unsupported macro: PA_SOURCE_NETWORK PA_SOURCE_NETWORK
   --  unsupported macro: PA_SOURCE_HW_MUTE_CTRL PA_SOURCE_HW_MUTE_CTRL
   --  unsupported macro: PA_SOURCE_DECIBEL_VOLUME PA_SOURCE_DECIBEL_VOLUME
   --  unsupported macro: PA_SOURCE_DYNAMIC_LATENCY PA_SOURCE_DYNAMIC_LATENCY
   --  unsupported macro: PA_SOURCE_FLAT_VOLUME PA_SOURCE_FLAT_VOLUME
   --  unsupported macro: PA_SOURCE_INVALID_STATE PA_SOURCE_INVALID_STATE
   --  unsupported macro: PA_SOURCE_RUNNING PA_SOURCE_RUNNING
   --  unsupported macro: PA_SOURCE_IDLE PA_SOURCE_IDLE
   --  unsupported macro: PA_SOURCE_SUSPENDED PA_SOURCE_SUSPENDED
   --  unsupported macro: PA_SOURCE_INIT PA_SOURCE_INIT
   --  unsupported macro: PA_SOURCE_UNLINKED PA_SOURCE_UNLINKED
   --  unsupported macro: PA_SOURCE_IS_OPENED PA_SOURCE_IS_OPENED
   --  unsupported macro: PA_STREAM_EVENT_REQUEST_CORK "request-cork"
   --  unsupported macro: PA_STREAM_EVENT_REQUEST_UNCORK "request-uncork"
   --  unsupported macro: PA_STREAM_EVENT_FORMAT_LOST "format-lost"
   --  unsupported macro: PA_PORT_AVAILABLE_UNKNOWN PA_PORT_AVAILABLE_UNKNOWN
   --  unsupported macro: PA_PORT_AVAILABLE_NO PA_PORT_AVAILABLE_NO
   --  unsupported macro: PA_PORT_AVAILABLE_YES PA_PORT_AVAILABLE_YES
   type pa_context_state is
     (PA_CONTEXT_UNCONNECTED,
      PA_CONTEXT_CONNECTING,
      PA_CONTEXT_AUTHORIZING,
      PA_CONTEXT_SETTING_NAME,
      PA_CONTEXT_READY,
      PA_CONTEXT_FAILED,
      PA_CONTEXT_TERMINATED);
   pragma Convention (C, pa_context_state);  -- /usr/include/pulse/def.h:39

   subtype pa_context_state_t is pa_context_state;

   function PA_CONTEXT_IS_GOOD (x : pa_context_state_t) return int;  -- /usr/include/pulse/def.h:50
   pragma Import (C, PA_CONTEXT_IS_GOOD, "PA_CONTEXT_IS_GOOD");

   type pa_stream_state is
     (PA_STREAM_UNCONNECTED,
      PA_STREAM_CREATING,
      PA_STREAM_READY,
      PA_STREAM_FAILED,
      PA_STREAM_TERMINATED);
   pragma Convention (C, pa_stream_state);  -- /usr/include/pulse/def.h:70

   subtype pa_stream_state_t is pa_stream_state;

   function PA_STREAM_IS_GOOD (x : pa_stream_state_t) return int;  -- /usr/include/pulse/def.h:79
   pragma Import (C, PA_STREAM_IS_GOOD, "PA_STREAM_IS_GOOD");

   type pa_operation_state is
     (PA_OPERATION_RUNNING,
      PA_OPERATION_DONE,
      PA_OPERATION_CANCELLED);
   pragma Convention (C, pa_operation_state);  -- /usr/include/pulse/def.h:95

   subtype pa_operation_state_t is pa_operation_state;

   type pa_context_flags is
     (PA_CONTEXT_NOFLAGS,
      PA_CONTEXT_NOAUTOSPAWN,
      PA_CONTEXT_NOFAIL);
   pragma Convention (C, pa_context_flags);  -- /usr/include/pulse/def.h:117

   subtype pa_context_flags_t is pa_context_flags;

   subtype pa_direction is unsigned;
   PA_DIRECTION_OUTPUT : constant pa_direction := 1;
   PA_DIRECTION_INPUT : constant pa_direction := 2;  -- /usr/include/pulse/def.h:136

   subtype pa_direction_t is pa_direction;

   type pa_device_type is
     (PA_DEVICE_TYPE_SINK,
      PA_DEVICE_TYPE_SOURCE);
   pragma Convention (C, pa_device_type);  -- /usr/include/pulse/def.h:147

   subtype pa_device_type_t is pa_device_type;

   type pa_stream_direction is
     (PA_STREAM_NODIRECTION,
      PA_STREAM_PLAYBACK,
      PA_STREAM_RECORD,
      PA_STREAM_UPLOAD);
   pragma Convention (C, pa_stream_direction);  -- /usr/include/pulse/def.h:158

   subtype pa_stream_direction_t is pa_stream_direction;

   subtype pa_stream_flags is unsigned;
   PA_STREAM_NOFLAGS : constant pa_stream_flags := 0;
   PA_STREAM_START_CORKED : constant pa_stream_flags := 1;
   PA_STREAM_INTERPOLATE_TIMING : constant pa_stream_flags := 2;
   PA_STREAM_NOT_MONOTONIC : constant pa_stream_flags := 4;
   PA_STREAM_AUTO_TIMING_UPDATE : constant pa_stream_flags := 8;
   PA_STREAM_NO_REMAP_CHANNELS : constant pa_stream_flags := 16;
   PA_STREAM_NO_REMIX_CHANNELS : constant pa_stream_flags := 32;
   PA_STREAM_FIX_FORMAT : constant pa_stream_flags := 64;
   PA_STREAM_FIX_RATE : constant pa_stream_flags := 128;
   PA_STREAM_FIX_CHANNELS : constant pa_stream_flags := 256;
   PA_STREAM_DONT_MOVE : constant pa_stream_flags := 512;
   PA_STREAM_VARIABLE_RATE : constant pa_stream_flags := 1024;
   PA_STREAM_PEAK_DETECT : constant pa_stream_flags := 2048;
   PA_STREAM_START_MUTED : constant pa_stream_flags := 4096;
   PA_STREAM_ADJUST_LATENCY : constant pa_stream_flags := 8192;
   PA_STREAM_EARLY_REQUESTS : constant pa_stream_flags := 16384;
   PA_STREAM_DONT_INHIBIT_AUTO_SUSPEND : constant pa_stream_flags := 32768;
   PA_STREAM_START_UNMUTED : constant pa_stream_flags := 65536;
   PA_STREAM_FAIL_ON_SUSPEND : constant pa_stream_flags := 131072;
   PA_STREAM_RELATIVE_VOLUME : constant pa_stream_flags := 262144;
   PA_STREAM_PASSTHROUGH : constant pa_stream_flags := 524288;  -- /usr/include/pulse/def.h:173

   subtype pa_stream_flags_t is pa_stream_flags;

   type pa_buffer_attr is record
      maxlength : aliased Libc.Stdint.uint32_t;  -- /usr/include/pulse/def.h:360
      tlength : aliased Libc.Stdint.uint32_t;  -- /usr/include/pulse/def.h:371
      prebuf : aliased Libc.Stdint.uint32_t;  -- /usr/include/pulse/def.h:389
      minreq : aliased Libc.Stdint.uint32_t;  -- /usr/include/pulse/def.h:400
      fragsize : aliased Libc.Stdint.uint32_t;  -- /usr/include/pulse/def.h:410
   end record;
   pragma Convention (C_Pass_By_Copy, pa_buffer_attr);  -- /usr/include/pulse/def.h:359

   type pa_error_code is
     (PA_OK,
      PA_ERR_ACCESS,
      PA_ERR_COMMAND,
      PA_ERR_INVALID,
      PA_ERR_EXIST,
      PA_ERR_NOENTITY,
      PA_ERR_CONNECTIONREFUSED,
      PA_ERR_PROTOCOL,
      PA_ERR_TIMEOUT,
      PA_ERR_AUTHKEY,
      PA_ERR_INTERNAL,
      PA_ERR_CONNECTIONTERMINATED,
      PA_ERR_KILLED,
      PA_ERR_INVALIDSERVER,
      PA_ERR_MODINITFAILED,
      PA_ERR_BADSTATE,
      PA_ERR_NODATA,
      PA_ERR_VERSION,
      PA_ERR_TOOLARGE,
      PA_ERR_NOTSUPPORTED,
      PA_ERR_UNKNOWN,
      PA_ERR_NOEXTENSION,
      PA_ERR_OBSOLETE,
      PA_ERR_NOTIMPLEMENTED,
      PA_ERR_FORKED,
      PA_ERR_IO,
      PA_ERR_BUSY,
      PA_ERR_MAX);
   pragma Convention (C, pa_error_code);  -- /usr/include/pulse/def.h:427

   subtype pa_error_code_t is pa_error_code;

   subtype pa_subscription_mask is unsigned;
   PA_SUBSCRIPTION_MASK_NULL : constant pa_subscription_mask := 0;
   PA_SUBSCRIPTION_MASK_SINK : constant pa_subscription_mask := 1;
   PA_SUBSCRIPTION_MASK_SOURCE : constant pa_subscription_mask := 2;
   PA_SUBSCRIPTION_MASK_SINK_INPUT : constant pa_subscription_mask := 4;
   PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT : constant pa_subscription_mask := 8;
   PA_SUBSCRIPTION_MASK_MODULE : constant pa_subscription_mask := 16;
   PA_SUBSCRIPTION_MASK_CLIENT : constant pa_subscription_mask := 32;
   PA_SUBSCRIPTION_MASK_SAMPLE_CACHE : constant pa_subscription_mask := 64;
   PA_SUBSCRIPTION_MASK_SERVER : constant pa_subscription_mask := 128;
   PA_SUBSCRIPTION_MASK_AUTOLOAD : constant pa_subscription_mask := 256;
   PA_SUBSCRIPTION_MASK_CARD : constant pa_subscription_mask := 512;
   PA_SUBSCRIPTION_MASK_ALL : constant pa_subscription_mask := 767;  -- /usr/include/pulse/def.h:488

   subtype pa_subscription_mask_t is pa_subscription_mask;

   subtype pa_subscription_event_type is unsigned;
   PA_SUBSCRIPTION_EVENT_SINK : constant pa_subscription_event_type := 0;
   PA_SUBSCRIPTION_EVENT_SOURCE : constant pa_subscription_event_type := 1;
   PA_SUBSCRIPTION_EVENT_SINK_INPUT : constant pa_subscription_event_type := 2;
   PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT : constant pa_subscription_event_type := 3;
   PA_SUBSCRIPTION_EVENT_MODULE : constant pa_subscription_event_type := 4;
   PA_SUBSCRIPTION_EVENT_CLIENT : constant pa_subscription_event_type := 5;
   PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE : constant pa_subscription_event_type := 6;
   PA_SUBSCRIPTION_EVENT_SERVER : constant pa_subscription_event_type := 7;
   PA_SUBSCRIPTION_EVENT_AUTOLOAD : constant pa_subscription_event_type := 8;
   PA_SUBSCRIPTION_EVENT_CARD : constant pa_subscription_event_type := 9;
   PA_SUBSCRIPTION_EVENT_FACILITY_MASK : constant pa_subscription_event_type := 15;
   PA_SUBSCRIPTION_EVENT_NEW : constant pa_subscription_event_type := 0;
   PA_SUBSCRIPTION_EVENT_CHANGE : constant pa_subscription_event_type := 16;
   PA_SUBSCRIPTION_EVENT_REMOVE : constant pa_subscription_event_type := 32;
   PA_SUBSCRIPTION_EVENT_TYPE_MASK : constant pa_subscription_event_type := 48;  -- /usr/include/pulse/def.h:529

   subtype pa_subscription_event_type_t is pa_subscription_event_type;

   type pa_timing_info is record
      timestamp : aliased Libc.Time.GNU.timeval;  -- /usr/include/pulse/def.h:631
      synchronized_clocks : aliased int;  -- /usr/include/pulse/def.h:634
      sink_usec : aliased Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/def.h:641
      source_usec : aliased Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/def.h:646
      transport_usec : aliased Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/def.h:650
      playing : aliased int;  -- /usr/include/pulse/def.h:654
      write_index_corrupt : aliased int;  -- /usr/include/pulse/def.h:661
      write_index : aliased Libc.Stdint.int64_t;  -- /usr/include/pulse/def.h:668
      read_index_corrupt : aliased int;  -- /usr/include/pulse/def.h:674
      read_index : aliased Libc.Stdint.int64_t;  -- /usr/include/pulse/def.h:679
      configured_sink_usec : aliased Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/def.h:685
      configured_source_usec : aliased Pulse.Sample.pa_usec_t;  -- /usr/include/pulse/def.h:688
      since_underrun : aliased Libc.Stdint.int64_t;  -- /usr/include/pulse/def.h:691
   end record;
   pragma Convention (C_Pass_By_Copy, pa_timing_info);  -- /usr/include/pulse/def.h:630

   type pa_spawn_api is record
      prefork : access procedure;  -- /usr/include/pulse/def.h:707
      postfork : access procedure;  -- /usr/include/pulse/def.h:711
      atfork : access procedure;  -- /usr/include/pulse/def.h:715
   end record;
   pragma Convention (C_Pass_By_Copy, pa_spawn_api);  -- /usr/include/pulse/def.h:706

   type pa_seek_mode is
     (PA_SEEK_RELATIVE,
      PA_SEEK_ABSOLUTE,
      PA_SEEK_RELATIVE_ON_READ,
      PA_SEEK_RELATIVE_END);
   pragma Convention (C, pa_seek_mode);  -- /usr/include/pulse/def.h:724

   subtype pa_seek_mode_t is pa_seek_mode;

   subtype pa_sink_flags is unsigned;
   PA_SINK_NOFLAGS : constant pa_sink_flags := 0;
   PA_SINK_HW_VOLUME_CTRL : constant pa_sink_flags := 1;
   PA_SINK_LATENCY : constant pa_sink_flags := 2;
   PA_SINK_HARDWARE : constant pa_sink_flags := 4;
   PA_SINK_NETWORK : constant pa_sink_flags := 8;
   PA_SINK_HW_MUTE_CTRL : constant pa_sink_flags := 16;
   PA_SINK_DECIBEL_VOLUME : constant pa_sink_flags := 32;
   PA_SINK_FLAT_VOLUME : constant pa_sink_flags := 64;
   PA_SINK_DYNAMIC_LATENCY : constant pa_sink_flags := 128;
   PA_SINK_SET_FORMATS : constant pa_sink_flags := 256;  -- /usr/include/pulse/def.h:746

   subtype pa_sink_flags_t is pa_sink_flags;

   subtype pa_sink_state is unsigned;
   PA_SINK_INVALID_STATE : constant pa_sink_state := -1;
   PA_SINK_RUNNING : constant pa_sink_state := 0;
   PA_SINK_IDLE : constant pa_sink_state := 1;
   PA_SINK_SUSPENDED : constant pa_sink_state := 2;
   PA_SINK_INIT : constant pa_sink_state := -2;
   PA_SINK_UNLINKED : constant pa_sink_state := -3;  -- /usr/include/pulse/def.h:820

   subtype pa_sink_state_t is pa_sink_state;

   function PA_SINK_IS_OPENED (x : pa_sink_state_t) return int;  -- /usr/include/pulse/def.h:850
   pragma Import (C, PA_SINK_IS_OPENED, "PA_SINK_IS_OPENED");

   function PA_SINK_IS_RUNNING (x : pa_sink_state_t) return int;  -- /usr/include/pulse/def.h:855
   pragma Import (C, PA_SINK_IS_RUNNING, "PA_SINK_IS_RUNNING");

   subtype pa_source_flags is unsigned;
   PA_SOURCE_NOFLAGS : constant pa_source_flags := 0;
   PA_SOURCE_HW_VOLUME_CTRL : constant pa_source_flags := 1;
   PA_SOURCE_LATENCY : constant pa_source_flags := 2;
   PA_SOURCE_HARDWARE : constant pa_source_flags := 4;
   PA_SOURCE_NETWORK : constant pa_source_flags := 8;
   PA_SOURCE_HW_MUTE_CTRL : constant pa_source_flags := 16;
   PA_SOURCE_DECIBEL_VOLUME : constant pa_source_flags := 32;
   PA_SOURCE_DYNAMIC_LATENCY : constant pa_source_flags := 64;
   PA_SOURCE_FLAT_VOLUME : constant pa_source_flags := 128;  -- /usr/include/pulse/def.h:870

   subtype pa_source_flags_t is pa_source_flags;

   subtype pa_source_state is unsigned;
   PA_SOURCE_INVALID_STATE : constant pa_source_state := -1;
   PA_SOURCE_RUNNING : constant pa_source_state := 0;
   PA_SOURCE_IDLE : constant pa_source_state := 1;
   PA_SOURCE_SUSPENDED : constant pa_source_state := 2;
   PA_SOURCE_INIT : constant pa_source_state := -2;
   PA_SOURCE_UNLINKED : constant pa_source_state := -3;  -- /usr/include/pulse/def.h:936

   subtype pa_source_state_t is pa_source_state;

   function PA_SOURCE_IS_OPENED (x : pa_source_state_t) return int;  -- /usr/include/pulse/def.h:966
   pragma Import (C, PA_SOURCE_IS_OPENED, "PA_SOURCE_IS_OPENED");

   function PA_SOURCE_IS_RUNNING (x : pa_source_state_t) return int;  -- /usr/include/pulse/def.h:971
   pragma Import (C, PA_SOURCE_IS_RUNNING, "PA_SOURCE_IS_RUNNING");

   type pa_free_cb_t is access procedure (arg1 : System.Address);
   pragma Convention (C, pa_free_cb_t);  -- /usr/include/pulse/def.h:986

   type pa_port_available is
     (PA_PORT_AVAILABLE_UNKNOWN,
      PA_PORT_AVAILABLE_NO,
      PA_PORT_AVAILABLE_YES);
   pragma Convention (C, pa_port_available);  -- /usr/include/pulse/def.h:1008

   subtype pa_port_available_t is pa_port_available;

end Pulse.Def;
