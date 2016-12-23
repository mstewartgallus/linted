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

with Pulse.Sample;
with Pulse.Channelmap;
with Pulse.Volume;
with Pulse.Def;
with Pulse.Context;
limited with Pulse.Format;

package Pulse.Introspect with
     Spark_Mode => Off is

   type pa_sink_port_info is record
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:202
      description : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:203
      priority : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:204
      available : aliased int;  -- /usr/include/pulse/introspect.h:205
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_sink_port_info);  -- /usr/include/pulse/introspect.h:201

   type pa_sink_info is record
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:212
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:213
      description : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:214
      sample_spec : aliased Pulse.Sample
        .pa_sample_spec;  -- /usr/include/pulse/introspect.h:215
      channel_map : aliased Pulse.Channelmap
        .pa_channel_map;  -- /usr/include/pulse/introspect.h:216
      owner_module : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:217
      volume : aliased Pulse.Volume
        .pa_cvolume;  -- /usr/include/pulse/introspect.h:218
      mute : aliased int;  -- /usr/include/pulse/introspect.h:219
      monitor_source : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:220
      monitor_source_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:221
      latency : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:222
      driver : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:223
      flags : aliased Pulse.Def
        .pa_sink_flags_t;  -- /usr/include/pulse/introspect.h:224
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:225
      configured_latency : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:226
      base_volume : aliased Pulse.Volume
        .pa_volume_t;  -- /usr/include/pulse/introspect.h:227
      state : aliased Pulse.Def
        .pa_sink_state_t;  -- /usr/include/pulse/introspect.h:228
      n_volume_steps : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:229
      card : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:230
      n_ports : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:231
      ports : System.Address;  -- /usr/include/pulse/introspect.h:232
      active_port : access pa_sink_port_info;  -- /usr/include/pulse/introspect.h:233
      n_formats : aliased Libc.Stdint
        .uint8_t;  -- /usr/include/pulse/introspect.h:234
      formats : System.Address;  -- /usr/include/pulse/introspect.h:235
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_sink_info);  -- /usr/include/pulse/introspect.h:211

   type pa_sink_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_sink_info_cb_t);  -- /usr/include/pulse/introspect.h:239

   function pa_context_get_sink_info_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_sink_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:242
   pragma Import
     (C,
      pa_context_get_sink_info_by_name,
      "pa_context_get_sink_info_by_name");

   function pa_context_get_sink_info_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_sink_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:245
   pragma Import
     (C,
      pa_context_get_sink_info_by_index,
      "pa_context_get_sink_info_by_index");

   function pa_context_get_sink_info_list
     (c : System.Address;
      cb : pa_sink_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:248
   pragma Import
     (C,
      pa_context_get_sink_info_list,
      "pa_context_get_sink_info_list");

   function pa_context_set_sink_volume_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      volume : access constant Pulse.Volume.pa_cvolume;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:251
   pragma Import
     (C,
      pa_context_set_sink_volume_by_index,
      "pa_context_set_sink_volume_by_index");

   function pa_context_set_sink_volume_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      volume : access constant Pulse.Volume.pa_cvolume;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:254
   pragma Import
     (C,
      pa_context_set_sink_volume_by_name,
      "pa_context_set_sink_volume_by_name");

   function pa_context_set_sink_mute_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      mute : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:257
   pragma Import
     (C,
      pa_context_set_sink_mute_by_index,
      "pa_context_set_sink_mute_by_index");

   function pa_context_set_sink_mute_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      mute : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:260
   pragma Import
     (C,
      pa_context_set_sink_mute_by_name,
      "pa_context_set_sink_mute_by_name");

   function pa_context_suspend_sink_by_name
     (c : System.Address;
      sink_name : Interfaces.C.Strings.chars_ptr;
      suspend : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:263
   pragma Import
     (C,
      pa_context_suspend_sink_by_name,
      "pa_context_suspend_sink_by_name");

   function pa_context_suspend_sink_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      suspend : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:266
   pragma Import
     (C,
      pa_context_suspend_sink_by_index,
      "pa_context_suspend_sink_by_index");

   function pa_context_set_sink_port_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      port : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:269
   pragma Import
     (C,
      pa_context_set_sink_port_by_index,
      "pa_context_set_sink_port_by_index");

   function pa_context_set_sink_port_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      port : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:272
   pragma Import
     (C,
      pa_context_set_sink_port_by_name,
      "pa_context_set_sink_port_by_name");

   type pa_source_port_info is record
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:282
      description : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:283
      priority : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:284
      available : aliased int;  -- /usr/include/pulse/introspect.h:285
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_source_port_info);  -- /usr/include/pulse/introspect.h:281

   type pa_source_info is record
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:292
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:293
      description : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:294
      sample_spec : aliased Pulse.Sample
        .pa_sample_spec;  -- /usr/include/pulse/introspect.h:295
      channel_map : aliased Pulse.Channelmap
        .pa_channel_map;  -- /usr/include/pulse/introspect.h:296
      owner_module : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:297
      volume : aliased Pulse.Volume
        .pa_cvolume;  -- /usr/include/pulse/introspect.h:298
      mute : aliased int;  -- /usr/include/pulse/introspect.h:299
      monitor_of_sink : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:300
      monitor_of_sink_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:301
      latency : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:302
      driver : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:303
      flags : aliased Pulse.Def
        .pa_source_flags_t;  -- /usr/include/pulse/introspect.h:304
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:305
      configured_latency : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:306
      base_volume : aliased Pulse.Volume
        .pa_volume_t;  -- /usr/include/pulse/introspect.h:307
      state : aliased Pulse.Def
        .pa_source_state_t;  -- /usr/include/pulse/introspect.h:308
      n_volume_steps : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:309
      card : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:310
      n_ports : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:311
      ports : System.Address;  -- /usr/include/pulse/introspect.h:312
      active_port : access pa_source_port_info;  -- /usr/include/pulse/introspect.h:313
      n_formats : aliased Libc.Stdint
        .uint8_t;  -- /usr/include/pulse/introspect.h:314
      formats : System.Address;  -- /usr/include/pulse/introspect.h:315
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_source_info);  -- /usr/include/pulse/introspect.h:291

   type pa_source_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_source_info_cb_t);  -- /usr/include/pulse/introspect.h:319

   function pa_context_get_source_info_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_source_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:322
   pragma Import
     (C,
      pa_context_get_source_info_by_name,
      "pa_context_get_source_info_by_name");

   function pa_context_get_source_info_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_source_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:325
   pragma Import
     (C,
      pa_context_get_source_info_by_index,
      "pa_context_get_source_info_by_index");

   function pa_context_get_source_info_list
     (c : System.Address;
      cb : pa_source_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:328
   pragma Import
     (C,
      pa_context_get_source_info_list,
      "pa_context_get_source_info_list");

   function pa_context_set_source_volume_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      volume : access constant Pulse.Volume.pa_cvolume;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:331
   pragma Import
     (C,
      pa_context_set_source_volume_by_index,
      "pa_context_set_source_volume_by_index");

   function pa_context_set_source_volume_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      volume : access constant Pulse.Volume.pa_cvolume;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:334
   pragma Import
     (C,
      pa_context_set_source_volume_by_name,
      "pa_context_set_source_volume_by_name");

   function pa_context_set_source_mute_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      mute : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:337
   pragma Import
     (C,
      pa_context_set_source_mute_by_index,
      "pa_context_set_source_mute_by_index");

   function pa_context_set_source_mute_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      mute : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:340
   pragma Import
     (C,
      pa_context_set_source_mute_by_name,
      "pa_context_set_source_mute_by_name");

   function pa_context_suspend_source_by_name
     (c : System.Address;
      source_name : Interfaces.C.Strings.chars_ptr;
      suspend : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:343
   pragma Import
     (C,
      pa_context_suspend_source_by_name,
      "pa_context_suspend_source_by_name");

   function pa_context_suspend_source_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      suspend : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:346
   pragma Import
     (C,
      pa_context_suspend_source_by_index,
      "pa_context_suspend_source_by_index");

   function pa_context_set_source_port_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      port : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:349
   pragma Import
     (C,
      pa_context_set_source_port_by_index,
      "pa_context_set_source_port_by_index");

   function pa_context_set_source_port_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      port : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:352
   pragma Import
     (C,
      pa_context_set_source_port_by_name,
      "pa_context_set_source_port_by_name");

   type pa_server_info is record
      user_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:362
      host_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:363
      server_version : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:364
      server_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:365
      sample_spec : aliased Pulse.Sample
        .pa_sample_spec;  -- /usr/include/pulse/introspect.h:366
      default_sink_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:367
      default_source_name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:368
      cookie : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:369
      channel_map : aliased Pulse.Channelmap
        .pa_channel_map;  -- /usr/include/pulse/introspect.h:370
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_server_info);  -- /usr/include/pulse/introspect.h:361

   type pa_server_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_server_info_cb_t);  -- /usr/include/pulse/introspect.h:374

   function pa_context_get_server_info
     (c : System.Address;
      cb : pa_server_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:377
   pragma Import (C, pa_context_get_server_info, "pa_context_get_server_info");

   type pa_module_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:387
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:388
      argument : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:389
      n_used : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:390
      auto_unload : aliased int;  -- /usr/include/pulse/introspect.h:392
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:394
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_module_info);  -- /usr/include/pulse/introspect.h:386

   type pa_module_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_module_info_cb_t);  -- /usr/include/pulse/introspect.h:398

   function pa_context_get_module_info
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_module_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:401
   pragma Import (C, pa_context_get_module_info, "pa_context_get_module_info");

   function pa_context_get_module_info_list
     (c : System.Address;
      cb : pa_module_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:404
   pragma Import
     (C,
      pa_context_get_module_info_list,
      "pa_context_get_module_info_list");

   type pa_context_index_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : Libc.Stdint.uint32_t;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_context_index_cb_t);  -- /usr/include/pulse/introspect.h:407

   function pa_context_load_module
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      argument : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_index_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:410
   pragma Import (C, pa_context_load_module, "pa_context_load_module");

   function pa_context_unload_module
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:413
   pragma Import (C, pa_context_unload_module, "pa_context_unload_module");

   type pa_client_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:423
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:424
      owner_module : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:425
      driver : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:426
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:427
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_client_info);  -- /usr/include/pulse/introspect.h:422

   type pa_client_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_client_info_cb_t);  -- /usr/include/pulse/introspect.h:431

   function pa_context_get_client_info
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_client_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:434
   pragma Import (C, pa_context_get_client_info, "pa_context_get_client_info");

   function pa_context_get_client_info_list
     (c : System.Address;
      cb : pa_client_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:437
   pragma Import
     (C,
      pa_context_get_client_info_list,
      "pa_context_get_client_info_list");

   function pa_context_kill_client
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:440
   pragma Import (C, pa_context_kill_client, "pa_context_kill_client");

   type pa_card_profile_info is record
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:450
      description : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:451
      n_sinks : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:452
      n_sources : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:453
      priority : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:454
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_card_profile_info);  -- /usr/include/pulse/introspect.h:449

   type pa_card_port_info is record
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:461
      description : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:462
      priority : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:463
      available : aliased int;  -- /usr/include/pulse/introspect.h:464
      direction : aliased int;  -- /usr/include/pulse/introspect.h:465
      n_profiles : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:466
      profiles : System.Address;  -- /usr/include/pulse/introspect.h:467
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:468
      latency_offset : aliased Libc.Stdint
        .int64_t;  -- /usr/include/pulse/introspect.h:469
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_card_port_info);  -- /usr/include/pulse/introspect.h:460

   type pa_card_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:476
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:477
      owner_module : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:478
      driver : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:479
      n_profiles : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:480
      profiles : access pa_card_profile_info;  -- /usr/include/pulse/introspect.h:481
      active_profile : access pa_card_profile_info;  -- /usr/include/pulse/introspect.h:482
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:483
      n_ports : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:484
      ports : System.Address;  -- /usr/include/pulse/introspect.h:485
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_card_info);  -- /usr/include/pulse/introspect.h:475

   type pa_card_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_card_info_cb_t);  -- /usr/include/pulse/introspect.h:489

   function pa_context_get_card_info_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_card_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:492
   pragma Import
     (C,
      pa_context_get_card_info_by_index,
      "pa_context_get_card_info_by_index");

   function pa_context_get_card_info_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_card_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:495
   pragma Import
     (C,
      pa_context_get_card_info_by_name,
      "pa_context_get_card_info_by_name");

   function pa_context_get_card_info_list
     (c : System.Address;
      cb : pa_card_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:498
   pragma Import
     (C,
      pa_context_get_card_info_list,
      "pa_context_get_card_info_list");

   function pa_context_set_card_profile_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      profile : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:501
   pragma Import
     (C,
      pa_context_set_card_profile_by_index,
      "pa_context_set_card_profile_by_index");

   function pa_context_set_card_profile_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      profile : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:504
   pragma Import
     (C,
      pa_context_set_card_profile_by_name,
      "pa_context_set_card_profile_by_name");

   function pa_context_set_port_latency_offset
     (c : System.Address;
      card_name : Interfaces.C.Strings.chars_ptr;
      port_name : Interfaces.C.Strings.chars_ptr;
      offset : Libc.Stdint.int64_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:507
   pragma Import
     (C,
      pa_context_set_port_latency_offset,
      "pa_context_set_port_latency_offset");

   type pa_sink_input_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:517
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:518
      owner_module : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:519
      client : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:520
      sink : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:521
      sample_spec : aliased Pulse.Sample
        .pa_sample_spec;  -- /usr/include/pulse/introspect.h:522
      channel_map : aliased Pulse.Channelmap
        .pa_channel_map;  -- /usr/include/pulse/introspect.h:523
      volume : aliased Pulse.Volume
        .pa_cvolume;  -- /usr/include/pulse/introspect.h:524
      buffer_usec : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:525
      sink_usec : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:526
      resample_method : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:527
      driver : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:528
      mute : aliased int;  -- /usr/include/pulse/introspect.h:529
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:530
      corked : aliased int;  -- /usr/include/pulse/introspect.h:531
      has_volume : aliased int;  -- /usr/include/pulse/introspect.h:532
      volume_writable : aliased int;  -- /usr/include/pulse/introspect.h:533
      format : access Pulse.Format
        .pa_format_info;  -- /usr/include/pulse/introspect.h:534
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_sink_input_info);  -- /usr/include/pulse/introspect.h:516

   type pa_sink_input_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_sink_input_info_cb_t);  -- /usr/include/pulse/introspect.h:538

   function pa_context_get_sink_input_info
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_sink_input_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:541
   pragma Import
     (C,
      pa_context_get_sink_input_info,
      "pa_context_get_sink_input_info");

   function pa_context_get_sink_input_info_list
     (c : System.Address;
      cb : pa_sink_input_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:544
   pragma Import
     (C,
      pa_context_get_sink_input_info_list,
      "pa_context_get_sink_input_info_list");

   function pa_context_move_sink_input_by_name
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      sink_name : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:547
   pragma Import
     (C,
      pa_context_move_sink_input_by_name,
      "pa_context_move_sink_input_by_name");

   function pa_context_move_sink_input_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      sink_idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:550
   pragma Import
     (C,
      pa_context_move_sink_input_by_index,
      "pa_context_move_sink_input_by_index");

   function pa_context_set_sink_input_volume
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      volume : access constant Pulse.Volume.pa_cvolume;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:553
   pragma Import
     (C,
      pa_context_set_sink_input_volume,
      "pa_context_set_sink_input_volume");

   function pa_context_set_sink_input_mute
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      mute : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:556
   pragma Import
     (C,
      pa_context_set_sink_input_mute,
      "pa_context_set_sink_input_mute");

   function pa_context_kill_sink_input
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:559
   pragma Import (C, pa_context_kill_sink_input, "pa_context_kill_sink_input");

   type pa_source_output_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:569
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:570
      owner_module : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:571
      client : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:572
      source : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:573
      sample_spec : aliased Pulse.Sample
        .pa_sample_spec;  -- /usr/include/pulse/introspect.h:574
      channel_map : aliased Pulse.Channelmap
        .pa_channel_map;  -- /usr/include/pulse/introspect.h:575
      buffer_usec : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:576
      source_usec : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:577
      resample_method : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:578
      driver : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:579
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:580
      corked : aliased int;  -- /usr/include/pulse/introspect.h:581
      volume : aliased Pulse.Volume
        .pa_cvolume;  -- /usr/include/pulse/introspect.h:582
      mute : aliased int;  -- /usr/include/pulse/introspect.h:583
      has_volume : aliased int;  -- /usr/include/pulse/introspect.h:584
      volume_writable : aliased int;  -- /usr/include/pulse/introspect.h:585
      format : access Pulse.Format
        .pa_format_info;  -- /usr/include/pulse/introspect.h:586
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_source_output_info);  -- /usr/include/pulse/introspect.h:568

   type pa_source_output_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_source_output_info_cb_t);  -- /usr/include/pulse/introspect.h:590

   function pa_context_get_source_output_info
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_source_output_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:593
   pragma Import
     (C,
      pa_context_get_source_output_info,
      "pa_context_get_source_output_info");

   function pa_context_get_source_output_info_list
     (c : System.Address;
      cb : pa_source_output_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:596
   pragma Import
     (C,
      pa_context_get_source_output_info_list,
      "pa_context_get_source_output_info_list");

   function pa_context_move_source_output_by_name
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      source_name : Interfaces.C.Strings.chars_ptr;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:599
   pragma Import
     (C,
      pa_context_move_source_output_by_name,
      "pa_context_move_source_output_by_name");

   function pa_context_move_source_output_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      source_idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:602
   pragma Import
     (C,
      pa_context_move_source_output_by_index,
      "pa_context_move_source_output_by_index");

   function pa_context_set_source_output_volume
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      volume : access constant Pulse.Volume.pa_cvolume;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:605
   pragma Import
     (C,
      pa_context_set_source_output_volume,
      "pa_context_set_source_output_volume");

   function pa_context_set_source_output_mute
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      mute : int;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:608
   pragma Import
     (C,
      pa_context_set_source_output_mute,
      "pa_context_set_source_output_mute");

   function pa_context_kill_source_output
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:611
   pragma Import
     (C,
      pa_context_kill_source_output,
      "pa_context_kill_source_output");

   type pa_stat_info is record
      memblock_total : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:621
      memblock_total_size : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:622
      memblock_allocated : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:623
      memblock_allocated_size : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:624
      scache_size : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:625
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_stat_info);  -- /usr/include/pulse/introspect.h:620

   type pa_stat_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : System.Address);
   pragma Convention
     (C,
      pa_stat_info_cb_t);  -- /usr/include/pulse/introspect.h:629

   function pa_context_stat
     (c : System.Address;
      cb : pa_stat_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:632
   pragma Import (C, pa_context_stat, "pa_context_stat");

   type pa_sample_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:642
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:643
      volume : aliased Pulse.Volume
        .pa_cvolume;  -- /usr/include/pulse/introspect.h:644
      sample_spec : aliased Pulse.Sample
        .pa_sample_spec;  -- /usr/include/pulse/introspect.h:645
      channel_map : aliased Pulse.Channelmap
        .pa_channel_map;  -- /usr/include/pulse/introspect.h:646
      duration : aliased Pulse.Sample
        .pa_usec_t;  -- /usr/include/pulse/introspect.h:647
      bytes : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:648
      lazy : aliased int;  -- /usr/include/pulse/introspect.h:649
      filename : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:650
      proplist : System.Address;  -- /usr/include/pulse/introspect.h:651
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_sample_info);  -- /usr/include/pulse/introspect.h:641

   type pa_sample_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_sample_info_cb_t);  -- /usr/include/pulse/introspect.h:655

   function pa_context_get_sample_info_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_sample_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:658
   pragma Import
     (C,
      pa_context_get_sample_info_by_name,
      "pa_context_get_sample_info_by_name");

   function pa_context_get_sample_info_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_sample_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:661
   pragma Import
     (C,
      pa_context_get_sample_info_by_index,
      "pa_context_get_sample_info_by_index");

   function pa_context_get_sample_info_list
     (c : System.Address;
      cb : pa_sample_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:664
   pragma Import
     (C,
      pa_context_get_sample_info_list,
      "pa_context_get_sample_info_list");

   type pa_autoload_type is (PA_AUTOLOAD_SINK, PA_AUTOLOAD_SOURCE);
   pragma Convention
     (C,
      pa_autoload_type);  -- /usr/include/pulse/introspect.h:673

   subtype pa_autoload_type_t is pa_autoload_type;

   type pa_autoload_info is record
      index : aliased Libc.Stdint
        .uint32_t;  -- /usr/include/pulse/introspect.h:682
      name : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:683
      c_type : aliased pa_autoload_type_t;  -- /usr/include/pulse/introspect.h:684
      module : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:685
      argument : Interfaces.C.Strings
        .chars_ptr;  -- /usr/include/pulse/introspect.h:686
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_autoload_info);  -- /usr/include/pulse/introspect.h:681

   type pa_autoload_info_cb_t is access procedure
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int;
      arg4 : System.Address);
   pragma Convention
     (C,
      pa_autoload_info_cb_t);  -- /usr/include/pulse/introspect.h:690

   function pa_context_get_autoload_info_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : pa_autoload_type_t;
      cb : pa_autoload_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:693
   pragma Import
     (C,
      pa_context_get_autoload_info_by_name,
      "pa_context_get_autoload_info_by_name");

   function pa_context_get_autoload_info_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : pa_autoload_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:696
   pragma Import
     (C,
      pa_context_get_autoload_info_by_index,
      "pa_context_get_autoload_info_by_index");

   function pa_context_get_autoload_info_list
     (c : System.Address;
      cb : pa_autoload_info_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:699
   pragma Import
     (C,
      pa_context_get_autoload_info_list,
      "pa_context_get_autoload_info_list");

   function pa_context_add_autoload
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : pa_autoload_type_t;
      module : Interfaces.C.Strings.chars_ptr;
      argument : Interfaces.C.Strings.chars_ptr;
      arg6 : pa_context_index_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:702
   pragma Import (C, pa_context_add_autoload, "pa_context_add_autoload");

   function pa_context_remove_autoload_by_name
     (c : System.Address;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : pa_autoload_type_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:705
   pragma Import
     (C,
      pa_context_remove_autoload_by_name,
      "pa_context_remove_autoload_by_name");

   function pa_context_remove_autoload_by_index
     (c : System.Address;
      idx : Libc.Stdint.uint32_t;
      cb : Pulse.Context.pa_context_success_cb_t;
      userdata : System.Address)
     return System.Address;  -- /usr/include/pulse/introspect.h:708
   pragma Import
     (C,
      pa_context_remove_autoload_by_index,
      "pa_context_remove_autoload_by_index");

end Pulse.Introspect;
