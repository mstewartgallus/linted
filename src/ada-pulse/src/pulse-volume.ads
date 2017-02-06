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

with Interfaces.C;
with Interfaces.C.Strings;

with Libc.Stdint;
with Libc.Stddef;

with Pulse.Channelmap;
limited with Pulse.Sample;

package Pulse.Volume with
     Spark_Mode => Off is
   subtype pa_volume_t is
     Libc.Stdint.uint32_t;  -- /usr/include/pulse/volume.h:107

   PA_VOLUME_NORM : constant pa_volume_t := 16#10000#;
   PA_VOLUME_MUTED : constant pa_volume_t := 0;
   --  unsupported macro: PA_VOLUME_MAX ((pa_volume_t) UINT32_MAX/2)
   --  unsupported macro: PA_VOLUME_UI_MAX (pa_sw_volume_from_dB(+11.0))
   --  unsupported macro: PA_VOLUME_INVALID ((pa_volume_t) UINT32_MAX)
   --  arg-macro: function PA_VOLUME_IS_VALID (v)
   --    return (v) <= PA_VOLUME_MAX;
   --  arg-macro: function PA_CLAMP_VOLUME (v)
   --    return PA_CLAMP_UNLIKELY((v), PA_VOLUME_MUTED, PA_VOLUME_MAX);
   --  arg-macro: procedure pa_cvolume_reset (a, n)
   --    pa_cvolume_set((a), (n), PA_VOLUME_NORM)
   --  arg-macro: procedure pa_cvolume_mute (a, n)
   --    pa_cvolume_set((a), (n), PA_VOLUME_MUTED)
   --  unsupported macro: PA_CVOLUME_SNPRINT_MAX 320
   --  unsupported macro: PA_SW_CVOLUME_SNPRINT_DB_MAX 448
   --  unsupported macro: PA_VOLUME_SNPRINT_MAX 10
   --  unsupported macro: PA_SW_VOLUME_SNPRINT_DB_MAX 10
   --  arg-macro: procedure pa_cvolume_is_muted (a)
   --    pa_cvolume_channels_equal_to((a), PA_VOLUME_MUTED)
   --  arg-macro: procedure pa_cvolume_is_norm (a)
   --    pa_cvolume_channels_equal_to((a), PA_VOLUME_NORM)
   --  unsupported macro: PA_DECIBEL_MININFTY ((double) -200.0)

   type pa_cvolume_values_array is array (0 .. 31) of aliased pa_volume_t;
   type pa_cvolume is record
      channels : aliased Libc.Stdint
        .uint8_t;  -- /usr/include/pulse/volume.h:136
      values : aliased pa_cvolume_values_array;  -- /usr/include/pulse/volume.h:137
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_cvolume);  -- /usr/include/pulse/volume.h:135

   function pa_cvolume_equal
     (a : System.Address;
      b : System.Address)
     return Interfaces.C.int;  -- /usr/include/pulse/volume.h:141
   pragma Import (C, pa_cvolume_equal, "pa_cvolume_equal");

   function pa_cvolume_init
     (a : access pa_cvolume)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:146
   pragma Import (C, pa_cvolume_init, "pa_cvolume_init");

   function pa_cvolume_set
     (a : access pa_cvolume;
      channels : Interfaces.C.unsigned;
      v : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:155
   pragma Import (C, pa_cvolume_set, "pa_cvolume_set");

   function pa_cvolume_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      c : System.Address)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/volume.h:165
   pragma Import (C, pa_cvolume_snprint, "pa_cvolume_snprint");

   function pa_sw_cvolume_snprint_dB
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      c : System.Address)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/volume.h:175
   pragma Import (C, pa_sw_cvolume_snprint_dB, "pa_sw_cvolume_snprint_dB");

   function pa_volume_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      v : pa_volume_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/volume.h:185
   pragma Import (C, pa_volume_snprint, "pa_volume_snprint");

   function pa_sw_volume_snprint_dB
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      v : pa_volume_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/volume.h:195
   pragma Import (C, pa_sw_volume_snprint_dB, "pa_sw_volume_snprint_dB");

   function pa_cvolume_avg
     (a : System.Address)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:198
   pragma Import (C, pa_cvolume_avg, "pa_cvolume_avg");

   function pa_cvolume_avg_mask
     (a : System.Address;
      cm : access constant Pulse.Channelmap.pa_channel_map;
      mask : Pulse.Channelmap.pa_channel_position_mask_t)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:205
   pragma Import (C, pa_cvolume_avg_mask, "pa_cvolume_avg_mask");

   function pa_cvolume_max
     (a : System.Address)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:208
   pragma Import (C, pa_cvolume_max, "pa_cvolume_max");

   function pa_cvolume_max_mask
     (a : System.Address;
      cm : access constant Pulse.Channelmap.pa_channel_map;
      mask : Pulse.Channelmap.pa_channel_position_mask_t)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:215
   pragma Import (C, pa_cvolume_max_mask, "pa_cvolume_max_mask");

   function pa_cvolume_min
     (a : System.Address)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:218
   pragma Import (C, pa_cvolume_min, "pa_cvolume_min");

   function pa_cvolume_min_mask
     (a : System.Address;
      cm : access constant Pulse.Channelmap.pa_channel_map;
      mask : Pulse.Channelmap.pa_channel_position_mask_t)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:225
   pragma Import (C, pa_cvolume_min_mask, "pa_cvolume_min_mask");

   function pa_cvolume_valid
     (v : System.Address)
     return Interfaces.C.int;  -- /usr/include/pulse/volume.h:228
   pragma Import (C, pa_cvolume_valid, "pa_cvolume_valid");

   function pa_cvolume_channels_equal_to
     (a : System.Address;
      v : pa_volume_t)
     return Interfaces.C.int;  -- /usr/include/pulse/volume.h:231
   pragma Import
     (C,
      pa_cvolume_channels_equal_to,
      "pa_cvolume_channels_equal_to");

   function pa_sw_volume_multiply
     (a : pa_volume_t;
      b : pa_volume_t) return pa_volume_t;  -- /usr/include/pulse/volume.h:242
   pragma Import (C, pa_sw_volume_multiply, "pa_sw_volume_multiply");

   function pa_sw_cvolume_multiply
     (dest : access pa_cvolume;
      a : System.Address;
      b : System.Address)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:247
   pragma Import (C, pa_sw_cvolume_multiply, "pa_sw_cvolume_multiply");

   function pa_sw_cvolume_multiply_scalar
     (dest : access pa_cvolume;
      a : System.Address;
      b : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:253
   pragma Import
     (C,
      pa_sw_cvolume_multiply_scalar,
      "pa_sw_cvolume_multiply_scalar");

   function pa_sw_volume_divide
     (a : pa_volume_t;
      b : pa_volume_t) return pa_volume_t;  -- /usr/include/pulse/volume.h:259
   pragma Import (C, pa_sw_volume_divide, "pa_sw_volume_divide");

   function pa_sw_cvolume_divide
     (dest : access pa_cvolume;
      a : System.Address;
      b : System.Address)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:264
   pragma Import (C, pa_sw_cvolume_divide, "pa_sw_cvolume_divide");

   function pa_sw_cvolume_divide_scalar
     (dest : access pa_cvolume;
      a : System.Address;
      b : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:270
   pragma Import
     (C,
      pa_sw_cvolume_divide_scalar,
      "pa_sw_cvolume_divide_scalar");

   function pa_sw_volume_from_dB
     (f : Interfaces.C.double)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:273
   pragma Import (C, pa_sw_volume_from_dB, "pa_sw_volume_from_dB");

   function pa_sw_volume_to_dB
     (v : pa_volume_t)
     return Interfaces.C.double;  -- /usr/include/pulse/volume.h:276
   pragma Import (C, pa_sw_volume_to_dB, "pa_sw_volume_to_dB");

   function pa_sw_volume_from_linear
     (v : Interfaces.C.double)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:280
   pragma Import (C, pa_sw_volume_from_linear, "pa_sw_volume_from_linear");

   function pa_sw_volume_to_linear
     (v : pa_volume_t)
     return Interfaces.C.double;  -- /usr/include/pulse/volume.h:283
   pragma Import (C, pa_sw_volume_to_linear, "pa_sw_volume_to_linear");

   function pa_cvolume_remap
     (v : access pa_cvolume;
      from : access constant Pulse.Channelmap.pa_channel_map;
      to : access constant Pulse.Channelmap.pa_channel_map)
      return access pa_cvolume;  -- /usr/include/pulse/volume.h:293
   pragma Import (C, pa_cvolume_remap, "pa_cvolume_remap");

   function pa_cvolume_compatible
     (v : System.Address;
      ss : Pulse.Sample.pa_sample_spec)
     return Interfaces.C.int;  -- /usr/include/pulse/volume.h:297
   pragma Import (C, pa_cvolume_compatible, "pa_cvolume_compatible");

   function pa_cvolume_compatible_with_channel_map
     (v : System.Address;
      cm : access constant Pulse.Channelmap.pa_channel_map)
      return Interfaces.C.int;  -- /usr/include/pulse/volume.h:301
   pragma Import
     (C,
      pa_cvolume_compatible_with_channel_map,
      "pa_cvolume_compatible_with_channel_map");

   function pa_cvolume_get_balance
     (v : System.Address;
      map : access constant Pulse.Channelmap.pa_channel_map)
      return Interfaces.C.C_float;  -- /usr/include/pulse/volume.h:308
   pragma Import (C, pa_cvolume_get_balance, "pa_cvolume_get_balance");

   function pa_cvolume_set_balance
     (v : access pa_cvolume;
      map : access constant Pulse.Channelmap.pa_channel_map;
      new_balance : Interfaces.C.C_float)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:319
   pragma Import (C, pa_cvolume_set_balance, "pa_cvolume_set_balance");

   function pa_cvolume_get_fade
     (v : System.Address;
      map : access constant Pulse.Channelmap.pa_channel_map)
      return Interfaces.C.C_float;  -- /usr/include/pulse/volume.h:326
   pragma Import (C, pa_cvolume_get_fade, "pa_cvolume_get_fade");

   function pa_cvolume_set_fade
     (v : access pa_cvolume;
      map : access constant Pulse.Channelmap.pa_channel_map;
      new_fade : Interfaces.C.C_float)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:337
   pragma Import (C, pa_cvolume_set_fade, "pa_cvolume_set_fade");

   function pa_cvolume_scale
     (v : access pa_cvolume;
      max : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:342
   pragma Import (C, pa_cvolume_scale, "pa_cvolume_scale");

   function pa_cvolume_scale_mask
     (v : access pa_cvolume;
      max : pa_volume_t;
      cm : access Pulse.Channelmap.pa_channel_map;
      mask : Pulse.Channelmap.pa_channel_position_mask_t)
      return access pa_cvolume;  -- /usr/include/pulse/volume.h:348
   pragma Import (C, pa_cvolume_scale_mask, "pa_cvolume_scale_mask");

   function pa_cvolume_set_position
     (cv : access pa_cvolume;
      map : access constant Pulse.Channelmap.pa_channel_map;
      t : Pulse.Channelmap.pa_channel_position_t;
      v : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:355
   pragma Import (C, pa_cvolume_set_position, "pa_cvolume_set_position");

   function pa_cvolume_get_position
     (cv : access pa_cvolume;
      map : access constant Pulse.Channelmap.pa_channel_map;
      t : Pulse.Channelmap.pa_channel_position_t)
     return pa_volume_t;  -- /usr/include/pulse/volume.h:361
   pragma Import (C, pa_cvolume_get_position, "pa_cvolume_get_position");

   function pa_cvolume_merge
     (dest : access pa_cvolume;
      a : System.Address;
      b : System.Address)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:366
   pragma Import (C, pa_cvolume_merge, "pa_cvolume_merge");

   function pa_cvolume_inc_clamp
     (v : access pa_cvolume;
      inc : pa_volume_t;
      limit : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:370
   pragma Import (C, pa_cvolume_inc_clamp, "pa_cvolume_inc_clamp");

   function pa_cvolume_inc
     (v : access pa_cvolume;
      inc : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:374
   pragma Import (C, pa_cvolume_inc, "pa_cvolume_inc");

   function pa_cvolume_dec
     (v : access pa_cvolume;
      dec : pa_volume_t)
     return access pa_cvolume;  -- /usr/include/pulse/volume.h:378
   pragma Import (C, pa_cvolume_dec, "pa_cvolume_dec");

end Pulse.Volume;
