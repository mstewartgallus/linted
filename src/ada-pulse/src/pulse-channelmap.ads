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

limited with Pulse.Sample;

package Pulse.Channelmap with SPARK_Mode => Off is

   --  unsupported macro: PA_CHANNEL_POSITION_INVALID PA_CHANNEL_POSITION_INVALID
   --  unsupported macro: PA_CHANNEL_POSITION_MONO PA_CHANNEL_POSITION_MONO
   --  unsupported macro: PA_CHANNEL_POSITION_LEFT PA_CHANNEL_POSITION_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_RIGHT PA_CHANNEL_POSITION_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_CENTER PA_CHANNEL_POSITION_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_LEFT PA_CHANNEL_POSITION_FRONT_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_RIGHT PA_CHANNEL_POSITION_FRONT_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_CENTER PA_CHANNEL_POSITION_FRONT_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_REAR_CENTER PA_CHANNEL_POSITION_REAR_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_REAR_LEFT PA_CHANNEL_POSITION_REAR_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_REAR_RIGHT PA_CHANNEL_POSITION_REAR_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_LFE PA_CHANNEL_POSITION_LFE
   --  unsupported macro: PA_CHANNEL_POSITION_SUBWOOFER PA_CHANNEL_POSITION_SUBWOOFER
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_SIDE_LEFT PA_CHANNEL_POSITION_SIDE_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_SIDE_RIGHT PA_CHANNEL_POSITION_SIDE_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_AUX0 PA_CHANNEL_POSITION_AUX0
   --  unsupported macro: PA_CHANNEL_POSITION_AUX1 PA_CHANNEL_POSITION_AUX1
   --  unsupported macro: PA_CHANNEL_POSITION_AUX2 PA_CHANNEL_POSITION_AUX2
   --  unsupported macro: PA_CHANNEL_POSITION_AUX3 PA_CHANNEL_POSITION_AUX3
   --  unsupported macro: PA_CHANNEL_POSITION_AUX4 PA_CHANNEL_POSITION_AUX4
   --  unsupported macro: PA_CHANNEL_POSITION_AUX5 PA_CHANNEL_POSITION_AUX5
   --  unsupported macro: PA_CHANNEL_POSITION_AUX6 PA_CHANNEL_POSITION_AUX6
   --  unsupported macro: PA_CHANNEL_POSITION_AUX7 PA_CHANNEL_POSITION_AUX7
   --  unsupported macro: PA_CHANNEL_POSITION_AUX8 PA_CHANNEL_POSITION_AUX8
   --  unsupported macro: PA_CHANNEL_POSITION_AUX9 PA_CHANNEL_POSITION_AUX9
   --  unsupported macro: PA_CHANNEL_POSITION_AUX10 PA_CHANNEL_POSITION_AUX10
   --  unsupported macro: PA_CHANNEL_POSITION_AUX11 PA_CHANNEL_POSITION_AUX11
   --  unsupported macro: PA_CHANNEL_POSITION_AUX12 PA_CHANNEL_POSITION_AUX12
   --  unsupported macro: PA_CHANNEL_POSITION_AUX13 PA_CHANNEL_POSITION_AUX13
   --  unsupported macro: PA_CHANNEL_POSITION_AUX14 PA_CHANNEL_POSITION_AUX14
   --  unsupported macro: PA_CHANNEL_POSITION_AUX15 PA_CHANNEL_POSITION_AUX15
   --  unsupported macro: PA_CHANNEL_POSITION_AUX16 PA_CHANNEL_POSITION_AUX16
   --  unsupported macro: PA_CHANNEL_POSITION_AUX17 PA_CHANNEL_POSITION_AUX17
   --  unsupported macro: PA_CHANNEL_POSITION_AUX18 PA_CHANNEL_POSITION_AUX18
   --  unsupported macro: PA_CHANNEL_POSITION_AUX19 PA_CHANNEL_POSITION_AUX19
   --  unsupported macro: PA_CHANNEL_POSITION_AUX20 PA_CHANNEL_POSITION_AUX20
   --  unsupported macro: PA_CHANNEL_POSITION_AUX21 PA_CHANNEL_POSITION_AUX21
   --  unsupported macro: PA_CHANNEL_POSITION_AUX22 PA_CHANNEL_POSITION_AUX22
   --  unsupported macro: PA_CHANNEL_POSITION_AUX23 PA_CHANNEL_POSITION_AUX23
   --  unsupported macro: PA_CHANNEL_POSITION_AUX24 PA_CHANNEL_POSITION_AUX24
   --  unsupported macro: PA_CHANNEL_POSITION_AUX25 PA_CHANNEL_POSITION_AUX25
   --  unsupported macro: PA_CHANNEL_POSITION_AUX26 PA_CHANNEL_POSITION_AUX26
   --  unsupported macro: PA_CHANNEL_POSITION_AUX27 PA_CHANNEL_POSITION_AUX27
   --  unsupported macro: PA_CHANNEL_POSITION_AUX28 PA_CHANNEL_POSITION_AUX28
   --  unsupported macro: PA_CHANNEL_POSITION_AUX29 PA_CHANNEL_POSITION_AUX29
   --  unsupported macro: PA_CHANNEL_POSITION_AUX30 PA_CHANNEL_POSITION_AUX30
   --  unsupported macro: PA_CHANNEL_POSITION_AUX31 PA_CHANNEL_POSITION_AUX31
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_CENTER PA_CHANNEL_POSITION_TOP_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_FRONT_LEFT PA_CHANNEL_POSITION_TOP_FRONT_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_FRONT_RIGHT PA_CHANNEL_POSITION_TOP_FRONT_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_FRONT_CENTER PA_CHANNEL_POSITION_TOP_FRONT_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_REAR_LEFT PA_CHANNEL_POSITION_TOP_REAR_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_REAR_RIGHT PA_CHANNEL_POSITION_TOP_REAR_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_REAR_CENTER PA_CHANNEL_POSITION_TOP_REAR_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_MAX PA_CHANNEL_POSITION_MAX
   --  arg-macro: function PA_CHANNEL_POSITION_MASK (f)
   --    return (pa_channel_position_mask_t) (1ULL << (f));
   --  unsupported macro: PA_CHANNEL_MAP_AIFF PA_CHANNEL_MAP_AIFF
   --  unsupported macro: PA_CHANNEL_MAP_ALSA PA_CHANNEL_MAP_ALSA
   --  unsupported macro: PA_CHANNEL_MAP_AUX PA_CHANNEL_MAP_AUX
   --  unsupported macro: PA_CHANNEL_MAP_WAVEEX PA_CHANNEL_MAP_WAVEEX
   --  unsupported macro: PA_CHANNEL_MAP_OSS PA_CHANNEL_MAP_OSS
   --  unsupported macro: PA_CHANNEL_MAP_DEF_MAX PA_CHANNEL_MAP_DEF_MAX
   --  unsupported macro: PA_CHANNEL_MAP_DEFAULT PA_CHANNEL_MAP_DEFAULT
   --  unsupported macro: PA_CHANNEL_MAP_SNPRINT_MAX 336
   subtype pa_channel_position is unsigned;
   PA_CHANNEL_POSITION_INVALID : constant pa_channel_position := -1;
   PA_CHANNEL_POSITION_MONO : constant pa_channel_position := 0;
   PA_CHANNEL_POSITION_FRONT_LEFT : constant pa_channel_position := 1;
   PA_CHANNEL_POSITION_FRONT_RIGHT : constant pa_channel_position := 2;
   PA_CHANNEL_POSITION_FRONT_CENTER : constant pa_channel_position := 3;
   PA_CHANNEL_POSITION_LEFT : constant pa_channel_position := 1;
   PA_CHANNEL_POSITION_RIGHT : constant pa_channel_position := 2;
   PA_CHANNEL_POSITION_CENTER : constant pa_channel_position := 3;
   PA_CHANNEL_POSITION_REAR_CENTER : constant pa_channel_position := 4;
   PA_CHANNEL_POSITION_REAR_LEFT : constant pa_channel_position := 5;
   PA_CHANNEL_POSITION_REAR_RIGHT : constant pa_channel_position := 6;
   PA_CHANNEL_POSITION_LFE : constant pa_channel_position := 7;
   PA_CHANNEL_POSITION_SUBWOOFER : constant pa_channel_position := 7;
   PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER : constant pa_channel_position := 8;
   PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER : constant pa_channel_position := 9;
   PA_CHANNEL_POSITION_SIDE_LEFT : constant pa_channel_position := 10;
   PA_CHANNEL_POSITION_SIDE_RIGHT : constant pa_channel_position := 11;
   PA_CHANNEL_POSITION_AUX0 : constant pa_channel_position := 12;
   PA_CHANNEL_POSITION_AUX1 : constant pa_channel_position := 13;
   PA_CHANNEL_POSITION_AUX2 : constant pa_channel_position := 14;
   PA_CHANNEL_POSITION_AUX3 : constant pa_channel_position := 15;
   PA_CHANNEL_POSITION_AUX4 : constant pa_channel_position := 16;
   PA_CHANNEL_POSITION_AUX5 : constant pa_channel_position := 17;
   PA_CHANNEL_POSITION_AUX6 : constant pa_channel_position := 18;
   PA_CHANNEL_POSITION_AUX7 : constant pa_channel_position := 19;
   PA_CHANNEL_POSITION_AUX8 : constant pa_channel_position := 20;
   PA_CHANNEL_POSITION_AUX9 : constant pa_channel_position := 21;
   PA_CHANNEL_POSITION_AUX10 : constant pa_channel_position := 22;
   PA_CHANNEL_POSITION_AUX11 : constant pa_channel_position := 23;
   PA_CHANNEL_POSITION_AUX12 : constant pa_channel_position := 24;
   PA_CHANNEL_POSITION_AUX13 : constant pa_channel_position := 25;
   PA_CHANNEL_POSITION_AUX14 : constant pa_channel_position := 26;
   PA_CHANNEL_POSITION_AUX15 : constant pa_channel_position := 27;
   PA_CHANNEL_POSITION_AUX16 : constant pa_channel_position := 28;
   PA_CHANNEL_POSITION_AUX17 : constant pa_channel_position := 29;
   PA_CHANNEL_POSITION_AUX18 : constant pa_channel_position := 30;
   PA_CHANNEL_POSITION_AUX19 : constant pa_channel_position := 31;
   PA_CHANNEL_POSITION_AUX20 : constant pa_channel_position := 32;
   PA_CHANNEL_POSITION_AUX21 : constant pa_channel_position := 33;
   PA_CHANNEL_POSITION_AUX22 : constant pa_channel_position := 34;
   PA_CHANNEL_POSITION_AUX23 : constant pa_channel_position := 35;
   PA_CHANNEL_POSITION_AUX24 : constant pa_channel_position := 36;
   PA_CHANNEL_POSITION_AUX25 : constant pa_channel_position := 37;
   PA_CHANNEL_POSITION_AUX26 : constant pa_channel_position := 38;
   PA_CHANNEL_POSITION_AUX27 : constant pa_channel_position := 39;
   PA_CHANNEL_POSITION_AUX28 : constant pa_channel_position := 40;
   PA_CHANNEL_POSITION_AUX29 : constant pa_channel_position := 41;
   PA_CHANNEL_POSITION_AUX30 : constant pa_channel_position := 42;
   PA_CHANNEL_POSITION_AUX31 : constant pa_channel_position := 43;
   PA_CHANNEL_POSITION_TOP_CENTER : constant pa_channel_position := 44;
   PA_CHANNEL_POSITION_TOP_FRONT_LEFT : constant pa_channel_position := 45;
   PA_CHANNEL_POSITION_TOP_FRONT_RIGHT : constant pa_channel_position := 46;
   PA_CHANNEL_POSITION_TOP_FRONT_CENTER : constant pa_channel_position := 47;
   PA_CHANNEL_POSITION_TOP_REAR_LEFT : constant pa_channel_position := 48;
   PA_CHANNEL_POSITION_TOP_REAR_RIGHT : constant pa_channel_position := 49;
   PA_CHANNEL_POSITION_TOP_REAR_CENTER : constant pa_channel_position := 50;
   PA_CHANNEL_POSITION_MAX : constant pa_channel_position := 51;  -- /usr/include/pulse/channelmap.h:76

   subtype pa_channel_position_t is pa_channel_position;

   subtype pa_channel_position_mask_t is Libc.Stdint.uint64_t;  -- /usr/include/pulse/channelmap.h:212

   subtype pa_channel_map_def is unsigned;
   PA_CHANNEL_MAP_AIFF : constant pa_channel_map_def := 0;
   PA_CHANNEL_MAP_ALSA : constant pa_channel_map_def := 1;
   PA_CHANNEL_MAP_AUX : constant pa_channel_map_def := 2;
   PA_CHANNEL_MAP_WAVEEX : constant pa_channel_map_def := 3;
   PA_CHANNEL_MAP_OSS : constant pa_channel_map_def := 4;
   PA_CHANNEL_MAP_DEF_MAX : constant pa_channel_map_def := 5;
   PA_CHANNEL_MAP_DEFAULT : constant pa_channel_map_def := 0;  -- /usr/include/pulse/channelmap.h:218

   subtype pa_channel_map_def_t is pa_channel_map_def;

   type pa_channel_map_map_array is array (0 .. 31) of aliased pa_channel_position_t;
   type pa_channel_map is record
      channels : aliased Libc.Stdint.uint8_t;  -- /usr/include/pulse/channelmap.h:265
      map : aliased pa_channel_map_map_array;  -- /usr/include/pulse/channelmap.h:268
   end record;
   pragma Convention (C_Pass_By_Copy, pa_channel_map);  -- /usr/include/pulse/channelmap.h:264

   function pa_channel_map_init (m : access pa_channel_map) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:275
   pragma Import (C, pa_channel_map_init, "pa_channel_map_init");

   function pa_channel_map_init_mono (m : access pa_channel_map) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:278
   pragma Import (C, pa_channel_map_init_mono, "pa_channel_map_init_mono");

   function pa_channel_map_init_stereo (m : access pa_channel_map) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:281
   pragma Import (C, pa_channel_map_init_stereo, "pa_channel_map_init_stereo");

   function pa_channel_map_init_auto
     (m : access pa_channel_map;
      channels : unsigned;
      def : pa_channel_map_def_t) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:287
   pragma Import (C, pa_channel_map_init_auto, "pa_channel_map_init_auto");

   function pa_channel_map_init_extend
     (m : access pa_channel_map;
      channels : unsigned;
      def : pa_channel_map_def_t) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:293
   pragma Import (C, pa_channel_map_init_extend, "pa_channel_map_init_extend");

   function pa_channel_position_to_string (pos : pa_channel_position_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:296
   pragma Import (C, pa_channel_position_to_string, "pa_channel_position_to_string");

   function pa_channel_position_from_string (s : Interfaces.C.Strings.chars_ptr) return pa_channel_position_t;  -- /usr/include/pulse/channelmap.h:299
   pragma Import (C, pa_channel_position_from_string, "pa_channel_position_from_string");

   function pa_channel_position_to_pretty_string (pos : pa_channel_position_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:302
   pragma Import (C, pa_channel_position_to_pretty_string, "pa_channel_position_to_pretty_string");

   function pa_channel_map_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      map : System.Address) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:312
   pragma Import (C, pa_channel_map_snprint, "pa_channel_map_snprint");

   function pa_channel_map_parse (map : access pa_channel_map; s : Interfaces.C.Strings.chars_ptr) return access pa_channel_map;  -- /usr/include/pulse/channelmap.h:318
   pragma Import (C, pa_channel_map_parse, "pa_channel_map_parse");

   function pa_channel_map_equal (a : System.Address; b : System.Address) return int;  -- /usr/include/pulse/channelmap.h:321
   pragma Import (C, pa_channel_map_equal, "pa_channel_map_equal");

   function pa_channel_map_valid (map : System.Address) return int;  -- /usr/include/pulse/channelmap.h:324
   pragma Import (C, pa_channel_map_valid, "pa_channel_map_valid");

   function pa_channel_map_compatible (map : System.Address; ss : access constant Pulse.Sample.pa_sample_spec) return int;  -- /usr/include/pulse/channelmap.h:328
   pragma Import (C, pa_channel_map_compatible, "pa_channel_map_compatible");

   function pa_channel_map_superset (a : System.Address; b : System.Address) return int;  -- /usr/include/pulse/channelmap.h:331
   pragma Import (C, pa_channel_map_superset, "pa_channel_map_superset");

   function pa_channel_map_can_balance (map : System.Address) return int;  -- /usr/include/pulse/channelmap.h:336
   pragma Import (C, pa_channel_map_can_balance, "pa_channel_map_can_balance");

   function pa_channel_map_can_fade (map : System.Address) return int;  -- /usr/include/pulse/channelmap.h:341
   pragma Import (C, pa_channel_map_can_fade, "pa_channel_map_can_fade");

   function pa_channel_map_to_name (map : System.Address) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:347
   pragma Import (C, pa_channel_map_to_name, "pa_channel_map_to_name");

   function pa_channel_map_to_pretty_name (map : System.Address) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/channelmap.h:352
   pragma Import (C, pa_channel_map_to_pretty_name, "pa_channel_map_to_pretty_name");

   function pa_channel_map_has_position (map : System.Address; p : pa_channel_position_t) return int;  -- /usr/include/pulse/channelmap.h:356
   pragma Import (C, pa_channel_map_has_position, "pa_channel_map_has_position");

   function pa_channel_map_mask (map : System.Address) return pa_channel_position_mask_t;  -- /usr/include/pulse/channelmap.h:359
   pragma Import (C, pa_channel_map_mask, "pa_channel_map_mask");

end Pulse.Channelmap;
