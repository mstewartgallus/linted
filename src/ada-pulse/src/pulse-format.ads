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

with Pulse.Sample;
limited with Pulse.Channelmap;

package Pulse.Format with
     Spark_Mode => Off is

   --  unsupported macro: PA_ENCODING_ANY PA_ENCODING_ANY
   --  unsupported macro: PA_ENCODING_PCM PA_ENCODING_PCM
   --  unsupported macro: PA_ENCODING_AC3_IEC61937 PA_ENCODING_AC3_IEC61937
   --  unsupported macro: PA_ENCODING_EAC3_IEC61937 PA_ENCODING_EAC3_IEC61937
   --  unsupported macro: PA_ENCODING_MPEG_IEC61937 PA_ENCODING_MPEG_IEC61937
   --  unsupported macro: PA_ENCODING_DTS_IEC61937 PA_ENCODING_DTS_IEC61937
   --  unsupported macro: PA_ENCODING_MPEG2_AAC_IEC61937 PA_ENCODING_MPEG2_AAC_IEC61937
   --  unsupported macro: PA_ENCODING_MAX PA_ENCODING_MAX
   --  unsupported macro: PA_ENCODING_INVALID PA_ENCODING_INVALID
   --  unsupported macro: PA_FORMAT_INFO_SNPRINT_MAX 256
   --  unsupported macro: PA_PROP_TYPE_INT PA_PROP_TYPE_INT
   --  unsupported macro: PA_PROP_TYPE_INT_RANGE PA_PROP_TYPE_INT_RANGE
   --  unsupported macro: PA_PROP_TYPE_INT_ARRAY PA_PROP_TYPE_INT_ARRAY
   --  unsupported macro: PA_PROP_TYPE_STRING PA_PROP_TYPE_STRING
   --  unsupported macro: PA_PROP_TYPE_STRING_ARRAY PA_PROP_TYPE_STRING_ARRAY
   --  unsupported macro: PA_PROP_TYPE_INVALID PA_PROP_TYPE_INVALID
   subtype pa_encoding is unsigned;
   PA_ENCODING_ANY : constant pa_encoding := 0;
   PA_ENCODING_PCM : constant pa_encoding := 1;
   PA_ENCODING_AC3_IEC61937 : constant pa_encoding := 2;
   PA_ENCODING_EAC3_IEC61937 : constant pa_encoding := 3;
   PA_ENCODING_MPEG_IEC61937 : constant pa_encoding := 4;
   PA_ENCODING_DTS_IEC61937 : constant pa_encoding := 5;
   PA_ENCODING_MPEG2_AAC_IEC61937 : constant pa_encoding := 6;
   PA_ENCODING_MAX : constant pa_encoding := 7;
   PA_ENCODING_INVALID : constant pa_encoding :=
     -1;  -- /usr/include/pulse/format.h:39

   subtype pa_encoding_t is pa_encoding;

   function pa_encoding_to_string
     (e : pa_encoding_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/format.h:81
   pragma Import (C, pa_encoding_to_string, "pa_encoding_to_string");

   function pa_encoding_from_string
     (encoding : Interfaces.C.Strings.chars_ptr)
     return pa_encoding_t;  -- /usr/include/pulse/format.h:84
   pragma Import (C, pa_encoding_from_string, "pa_encoding_from_string");

   type pa_format_info is record
      encoding : aliased pa_encoding_t;  -- /usr/include/pulse/format.h:88
      plist : System.Address;  -- /usr/include/pulse/format.h:91
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      pa_format_info);  -- /usr/include/pulse/format.h:87

   function pa_format_info_new
     return access pa_format_info;  -- /usr/include/pulse/format.h:96
   pragma Import (C, pa_format_info_new, "pa_format_info_new");

   function pa_format_info_copy
     (src : System.Address)
     return access pa_format_info;  -- /usr/include/pulse/format.h:99
   pragma Import (C, pa_format_info_copy, "pa_format_info_copy");

   procedure pa_format_info_free
     (f : access pa_format_info);  -- /usr/include/pulse/format.h:102
   pragma Import (C, pa_format_info_free, "pa_format_info_free");

   function pa_format_info_valid
     (f : System.Address) return int;  -- /usr/include/pulse/format.h:105
   pragma Import (C, pa_format_info_valid, "pa_format_info_valid");

   function pa_format_info_is_pcm
     (f : System.Address) return int;  -- /usr/include/pulse/format.h:108
   pragma Import (C, pa_format_info_is_pcm, "pa_format_info_is_pcm");

   function pa_format_info_is_compatible
     (first : access pa_format_info;
      second : access pa_format_info)
     return int;  -- /usr/include/pulse/format.h:117
   pragma Import
     (C,
      pa_format_info_is_compatible,
      "pa_format_info_is_compatible");

   function pa_format_info_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      f : System.Address)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/format.h:127
   pragma Import (C, pa_format_info_snprint, "pa_format_info_snprint");

   function pa_format_info_from_string
     (str : Interfaces.C.Strings.chars_ptr)
     return access pa_format_info;  -- /usr/include/pulse/format.h:131
   pragma Import (C, pa_format_info_from_string, "pa_format_info_from_string");

   function pa_format_info_from_sample_spec
     (ss : Pulse.Sample.pa_sample_spec;
      map : access Pulse.Channelmap.pa_channel_map)
      return access pa_format_info;  -- /usr/include/pulse/format.h:134
   pragma Import
     (C,
      pa_format_info_from_sample_spec,
      "pa_format_info_from_sample_spec");

   function pa_format_info_to_sample_spec
     (f : access pa_format_info;
      ss : out Pulse.Sample.pa_sample_spec;
      map : access Pulse.Channelmap.pa_channel_map)
     return int;  -- /usr/include/pulse/format.h:141
   pragma Import
     (C,
      pa_format_info_to_sample_spec,
      "pa_format_info_to_sample_spec");

   subtype pa_prop_type_t is unsigned;
   PA_PROP_TYPE_INT : constant pa_prop_type_t := 0;
   PA_PROP_TYPE_INT_RANGE : constant pa_prop_type_t := 1;
   PA_PROP_TYPE_INT_ARRAY : constant pa_prop_type_t := 2;
   PA_PROP_TYPE_STRING : constant pa_prop_type_t := 3;
   PA_PROP_TYPE_STRING_ARRAY : constant pa_prop_type_t := 4;
   PA_PROP_TYPE_INVALID : constant pa_prop_type_t :=
     -1;  -- /usr/include/pulse/format.h:144

   function pa_format_info_get_prop_type
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr)
     return pa_prop_type_t;  -- /usr/include/pulse/format.h:174
   pragma Import
     (C,
      pa_format_info_get_prop_type,
      "pa_format_info_get_prop_type");

   function pa_format_info_get_prop_int
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      v : access int) return int;  -- /usr/include/pulse/format.h:177
   pragma Import
     (C,
      pa_format_info_get_prop_int,
      "pa_format_info_get_prop_int");

   function pa_format_info_get_prop_int_range
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      min : access int;
      max : access int) return int;  -- /usr/include/pulse/format.h:180
   pragma Import
     (C,
      pa_format_info_get_prop_int_range,
      "pa_format_info_get_prop_int_range");

   function pa_format_info_get_prop_int_array
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : System.Address;
      n_values : access int) return int;  -- /usr/include/pulse/format.h:184
   pragma Import
     (C,
      pa_format_info_get_prop_int_array,
      "pa_format_info_get_prop_int_array");

   function pa_format_info_get_prop_string
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      v : System.Address) return int;  -- /usr/include/pulse/format.h:187
   pragma Import
     (C,
      pa_format_info_get_prop_string,
      "pa_format_info_get_prop_string");

   function pa_format_info_get_prop_string_array
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : System.Address;
      n_values : access int) return int;  -- /usr/include/pulse/format.h:191
   pragma Import
     (C,
      pa_format_info_get_prop_string_array,
      "pa_format_info_get_prop_string_array");

   procedure pa_format_info_free_string_array
     (values : System.Address;
      n_values : int);  -- /usr/include/pulse/format.h:194
   pragma Import
     (C,
      pa_format_info_free_string_array,
      "pa_format_info_free_string_array");

   procedure pa_format_info_set_prop_int
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      value : int);  -- /usr/include/pulse/format.h:197
   pragma Import
     (C,
      pa_format_info_set_prop_int,
      "pa_format_info_set_prop_int");

   procedure pa_format_info_set_prop_int_array
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : access int;
      n_values : int);  -- /usr/include/pulse/format.h:199
   pragma Import
     (C,
      pa_format_info_set_prop_int_array,
      "pa_format_info_set_prop_int_array");

   procedure pa_format_info_set_prop_int_range
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      min : int;
      max : int);  -- /usr/include/pulse/format.h:201
   pragma Import
     (C,
      pa_format_info_set_prop_int_range,
      "pa_format_info_set_prop_int_range");

   procedure pa_format_info_set_prop_string
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings
        .chars_ptr);  -- /usr/include/pulse/format.h:203
   pragma Import
     (C,
      pa_format_info_set_prop_string,
      "pa_format_info_set_prop_string");

   procedure pa_format_info_set_prop_string_array
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : System.Address;
      n_values : int);  -- /usr/include/pulse/format.h:205
   pragma Import
     (C,
      pa_format_info_set_prop_string_array,
      "pa_format_info_set_prop_string_array");

   procedure pa_format_info_set_sample_format
     (f : access pa_format_info;
      sf : Pulse.Sample
        .pa_sample_format_t);  -- /usr/include/pulse/format.h:208
   pragma Import
     (C,
      pa_format_info_set_sample_format,
      "pa_format_info_set_sample_format");

   procedure pa_format_info_set_rate
     (f : access pa_format_info;
      rate : int);  -- /usr/include/pulse/format.h:210
   pragma Import (C, pa_format_info_set_rate, "pa_format_info_set_rate");

   procedure pa_format_info_set_channels
     (f : access pa_format_info;
      channels : int);  -- /usr/include/pulse/format.h:212
   pragma Import
     (C,
      pa_format_info_set_channels,
      "pa_format_info_set_channels");

   procedure pa_format_info_set_channel_map
     (f : access pa_format_info;
      map : access constant Pulse.Channelmap
        .pa_channel_map);  -- /usr/include/pulse/format.h:214
   pragma Import
     (C,
      pa_format_info_set_channel_map,
      "pa_format_info_set_channel_map");

end Pulse.Format;
