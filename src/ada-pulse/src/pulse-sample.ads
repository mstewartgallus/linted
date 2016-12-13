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

package Pulse.Sample with SPARK_Mode => Off is

   --  unsupported macro: PA_CHANNELS_MAX 32U
   --  unsupported macro: PA_RATE_MAX (48000U*4U)
   --  unsupported macro: PA_SAMPLE_S16NE PA_SAMPLE_S16LE
   --  unsupported macro: PA_SAMPLE_FLOAT32NE PA_SAMPLE_FLOAT32LE
   --  unsupported macro: PA_SAMPLE_S32NE PA_SAMPLE_S32LE
   --  unsupported macro: PA_SAMPLE_S24NE PA_SAMPLE_S24LE
   --  unsupported macro: PA_SAMPLE_S24_32NE PA_SAMPLE_S24_32LE
   --  unsupported macro: PA_SAMPLE_S16RE PA_SAMPLE_S16BE
   --  unsupported macro: PA_SAMPLE_FLOAT32RE PA_SAMPLE_FLOAT32BE
   --  unsupported macro: PA_SAMPLE_S32RE PA_SAMPLE_S32BE
   --  unsupported macro: PA_SAMPLE_S24RE PA_SAMPLE_S24BE
   --  unsupported macro: PA_SAMPLE_S24_32RE PA_SAMPLE_S24_32BE
   --  unsupported macro: PA_SAMPLE_FLOAT32 PA_SAMPLE_FLOAT32NE
   --  unsupported macro: PA_SAMPLE_U8 PA_SAMPLE_U8
   --  unsupported macro: PA_SAMPLE_ALAW PA_SAMPLE_ALAW
   --  unsupported macro: PA_SAMPLE_ULAW PA_SAMPLE_ULAW
   --  unsupported macro: PA_SAMPLE_S16LE PA_SAMPLE_S16LE
   --  unsupported macro: PA_SAMPLE_S16BE PA_SAMPLE_S16BE
   --  unsupported macro: PA_SAMPLE_FLOAT32LE PA_SAMPLE_FLOAT32LE
   --  unsupported macro: PA_SAMPLE_FLOAT32BE PA_SAMPLE_FLOAT32BE
   --  unsupported macro: PA_SAMPLE_S32LE PA_SAMPLE_S32LE
   --  unsupported macro: PA_SAMPLE_S32BE PA_SAMPLE_S32BE
   --  unsupported macro: PA_SAMPLE_S24LE PA_SAMPLE_S24LE
   --  unsupported macro: PA_SAMPLE_S24BE PA_SAMPLE_S24BE
   --  unsupported macro: PA_SAMPLE_S24_32LE PA_SAMPLE_S24_32LE
   --  unsupported macro: PA_SAMPLE_S24_32BE PA_SAMPLE_S24_32BE
   --  unsupported macro: PA_SAMPLE_SPEC_SNPRINT_MAX 32
   --  unsupported macro: PA_BYTES_SNPRINT_MAX 11
   --  arg-macro: procedure pa_sample_format_is_ne (f)
   --    pa_sample_format_is_le(f)
   --  arg-macro: procedure pa_sample_format_is_re (f)
   --    pa_sample_format_is_be(f)
   subtype pa_sample_format is unsigned;
   PA_SAMPLE_U8 : constant pa_sample_format := 0;
   PA_SAMPLE_ALAW : constant pa_sample_format := 1;
   PA_SAMPLE_ULAW : constant pa_sample_format := 2;
   PA_SAMPLE_S16LE : constant pa_sample_format := 3;
   PA_SAMPLE_S16BE : constant pa_sample_format := 4;
   PA_SAMPLE_FLOAT32LE : constant pa_sample_format := 5;
   PA_SAMPLE_FLOAT32BE : constant pa_sample_format := 6;
   PA_SAMPLE_S32LE : constant pa_sample_format := 7;
   PA_SAMPLE_S32BE : constant pa_sample_format := 8;
   PA_SAMPLE_S24LE : constant pa_sample_format := 9;
   PA_SAMPLE_S24BE : constant pa_sample_format := 10;
   PA_SAMPLE_S24_32LE : constant pa_sample_format := 11;
   PA_SAMPLE_S24_32BE : constant pa_sample_format := 12;
   PA_SAMPLE_MAX : constant pa_sample_format := 13;
   PA_SAMPLE_INVALID : constant pa_sample_format := -1;  -- /usr/include/pulse/sample.h:136

   subtype pa_sample_format_t is pa_sample_format;

   type pa_sample_spec is record
      format : aliased pa_sample_format_t;  -- /usr/include/pulse/sample.h:251
      rate : aliased Libc.Stdint.uint32_t;  -- /usr/include/pulse/sample.h:254
      channels : aliased Libc.Stdint.uint8_t;  -- /usr/include/pulse/sample.h:257
   end record;
   pragma Convention (C_Pass_By_Copy, pa_sample_spec);  -- /usr/include/pulse/sample.h:250

   subtype pa_usec_t is Libc.Stdint.uint64_t;  -- /usr/include/pulse/sample.h:262

   function pa_bytes_per_second (spec : access constant pa_sample_spec) return Libc.Stddef.size_t;  -- /usr/include/pulse/sample.h:265
   pragma Import (C, pa_bytes_per_second, "pa_bytes_per_second");

   function pa_frame_size (spec : access constant pa_sample_spec) return Libc.Stddef.size_t;  -- /usr/include/pulse/sample.h:268
   pragma Import (C, pa_frame_size, "pa_frame_size");

   function pa_sample_size (spec : access constant pa_sample_spec) return Libc.Stddef.size_t;  -- /usr/include/pulse/sample.h:271
   pragma Import (C, pa_sample_size, "pa_sample_size");

   function pa_sample_size_of_format (f : pa_sample_format_t) return Libc.Stddef.size_t;  -- /usr/include/pulse/sample.h:275
   pragma Import (C, pa_sample_size_of_format, "pa_sample_size_of_format");

   function pa_bytes_to_usec (length : Libc.Stdint.uint64_t; spec : access constant pa_sample_spec) return pa_usec_t;  -- /usr/include/pulse/sample.h:280
   pragma Import (C, pa_bytes_to_usec, "pa_bytes_to_usec");

   function pa_usec_to_bytes (t : pa_usec_t; spec : access constant pa_sample_spec) return Libc.Stddef.size_t;  -- /usr/include/pulse/sample.h:285
   pragma Import (C, pa_usec_to_bytes, "pa_usec_to_bytes");

   function pa_sample_spec_init (spec : access pa_sample_spec) return access pa_sample_spec;  -- /usr/include/pulse/sample.h:290
   pragma Import (C, pa_sample_spec_init, "pa_sample_spec_init");

   function pa_sample_spec_valid (spec : access constant pa_sample_spec) return int;  -- /usr/include/pulse/sample.h:293
   pragma Import (C, pa_sample_spec_valid, "pa_sample_spec_valid");

   function pa_sample_spec_equal (a : System.Address; b : System.Address) return int;  -- /usr/include/pulse/sample.h:296
   pragma Import (C, pa_sample_spec_equal, "pa_sample_spec_equal");

   function pa_sample_format_to_string (f : pa_sample_format_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/sample.h:299
   pragma Import (C, pa_sample_format_to_string, "pa_sample_format_to_string");

   function pa_parse_sample_format (format : Interfaces.C.Strings.chars_ptr) return pa_sample_format_t;  -- /usr/include/pulse/sample.h:302
   pragma Import (C, pa_parse_sample_format, "pa_parse_sample_format");

   function pa_sample_spec_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      spec : access constant pa_sample_spec) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/sample.h:312
   pragma Import (C, pa_sample_spec_snprint, "pa_sample_spec_snprint");

   function pa_bytes_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : Libc.Stddef.size_t;
      v : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/sample.h:322
   pragma Import (C, pa_bytes_snprint, "pa_bytes_snprint");

   function pa_sample_format_is_le (f : pa_sample_format_t) return int;  -- /usr/include/pulse/sample.h:326
   pragma Import (C, pa_sample_format_is_le, "pa_sample_format_is_le");

   function pa_sample_format_is_be (f : pa_sample_format_t) return int;  -- /usr/include/pulse/sample.h:330
   pragma Import (C, pa_sample_format_is_be, "pa_sample_format_is_be");

end Pulse.Sample;
