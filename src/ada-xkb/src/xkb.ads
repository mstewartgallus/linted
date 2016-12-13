-- Copyright 2015 Steven Stewart-Gallus
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
with Interfaces.C; use Interfaces.C;
with Libc.Stdint;
with Interfaces.C.Strings;
with Libc.Stddef;
with System;
limited with Libc.Stdio;

package XKB with SPARK_Mode => Off is
   pragma Preelaborate;
   pragma Link_With ("-lxkbcommon");

   XKB_KEYCODE_INVALID : constant := 16#ffffffff#;
   XKB_LAYOUT_INVALID : constant := 16#ffffffff#;
   XKB_LEVEL_INVALID : constant := 16#ffffffff#;
   XKB_MOD_INVALID : constant := 16#ffffffff#;
   XKB_LED_INVALID : constant := 16#ffffffff#;
   XKB_KEYCODE_MAX : constant := 16#ffffffff# - 1;

   --  arg-macro: function xkb_keycode_is_legal_ext (key)
   --    return key <= XKB_KEYCODE_MAX;
   --  arg-macro: function xkb_keycode_is_legal_x11 (key)
   --    return key >= 8  and then  key <= 255;

   type xkb_context is limited private;
   type xkb_context_access is access all xkb_context;

   type xkb_keymap is limited private;
   type xkb_keymap_access is access all xkb_keymap;

   type xkb_state is limited private;
   type xkb_state_access is access all xkb_state;

   subtype xkb_keycode_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:161

   subtype xkb_keysym_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:191

   subtype xkb_layout_index_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:211

   subtype xkb_layout_mask_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:213

   subtype xkb_level_index_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:227

   subtype xkb_mod_index_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:252

   subtype xkb_mod_mask_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:254

   subtype xkb_led_index_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:281

   subtype xkb_led_mask_t is Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:283

   type xkb_rule_names is record
      rules : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:323
      model : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:331
      layout : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:340
      variant : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:351
      options : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:362
   end record;
   pragma Convention (C_Pass_By_Copy, xkb_rule_names);  -- /usr/include/xkbcommon/xkbcommon.h:314

   function xkb_keysym_get_name
     (keysym : xkb_keysym_t;
      buffer : Interfaces.C.Strings.chars_ptr;
      size : Libc.Stddef.size_t) return int;  -- /usr/include/xkbcommon/xkbcommon.h:393
   pragma Import (C, xkb_keysym_get_name, "xkb_keysym_get_name");

   type xkb_keysym_flags is
     (XKB_KEYSYM_NO_FLAGS,
      XKB_KEYSYM_CASE_INSENSITIVE);
   pragma Convention (C, xkb_keysym_flags);  -- /usr/include/xkbcommon/xkbcommon.h:396

   function xkb_keysym_from_name (name : Interfaces.C.Strings.chars_ptr; flags : xkb_keysym_flags) return xkb_keysym_t;  -- /usr/include/xkbcommon/xkbcommon.h:424
   pragma Import (C, xkb_keysym_from_name, "xkb_keysym_from_name");

   function xkb_keysym_to_utf8
     (keysym : xkb_keysym_t;
      buffer : Interfaces.C.Strings.chars_ptr;
      size : Libc.Stddef.size_t) return int;  -- /usr/include/xkbcommon/xkbcommon.h:443
   pragma Import (C, xkb_keysym_to_utf8, "xkb_keysym_to_utf8");

   function xkb_keysym_to_utf32 (keysym : xkb_keysym_t) return Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:458
   pragma Import (C, xkb_keysym_to_utf32, "xkb_keysym_to_utf32");

   type xkb_context_flags is
     (XKB_CONTEXT_NO_FLAGS,
      XKB_CONTEXT_NO_DEFAULT_INCLUDES,
      XKB_CONTEXT_NO_ENVIRONMENT_NAMES);
   pragma Convention (C, xkb_context_flags);  -- /usr/include/xkbcommon/xkbcommon.h:473

   function xkb_context_new (flags : xkb_context_flags) return xkb_context_access;  -- /usr/include/xkbcommon/xkbcommon.h:496
   pragma Import (C, xkb_context_new, "xkb_context_new");

   function xkb_context_ref (context : xkb_context) return xkb_context_access;  -- /usr/include/xkbcommon/xkbcommon.h:506
   pragma Import (C, xkb_context_ref, "xkb_context_ref");

   procedure xkb_context_unref (context : xkb_context);  -- /usr/include/xkbcommon/xkbcommon.h:516
   pragma Import (C, xkb_context_unref, "xkb_context_unref");

   procedure xkb_context_set_user_data (context : xkb_context_access; user_data : System.Address);  -- /usr/include/xkbcommon/xkbcommon.h:527
   pragma Import (C, xkb_context_set_user_data, "xkb_context_set_user_data");

   function xkb_context_get_user_data (context : xkb_context) return System.Address;  -- /usr/include/xkbcommon/xkbcommon.h:541
   pragma Import (C, xkb_context_get_user_data, "xkb_context_get_user_data");

   function xkb_context_include_path_append (context : xkb_context_access; path : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/xkbcommon/xkbcommon.h:565
   pragma Import (C, xkb_context_include_path_append, "xkb_context_include_path_append");

   function xkb_context_include_path_append_default (context : xkb_context) return int;  -- /usr/include/xkbcommon/xkbcommon.h:575
   pragma Import (C, xkb_context_include_path_append_default, "xkb_context_include_path_append_default");

   function xkb_context_include_path_reset_defaults (context : xkb_context) return int;  -- /usr/include/xkbcommon/xkbcommon.h:588
   pragma Import (C, xkb_context_include_path_reset_defaults, "xkb_context_include_path_reset_defaults");

   procedure xkb_context_include_path_clear (context : xkb_context);  -- /usr/include/xkbcommon/xkbcommon.h:596
   pragma Import (C, xkb_context_include_path_clear, "xkb_context_include_path_clear");

   function xkb_context_num_include_paths (context : xkb_context) return unsigned;  -- /usr/include/xkbcommon/xkbcommon.h:604
   pragma Import (C, xkb_context_num_include_paths, "xkb_context_num_include_paths");

   function xkb_context_include_path_get (context : xkb_context_access; index : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:615
   pragma Import (C, xkb_context_include_path_get, "xkb_context_include_path_get");

   subtype xkb_log_level is unsigned;
   XKB_LOG_LEVEL_CRITICAL : constant xkb_log_level := 10;
   XKB_LOG_LEVEL_ERROR : constant xkb_log_level := 20;
   XKB_LOG_LEVEL_WARNING : constant xkb_log_level := 30;
   XKB_LOG_LEVEL_INFO : constant xkb_log_level := 40;
   XKB_LOG_LEVEL_DEBUG : constant xkb_log_level := 50;  -- /usr/include/xkbcommon/xkbcommon.h:627

   procedure xkb_context_set_log_level (context : xkb_context_access; level : xkb_log_level);  -- /usr/include/xkbcommon/xkbcommon.h:649
   pragma Import (C, xkb_context_set_log_level, "xkb_context_set_log_level");

   function xkb_context_get_log_level (context : xkb_context) return xkb_log_level;  -- /usr/include/xkbcommon/xkbcommon.h:658
   pragma Import (C, xkb_context_get_log_level, "xkb_context_get_log_level");

   procedure xkb_context_set_log_verbosity (context : xkb_context_access; verbosity : int);  -- /usr/include/xkbcommon/xkbcommon.h:680
   pragma Import (C, xkb_context_set_log_verbosity, "xkb_context_set_log_verbosity");

   function xkb_context_get_log_verbosity (context : xkb_context) return int;  -- /usr/include/xkbcommon/xkbcommon.h:688
   pragma Import (C, xkb_context_get_log_verbosity, "xkb_context_get_log_verbosity");

   procedure xkb_context_set_log_fn (context : xkb_context_access; log_fn : access procedure
        (arg1 : System.Address;
         arg2 : xkb_log_level;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : access System.Address));  -- /usr/include/xkbcommon/xkbcommon.h:711
   pragma Import (C, xkb_context_set_log_fn, "xkb_context_set_log_fn");

   type xkb_keymap_compile_flags is
     (XKB_KEYMAP_COMPILE_NO_FLAGS);
   pragma Convention (C, xkb_keymap_compile_flags);  -- /usr/include/xkbcommon/xkbcommon.h:726

   function xkb_keymap_new_from_names
     (context : xkb_context_access;
      names : access constant xkb_rule_names;
      flags : xkb_keymap_compile_flags) return xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon.h:748
   pragma Import (C, xkb_keymap_new_from_names, "xkb_keymap_new_from_names");

   subtype xkb_keymap_format is unsigned;
   XKB_KEYMAP_FORMAT_TEXT_V1 : constant xkb_keymap_format := 1;  -- /usr/include/xkbcommon/xkbcommon.h:753
   XKB_KEYMAP_USE_ORIGINAL_FORMAT : constant xkb_keymap_format := xkb_keymap_format'Last;

   function xkb_keymap_new_from_file
     (context : xkb_context_access;
      the_file : access Libc.Stdio.FILE;
      format : xkb_keymap_format;
      flags : xkb_keymap_compile_flags) return xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon.h:777
   pragma Import (C, xkb_keymap_new_from_file, "xkb_keymap_new_from_file");

   function xkb_keymap_new_from_string
     (context : xkb_context_access;
      string : Interfaces.C.Strings.chars_ptr;
      format : xkb_keymap_format;
      flags : xkb_keymap_compile_flags) return xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon.h:791
   pragma Import (C, xkb_keymap_new_from_string, "xkb_keymap_new_from_string");

   function xkb_keymap_new_from_buffer
     (context : xkb_context_access;
      buffer : Interfaces.C.Strings.chars_ptr;
      length : Libc.Stddef.size_t;
      format : xkb_keymap_format;
      flags : xkb_keymap_compile_flags) return xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon.h:805
   pragma Import (C, xkb_keymap_new_from_buffer, "xkb_keymap_new_from_buffer");

   function xkb_keymap_ref (keymap : xkb_keymap) return xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon.h:817
   pragma Import (C, xkb_keymap_ref, "xkb_keymap_ref");

   procedure xkb_keymap_unref (keymap : xkb_keymap);  -- /usr/include/xkbcommon/xkbcommon.h:827
   pragma Import (C, xkb_keymap_unref, "xkb_keymap_unref");

   function xkb_keymap_get_as_string (keymap : xkb_keymap_access; format : xkb_keymap_format) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:854
   pragma Import (C, xkb_keymap_get_as_string, "xkb_keymap_get_as_string");

   function xkb_keymap_min_keycode (keymap : xkb_keymap) return xkb_keycode_t;  -- /usr/include/xkbcommon/xkbcommon.h:873
   pragma Import (C, xkb_keymap_min_keycode, "xkb_keymap_min_keycode");

   function xkb_keymap_max_keycode (keymap : xkb_keymap) return xkb_keycode_t;  -- /usr/include/xkbcommon/xkbcommon.h:882
   pragma Import (C, xkb_keymap_max_keycode, "xkb_keymap_max_keycode");

   type xkb_keymap_key_iter_t is access procedure
        (arg1 : System.Address;
         arg2 : xkb_keycode_t;
         arg3 : System.Address);
   pragma Convention (C, xkb_keymap_key_iter_t);  -- /usr/include/xkbcommon/xkbcommon.h:891

   procedure xkb_keymap_key_for_each
     (keymap : xkb_keymap_access;
      iter : xkb_keymap_key_iter_t;
      data : System.Address);  -- /usr/include/xkbcommon/xkbcommon.h:903
   pragma Import (C, xkb_keymap_key_for_each, "xkb_keymap_key_for_each");

   function xkb_keymap_num_mods (keymap : xkb_keymap) return xkb_mod_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:913
   pragma Import (C, xkb_keymap_num_mods, "xkb_keymap_num_mods");

   function xkb_keymap_mod_get_name (keymap : xkb_keymap_access; idx : xkb_mod_index_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:924
   pragma Import (C, xkb_keymap_mod_get_name, "xkb_keymap_mod_get_name");

   function xkb_keymap_mod_get_index (keymap : xkb_keymap_access; name : Interfaces.C.Strings.chars_ptr) return xkb_mod_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:936
   pragma Import (C, xkb_keymap_mod_get_index, "xkb_keymap_mod_get_index");

   function xkb_keymap_num_layouts (keymap : xkb_keymap) return xkb_layout_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:945
   pragma Import (C, xkb_keymap_num_layouts, "xkb_keymap_num_layouts");

   function xkb_keymap_layout_get_name (keymap : xkb_keymap_access; idx : xkb_layout_index_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:957
   pragma Import (C, xkb_keymap_layout_get_name, "xkb_keymap_layout_get_name");

   function xkb_keymap_layout_get_index (keymap : xkb_keymap_access; name : Interfaces.C.Strings.chars_ptr) return xkb_layout_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:969
   pragma Import (C, xkb_keymap_layout_get_index, "xkb_keymap_layout_get_index");

   function xkb_keymap_num_layouts_for_key (keymap : xkb_keymap_access; key : xkb_keycode_t) return xkb_layout_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:982
   pragma Import (C, xkb_keymap_num_layouts_for_key, "xkb_keymap_num_layouts_for_key");

   function xkb_keymap_num_levels_for_key
     (keymap : xkb_keymap_access;
      key : xkb_keycode_t;
      layout : xkb_layout_index_t) return xkb_level_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:995
   pragma Import (C, xkb_keymap_num_levels_for_key, "xkb_keymap_num_levels_for_key");

   function xkb_keymap_key_get_syms_by_level
     (keymap : xkb_keymap_access;
      key : xkb_keycode_t;
      layout : xkb_layout_index_t;
      level : xkb_level_index_t;
      syms_out : System.Address) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1027
   pragma Import (C, xkb_keymap_key_get_syms_by_level, "xkb_keymap_key_get_syms_by_level");

   function xkb_keymap_num_leds (keymap : xkb_keymap) return xkb_led_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:1045
   pragma Import (C, xkb_keymap_num_leds, "xkb_keymap_num_leds");

   function xkb_keymap_led_get_name (keymap : xkb_keymap_access; idx : xkb_led_index_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xkbcommon/xkbcommon.h:1055
   pragma Import (C, xkb_keymap_led_get_name, "xkb_keymap_led_get_name");

   function xkb_keymap_led_get_index (keymap : xkb_keymap_access; name : Interfaces.C.Strings.chars_ptr) return xkb_led_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:1066
   pragma Import (C, xkb_keymap_led_get_index, "xkb_keymap_led_get_index");

   function xkb_keymap_key_repeats (keymap : xkb_keymap_access; key : xkb_keycode_t) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1084
   pragma Import (C, xkb_keymap_key_repeats, "xkb_keymap_key_repeats");

   function xkb_state_new (keymap : xkb_keymap) return xkb_state_access;  -- /usr/include/xkbcommon/xkbcommon.h:1105
   pragma Import (C, xkb_state_new, "xkb_state_new");

   function xkb_state_ref (state : xkb_state) return xkb_state_access;  -- /usr/include/xkbcommon/xkbcommon.h:1115
   pragma Import (C, xkb_state_ref, "xkb_state_ref");

   procedure xkb_state_unref (state : xkb_state);  -- /usr/include/xkbcommon/xkbcommon.h:1125
   pragma Import (C, xkb_state_unref, "xkb_state_unref");

   function xkb_state_get_keymap (state : xkb_state) return xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon.h:1140
   pragma Import (C, xkb_state_get_keymap, "xkb_state_get_keymap");

   type xkb_key_direction is
     (XKB_KEY_UP,
      XKB_KEY_DOWN);
   pragma Convention (C, xkb_key_direction);  -- /usr/include/xkbcommon/xkbcommon.h:1143

   subtype xkb_state_component is unsigned;
   XKB_STATE_MODS_DEPRESSED : constant xkb_state_component := 1;
   XKB_STATE_MODS_LATCHED : constant xkb_state_component := 2;
   XKB_STATE_MODS_LOCKED : constant xkb_state_component := 4;
   XKB_STATE_MODS_EFFECTIVE : constant xkb_state_component := 8;
   XKB_STATE_LAYOUT_DEPRESSED : constant xkb_state_component := 16;
   XKB_STATE_LAYOUT_LATCHED : constant xkb_state_component := 32;
   XKB_STATE_LAYOUT_LOCKED : constant xkb_state_component := 64;
   XKB_STATE_LAYOUT_EFFECTIVE : constant xkb_state_component := 128;
   XKB_STATE_LEDS : constant xkb_state_component := 256;  -- /usr/include/xkbcommon/xkbcommon.h:1155

   function xkb_state_update_key
     (state : xkb_state_access;
      key : xkb_keycode_t;
      direction : xkb_key_direction) return xkb_state_component;  -- /usr/include/xkbcommon/xkbcommon.h:1214
   pragma Import (C, xkb_state_update_key, "xkb_state_update_key");

   function xkb_state_update_mask
     (state : xkb_state_access;
      depressed_mods : xkb_mod_mask_t;
      latched_mods : xkb_mod_mask_t;
      locked_mods : xkb_mod_mask_t;
      depressed_layout : xkb_layout_index_t;
      latched_layout : xkb_layout_index_t;
      locked_layout : xkb_layout_index_t) return xkb_state_component;  -- /usr/include/xkbcommon/xkbcommon.h:1245
   pragma Import (C, xkb_state_update_mask, "xkb_state_update_mask");

   function xkb_state_key_get_syms
     (state : xkb_state_access;
      key : xkb_keycode_t;
      syms_out : System.Address) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1278
   pragma Import (C, xkb_state_key_get_syms, "xkb_state_key_get_syms");

   function xkb_state_key_get_utf8
     (state : xkb_state_access;
      key : xkb_keycode_t;
      buffer : Interfaces.C.Strings.chars_ptr;
      size : Libc.Stddef.size_t) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1304
   pragma Import (C, xkb_state_key_get_utf8, "xkb_state_key_get_utf8");

   function xkb_state_key_get_utf32 (state : xkb_state_access; key : xkb_keycode_t) return Libc.Stdint.uint32_t;  -- /usr/include/xkbcommon/xkbcommon.h:1317
   pragma Import (C, xkb_state_key_get_utf32, "xkb_state_key_get_utf32");

   function xkb_state_key_get_one_sym (state : xkb_state_access; key : xkb_keycode_t) return xkb_keysym_t;  -- /usr/include/xkbcommon/xkbcommon.h:1335
   pragma Import (C, xkb_state_key_get_one_sym, "xkb_state_key_get_one_sym");

   function xkb_state_key_get_layout (state : xkb_state_access; key : xkb_keycode_t) return xkb_layout_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:1352
   pragma Import (C, xkb_state_key_get_layout, "xkb_state_key_get_layout");

   function xkb_state_key_get_level
     (state : xkb_state_access;
      key : xkb_keycode_t;
      layout : xkb_layout_index_t) return xkb_level_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:1377
   pragma Import (C, xkb_state_key_get_level, "xkb_state_key_get_level");

   subtype xkb_state_match is unsigned;
   XKB_STATE_MATCH_ANY : constant xkb_state_match := 1;
   XKB_STATE_MATCH_ALL : constant xkb_state_match := 2;
   XKB_STATE_MATCH_NON_EXCLUSIVE : constant xkb_state_match := 65536;  -- /usr/include/xkbcommon/xkbcommon.h:1386

   function xkb_state_serialize_mods (state : xkb_state_access; components : xkb_state_component) return xkb_mod_mask_t;  -- /usr/include/xkbcommon/xkbcommon.h:1415
   pragma Import (C, xkb_state_serialize_mods, "xkb_state_serialize_mods");

   function xkb_state_serialize_layout (state : xkb_state_access; components : xkb_state_component) return xkb_layout_index_t;  -- /usr/include/xkbcommon/xkbcommon.h:1437
   pragma Import (C, xkb_state_serialize_layout, "xkb_state_serialize_layout");

   function xkb_state_mod_name_is_active
     (state : xkb_state_access;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : xkb_state_component) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1449
   pragma Import (C, xkb_state_mod_name_is_active, "xkb_state_mod_name_is_active");

   function xkb_state_mod_names_are_active
     (state : xkb_state_access;
      c_type : xkb_state_component;
      match : xkb_state_match  -- , ...
      ) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1470
   pragma Import (C, xkb_state_mod_names_are_active, "xkb_state_mod_names_are_active");

   function xkb_state_mod_index_is_active
     (state : xkb_state_access;
      idx : xkb_mod_index_t;
      c_type : xkb_state_component) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1484
   pragma Import (C, xkb_state_mod_index_is_active, "xkb_state_mod_index_is_active");

   function xkb_state_mod_indices_are_active
     (state : xkb_state_access;
      c_type : xkb_state_component;
      match : xkb_state_match  -- , ...
      ) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1505
   pragma Import (C, xkb_state_mod_indices_are_active, "xkb_state_mod_indices_are_active");

   function xkb_state_mod_index_is_consumed
     (state : xkb_state_access;
      key : xkb_keycode_t;
      idx : xkb_mod_index_t) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1574
   pragma Import (C, xkb_state_mod_index_is_consumed, "xkb_state_mod_index_is_consumed");

   function xkb_state_mod_mask_remove_consumed
     (state : xkb_state_access;
      key : xkb_keycode_t;
      mask : xkb_mod_mask_t) return xkb_mod_mask_t;  -- /usr/include/xkbcommon/xkbcommon.h:1587
   pragma Import (C, xkb_state_mod_mask_remove_consumed, "xkb_state_mod_mask_remove_consumed");

   function xkb_state_key_get_consumed_mods (state : xkb_state_access; key : xkb_keycode_t) return xkb_mod_mask_t;  -- /usr/include/xkbcommon/xkbcommon.h:1599
   pragma Import (C, xkb_state_key_get_consumed_mods, "xkb_state_key_get_consumed_mods");

   function xkb_state_layout_name_is_active
     (state : xkb_state_access;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : xkb_state_component) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1614
   pragma Import (C, xkb_state_layout_name_is_active, "xkb_state_layout_name_is_active");

   function xkb_state_layout_index_is_active
     (state : xkb_state_access;
      idx : xkb_layout_index_t;
      c_type : xkb_state_component) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1627
   pragma Import (C, xkb_state_layout_index_is_active, "xkb_state_layout_index_is_active");

   function xkb_state_led_name_is_active (state : xkb_state_access; name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1641
   pragma Import (C, xkb_state_led_name_is_active, "xkb_state_led_name_is_active");

   function xkb_state_led_index_is_active (state : xkb_state_access; idx : xkb_led_index_t) return int;  -- /usr/include/xkbcommon/xkbcommon.h:1653
   pragma Import (C, xkb_state_led_index_is_active, "xkb_state_led_index_is_active");
private
   type xkb_context is limited record null; end record;
   type xkb_keymap is limited record null; end record;
   type xkb_state is limited record null; end record;
end XKB;
