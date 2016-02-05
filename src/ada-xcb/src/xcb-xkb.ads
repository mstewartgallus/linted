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
with XCB.XProto;
with System;
with Interfaces.C.Strings;

package XCB.XKB is
   pragma Preelaborate;
   pragma Link_With ("-lxcb -lxcb-xkb");

   XCB_XKB_MAJOR_VERSION : constant := 1;
   XCB_XKB_MINOR_VERSION : constant := 0;

   XCB_XKB_KEYBOARD : constant := 0;

   --  XCB_XKB_USE_EXTENSION : constant := 0;
   --  XCB_XKB_SELECT_EVENTS : constant := 1;
   --  XCB_XKB_BELL : constant := 3;
   --  XCB_XKB_GET_STATE : constant := 4;
   --  XCB_XKB_LATCH_LOCK_STATE : constant := 5;
   --  XCB_XKB_GET_CONTROLS : constant := 6;
   --  XCB_XKB_SET_CONTROLS : constant := 7;
   --  XCB_XKB_GET_MAP : constant := 8;
   --  XCB_XKB_SET_MAP : constant := 9;
   --  XCB_XKB_GET_COMPAT_MAP : constant := 10;
   --  XCB_XKB_SET_COMPAT_MAP : constant := 11;
   --  XCB_XKB_GET_INDICATOR_STATE : constant := 12;
   --  XCB_XKB_GET_INDICATOR_MAP : constant := 13;
   --  XCB_XKB_SET_INDICATOR_MAP : constant := 14;
   --  XCB_XKB_GET_NAMED_INDICATOR : constant := 15;
   --  XCB_XKB_SET_NAMED_INDICATOR : constant := 16;
   --  XCB_XKB_GET_NAMES : constant := 17;
   --  XCB_XKB_SET_NAMES : constant := 18;
   --  XCB_XKB_PER_CLIENT_FLAGS : constant := 21;
   --  XCB_XKB_LIST_COMPONENTS : constant := 22;
   --  XCB_XKB_GET_KBD_BY_NAME : constant := 23;
   --  XCB_XKB_GET_DEVICE_INFO : constant := 24;
   --  XCB_XKB_SET_DEVICE_INFO : constant := 25;
   --  XCB_XKB_SET_DEBUGGING_FLAGS : constant := 101;

   XCB_XKB_NEW_KEYBOARD_NOTIFY : constant := 0;
   XCB_XKB_MAP_NOTIFY : constant := 1;
   XCB_XKB_STATE_NOTIFY : constant := 2;
   XCB_XKB_CONTROLS_NOTIFY : constant := 3;
   XCB_XKB_INDICATOR_STATE_NOTIFY : constant := 4;
   XCB_XKB_INDICATOR_MAP_NOTIFY : constant := 5;
   XCB_XKB_NAMES_NOTIFY : constant := 6;
   XCB_XKB_COMPAT_MAP_NOTIFY : constant := 7;
   XCB_XKB_BELL_NOTIFY : constant := 8;
   XCB_XKB_ACTION_MESSAGE : constant := 9;
   XCB_XKB_ACCESS_X_NOTIFY : constant := 10;
   XCB_XKB_EXTENSION_DEVICE_NOTIFY : constant := 11;

   xcb_xkb_id : aliased XCB.xcb_extension_t_access;  -- /usr/include/xcb/xkb.h:25
   pragma Import (C, xcb_xkb_id, "xcb_xkb_id");

   subtype xcb_xkb_const_t is unsigned;
   XCB_XKB_CONST_MAX_LEGAL_KEY_CODE : constant xcb_xkb_const_t := 255;
   XCB_XKB_CONST_PER_KEY_BIT_ARRAY_SIZE : constant xcb_xkb_const_t := 32;
   XCB_XKB_CONST_KEY_NAME_LENGTH : constant xcb_xkb_const_t := 4;  -- /usr/include/xcb/xkb.h:27

   subtype xcb_xkb_event_type_t is unsigned;
   XCB_XKB_EVENT_TYPE_NEW_KEYBOARD_NOTIFY : constant xcb_xkb_event_type_t := 1;
   XCB_XKB_EVENT_TYPE_MAP_NOTIFY : constant xcb_xkb_event_type_t := 2;
   XCB_XKB_EVENT_TYPE_STATE_NOTIFY : constant xcb_xkb_event_type_t := 4;
   XCB_XKB_EVENT_TYPE_CONTROLS_NOTIFY : constant xcb_xkb_event_type_t := 8;
   XCB_XKB_EVENT_TYPE_INDICATOR_STATE_NOTIFY : constant xcb_xkb_event_type_t := 16;
   XCB_XKB_EVENT_TYPE_INDICATOR_MAP_NOTIFY : constant xcb_xkb_event_type_t := 32;
   XCB_XKB_EVENT_TYPE_NAMES_NOTIFY : constant xcb_xkb_event_type_t := 64;
   XCB_XKB_EVENT_TYPE_COMPAT_MAP_NOTIFY : constant xcb_xkb_event_type_t := 128;
   XCB_XKB_EVENT_TYPE_BELL_NOTIFY : constant xcb_xkb_event_type_t := 256;
   XCB_XKB_EVENT_TYPE_ACTION_MESSAGE : constant xcb_xkb_event_type_t := 512;
   XCB_XKB_EVENT_TYPE_ACCESS_X_NOTIFY : constant xcb_xkb_event_type_t := 1024;
   XCB_XKB_EVENT_TYPE_EXTENSION_DEVICE_NOTIFY : constant xcb_xkb_event_type_t := 2048;  -- /usr/include/xcb/xkb.h:33

   subtype xcb_xkb_nkn_detail_t is unsigned;
   XCB_XKB_NKN_DETAIL_KEYCODES : constant xcb_xkb_nkn_detail_t := 1;
   XCB_XKB_NKN_DETAIL_GEOMETRY : constant xcb_xkb_nkn_detail_t := 2;
   XCB_XKB_NKN_DETAIL_DEVICE_ID : constant xcb_xkb_nkn_detail_t := 4;  -- /usr/include/xcb/xkb.h:48

   subtype xcb_xkb_axn_detail_t is unsigned;
   XCB_XKB_AXN_DETAIL_SK_PRESS : constant xcb_xkb_axn_detail_t := 1;
   XCB_XKB_AXN_DETAIL_SK_ACCEPT : constant xcb_xkb_axn_detail_t := 2;
   XCB_XKB_AXN_DETAIL_SK_REJECT : constant xcb_xkb_axn_detail_t := 4;
   XCB_XKB_AXN_DETAIL_SK_RELEASE : constant xcb_xkb_axn_detail_t := 8;
   XCB_XKB_AXN_DETAIL_BK_ACCEPT : constant xcb_xkb_axn_detail_t := 16;
   XCB_XKB_AXN_DETAIL_BK_REJECT : constant xcb_xkb_axn_detail_t := 32;
   XCB_XKB_AXN_DETAIL_AXK_WARNING : constant xcb_xkb_axn_detail_t := 64;  -- /usr/include/xcb/xkb.h:54

   subtype xcb_xkb_map_part_t is unsigned;
   XCB_XKB_MAP_PART_KEY_TYPES : constant xcb_xkb_map_part_t := 1;
   XCB_XKB_MAP_PART_KEY_SYMS : constant xcb_xkb_map_part_t := 2;
   XCB_XKB_MAP_PART_MODIFIER_MAP : constant xcb_xkb_map_part_t := 4;
   XCB_XKB_MAP_PART_EXPLICIT_COMPONENTS : constant xcb_xkb_map_part_t := 8;
   XCB_XKB_MAP_PART_KEY_ACTIONS : constant xcb_xkb_map_part_t := 16;
   XCB_XKB_MAP_PART_KEY_BEHAVIORS : constant xcb_xkb_map_part_t := 32;
   XCB_XKB_MAP_PART_VIRTUAL_MODS : constant xcb_xkb_map_part_t := 64;
   XCB_XKB_MAP_PART_VIRTUAL_MOD_MAP : constant xcb_xkb_map_part_t := 128;  -- /usr/include/xcb/xkb.h:64

   subtype xcb_xkb_set_map_flags_t is unsigned;
   XCB_XKB_SET_MAP_FLAGS_RESIZE_TYPES : constant xcb_xkb_set_map_flags_t := 1;
   XCB_XKB_SET_MAP_FLAGS_RECOMPUTE_ACTIONS : constant xcb_xkb_set_map_flags_t := 2;  -- /usr/include/xcb/xkb.h:75

   subtype xcb_xkb_state_part_t is unsigned;
   XCB_XKB_STATE_PART_MODIFIER_STATE : constant xcb_xkb_state_part_t := 1;
   XCB_XKB_STATE_PART_MODIFIER_BASE : constant xcb_xkb_state_part_t := 2;
   XCB_XKB_STATE_PART_MODIFIER_LATCH : constant xcb_xkb_state_part_t := 4;
   XCB_XKB_STATE_PART_MODIFIER_LOCK : constant xcb_xkb_state_part_t := 8;
   XCB_XKB_STATE_PART_GROUP_STATE : constant xcb_xkb_state_part_t := 16;
   XCB_XKB_STATE_PART_GROUP_BASE : constant xcb_xkb_state_part_t := 32;
   XCB_XKB_STATE_PART_GROUP_LATCH : constant xcb_xkb_state_part_t := 64;
   XCB_XKB_STATE_PART_GROUP_LOCK : constant xcb_xkb_state_part_t := 128;
   XCB_XKB_STATE_PART_COMPAT_STATE : constant xcb_xkb_state_part_t := 256;
   XCB_XKB_STATE_PART_GRAB_MODS : constant xcb_xkb_state_part_t := 512;
   XCB_XKB_STATE_PART_COMPAT_GRAB_MODS : constant xcb_xkb_state_part_t := 1024;
   XCB_XKB_STATE_PART_LOOKUP_MODS : constant xcb_xkb_state_part_t := 2048;
   XCB_XKB_STATE_PART_COMPAT_LOOKUP_MODS : constant xcb_xkb_state_part_t := 4096;
   XCB_XKB_STATE_PART_POINTER_BUTTONS : constant xcb_xkb_state_part_t := 8192;  -- /usr/include/xcb/xkb.h:80

   subtype xcb_xkb_bool_ctrl_t is unsigned;
   XCB_XKB_BOOL_CTRL_REPEAT_KEYS : constant xcb_xkb_bool_ctrl_t := 1;
   XCB_XKB_BOOL_CTRL_SLOW_KEYS : constant xcb_xkb_bool_ctrl_t := 2;
   XCB_XKB_BOOL_CTRL_BOUNCE_KEYS : constant xcb_xkb_bool_ctrl_t := 4;
   XCB_XKB_BOOL_CTRL_STICKY_KEYS : constant xcb_xkb_bool_ctrl_t := 8;
   XCB_XKB_BOOL_CTRL_MOUSE_KEYS : constant xcb_xkb_bool_ctrl_t := 16;
   XCB_XKB_BOOL_CTRL_MOUSE_KEYS_ACCEL : constant xcb_xkb_bool_ctrl_t := 32;
   XCB_XKB_BOOL_CTRL_ACCESS_X_KEYS : constant xcb_xkb_bool_ctrl_t := 64;
   XCB_XKB_BOOL_CTRL_ACCESS_X_TIMEOUT_MASK : constant xcb_xkb_bool_ctrl_t := 128;
   XCB_XKB_BOOL_CTRL_ACCESS_X_FEEDBACK_MASK : constant xcb_xkb_bool_ctrl_t := 256;
   XCB_XKB_BOOL_CTRL_AUDIBLE_BELL_MASK : constant xcb_xkb_bool_ctrl_t := 512;
   XCB_XKB_BOOL_CTRL_OVERLAY_1_MASK : constant xcb_xkb_bool_ctrl_t := 1024;
   XCB_XKB_BOOL_CTRL_OVERLAY_2_MASK : constant xcb_xkb_bool_ctrl_t := 2048;
   XCB_XKB_BOOL_CTRL_IGNORE_GROUP_LOCK_MASK : constant xcb_xkb_bool_ctrl_t := 4096;  -- /usr/include/xcb/xkb.h:97

   subtype xcb_xkb_control_t is unsigned;
   XCB_XKB_CONTROL_GROUPS_WRAP : constant xcb_xkb_control_t := 134217728;
   XCB_XKB_CONTROL_INTERNAL_MODS : constant xcb_xkb_control_t := 268435456;
   XCB_XKB_CONTROL_IGNORE_LOCK_MODS : constant xcb_xkb_control_t := 536870912;
   XCB_XKB_CONTROL_PER_KEY_REPEAT : constant xcb_xkb_control_t := 1073741824;
   XCB_XKB_CONTROL_CONTROLS_ENABLED : constant xcb_xkb_control_t := 2147483648;  -- /usr/include/xcb/xkb.h:113

   subtype xcb_xkb_ax_option_t is unsigned;
   XCB_XKB_AX_OPTION_SK_PRESS_FB : constant xcb_xkb_ax_option_t := 1;
   XCB_XKB_AX_OPTION_SK_ACCEPT_FB : constant xcb_xkb_ax_option_t := 2;
   XCB_XKB_AX_OPTION_FEATURE_FB : constant xcb_xkb_ax_option_t := 4;
   XCB_XKB_AX_OPTION_SLOW_WARN_FB : constant xcb_xkb_ax_option_t := 8;
   XCB_XKB_AX_OPTION_INDICATOR_FB : constant xcb_xkb_ax_option_t := 16;
   XCB_XKB_AX_OPTION_STICKY_KEYS_FB : constant xcb_xkb_ax_option_t := 32;
   XCB_XKB_AX_OPTION_TWO_KEYS : constant xcb_xkb_ax_option_t := 64;
   XCB_XKB_AX_OPTION_LATCH_TO_LOCK : constant xcb_xkb_ax_option_t := 128;
   XCB_XKB_AX_OPTION_SK_RELEASE_FB : constant xcb_xkb_ax_option_t := 256;
   XCB_XKB_AX_OPTION_SK_REJECT_FB : constant xcb_xkb_ax_option_t := 512;
   XCB_XKB_AX_OPTION_BK_REJECT_FB : constant xcb_xkb_ax_option_t := 1024;
   XCB_XKB_AX_OPTION_DUMB_BELL : constant xcb_xkb_ax_option_t := 2048;  -- /usr/include/xcb/xkb.h:121

   subtype xcb_xkb_device_spec_t is Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:136

   type xcb_xkb_device_spec_iterator_t is record
      data : access xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:142
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:143
      index : aliased int;  -- /usr/include/xcb/xkb.h:144
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_device_spec_iterator_t);  -- /usr/include/xcb/xkb.h:141

   subtype xcb_xkb_led_class_result_t is unsigned;
   XCB_XKB_LED_CLASS_RESULT_KBD_FEEDBACK_CLASS : constant xcb_xkb_led_class_result_t := 0;
   XCB_XKB_LED_CLASS_RESULT_LED_FEEDBACK_CLASS : constant xcb_xkb_led_class_result_t := 4;  -- /usr/include/xcb/xkb.h:147

   subtype xcb_xkb_led_class_t is unsigned;
   XCB_XKB_LED_CLASS_KBD_FEEDBACK_CLASS : constant xcb_xkb_led_class_t := 0;
   XCB_XKB_LED_CLASS_LED_FEEDBACK_CLASS : constant xcb_xkb_led_class_t := 4;
   XCB_XKB_LED_CLASS_DFLT_XI_CLASS : constant xcb_xkb_led_class_t := 768;
   XCB_XKB_LED_CLASS_ALL_XI_CLASSES : constant xcb_xkb_led_class_t := 1280;  -- /usr/include/xcb/xkb.h:152

   subtype xcb_xkb_led_class_spec_t is Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:159

   type xcb_xkb_led_class_spec_iterator_t is record
      data : access xcb_xkb_led_class_spec_t;  -- /usr/include/xcb/xkb.h:165
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:166
      index : aliased int;  -- /usr/include/xcb/xkb.h:167
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_led_class_spec_iterator_t);  -- /usr/include/xcb/xkb.h:164

   subtype xcb_xkb_bell_class_result_t is unsigned;
   XCB_XKB_BELL_CLASS_RESULT_KBD_FEEDBACK_CLASS : constant xcb_xkb_bell_class_result_t := 0;
   XCB_XKB_BELL_CLASS_RESULT_BELL_FEEDBACK_CLASS : constant xcb_xkb_bell_class_result_t := 5;  -- /usr/include/xcb/xkb.h:170

   subtype xcb_xkb_bell_class_t is unsigned;
   XCB_XKB_BELL_CLASS_KBD_FEEDBACK_CLASS : constant xcb_xkb_bell_class_t := 0;
   XCB_XKB_BELL_CLASS_BELL_FEEDBACK_CLASS : constant xcb_xkb_bell_class_t := 5;
   XCB_XKB_BELL_CLASS_DFLT_XI_CLASS : constant xcb_xkb_bell_class_t := 768;  -- /usr/include/xcb/xkb.h:175

   subtype xcb_xkb_bell_class_spec_t is Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:181

   type xcb_xkb_bell_class_spec_iterator_t is record
      data : access xcb_xkb_bell_class_spec_t;  -- /usr/include/xcb/xkb.h:187
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:188
      index : aliased int;  -- /usr/include/xcb/xkb.h:189
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_bell_class_spec_iterator_t);  -- /usr/include/xcb/xkb.h:186

   subtype xcb_xkb_id_t is unsigned;
   XCB_XKB_ID_USE_CORE_KBD : constant xcb_xkb_id_t := 256;
   XCB_XKB_ID_USE_CORE_PTR : constant xcb_xkb_id_t := 512;
   XCB_XKB_ID_DFLT_XI_CLASS : constant xcb_xkb_id_t := 768;
   XCB_XKB_ID_DFLT_XI_ID : constant xcb_xkb_id_t := 1024;
   XCB_XKB_ID_ALL_XI_CLASS : constant xcb_xkb_id_t := 1280;
   XCB_XKB_ID_ALL_XI_ID : constant xcb_xkb_id_t := 1536;
   XCB_XKB_ID_XI_NONE : constant xcb_xkb_id_t := 65280;  -- /usr/include/xcb/xkb.h:192

   subtype xcb_xkb_id_spec_t is Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:202

   type xcb_xkb_id_spec_iterator_t is record
      data : access xcb_xkb_id_spec_t;  -- /usr/include/xcb/xkb.h:208
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:209
      index : aliased int;  -- /usr/include/xcb/xkb.h:210
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_id_spec_iterator_t);  -- /usr/include/xcb/xkb.h:207

   type xcb_xkb_group_t is
     (XCB_XKB_GROUP_1,
      XCB_XKB_GROUP_2,
      XCB_XKB_GROUP_3,
      XCB_XKB_GROUP_4);
   pragma Convention (C, xcb_xkb_group_t);  -- /usr/include/xcb/xkb.h:213

   subtype xcb_xkb_groups_t is unsigned;
   XCB_XKB_GROUPS_ANY : constant xcb_xkb_groups_t := 254;
   XCB_XKB_GROUPS_ALL : constant xcb_xkb_groups_t := 255;  -- /usr/include/xcb/xkb.h:220

   subtype xcb_xkb_set_of_group_t is unsigned;
   XCB_XKB_SET_OF_GROUP_GROUP_1 : constant xcb_xkb_set_of_group_t := 1;
   XCB_XKB_SET_OF_GROUP_GROUP_2 : constant xcb_xkb_set_of_group_t := 2;
   XCB_XKB_SET_OF_GROUP_GROUP_3 : constant xcb_xkb_set_of_group_t := 4;
   XCB_XKB_SET_OF_GROUP_GROUP_4 : constant xcb_xkb_set_of_group_t := 8;  -- /usr/include/xcb/xkb.h:225

   subtype xcb_xkb_set_of_groups_t is unsigned;
   XCB_XKB_SET_OF_GROUPS_ANY : constant xcb_xkb_set_of_groups_t := 128;  -- /usr/include/xcb/xkb.h:232

   subtype xcb_xkb_groups_wrap_t is unsigned;
   XCB_XKB_GROUPS_WRAP_WRAP_INTO_RANGE : constant xcb_xkb_groups_wrap_t := 0;
   XCB_XKB_GROUPS_WRAP_CLAMP_INTO_RANGE : constant xcb_xkb_groups_wrap_t := 64;
   XCB_XKB_GROUPS_WRAP_REDIRECT_INTO_RANGE : constant xcb_xkb_groups_wrap_t := 128;  -- /usr/include/xcb/xkb.h:236

   subtype xcb_xkb_v_mods_high_t is unsigned;
   XCB_XKB_V_MODS_HIGH_15 : constant xcb_xkb_v_mods_high_t := 128;
   XCB_XKB_V_MODS_HIGH_14 : constant xcb_xkb_v_mods_high_t := 64;
   XCB_XKB_V_MODS_HIGH_13 : constant xcb_xkb_v_mods_high_t := 32;
   XCB_XKB_V_MODS_HIGH_12 : constant xcb_xkb_v_mods_high_t := 16;
   XCB_XKB_V_MODS_HIGH_11 : constant xcb_xkb_v_mods_high_t := 8;
   XCB_XKB_V_MODS_HIGH_10 : constant xcb_xkb_v_mods_high_t := 4;
   XCB_XKB_V_MODS_HIGH_9 : constant xcb_xkb_v_mods_high_t := 2;
   XCB_XKB_V_MODS_HIGH_8 : constant xcb_xkb_v_mods_high_t := 1;  -- /usr/include/xcb/xkb.h:242

   subtype xcb_xkb_v_mods_low_t is unsigned;
   XCB_XKB_V_MODS_LOW_7 : constant xcb_xkb_v_mods_low_t := 128;
   XCB_XKB_V_MODS_LOW_6 : constant xcb_xkb_v_mods_low_t := 64;
   XCB_XKB_V_MODS_LOW_5 : constant xcb_xkb_v_mods_low_t := 32;
   XCB_XKB_V_MODS_LOW_4 : constant xcb_xkb_v_mods_low_t := 16;
   XCB_XKB_V_MODS_LOW_3 : constant xcb_xkb_v_mods_low_t := 8;
   XCB_XKB_V_MODS_LOW_2 : constant xcb_xkb_v_mods_low_t := 4;
   XCB_XKB_V_MODS_LOW_1 : constant xcb_xkb_v_mods_low_t := 2;
   XCB_XKB_V_MODS_LOW_0 : constant xcb_xkb_v_mods_low_t := 1;  -- /usr/include/xcb/xkb.h:253

   subtype xcb_xkb_v_mod_t is unsigned;
   XCB_XKB_V_MOD_15 : constant xcb_xkb_v_mod_t := 32768;
   XCB_XKB_V_MOD_14 : constant xcb_xkb_v_mod_t := 16384;
   XCB_XKB_V_MOD_13 : constant xcb_xkb_v_mod_t := 8192;
   XCB_XKB_V_MOD_12 : constant xcb_xkb_v_mod_t := 4096;
   XCB_XKB_V_MOD_11 : constant xcb_xkb_v_mod_t := 2048;
   XCB_XKB_V_MOD_10 : constant xcb_xkb_v_mod_t := 1024;
   XCB_XKB_V_MOD_9 : constant xcb_xkb_v_mod_t := 512;
   XCB_XKB_V_MOD_8 : constant xcb_xkb_v_mod_t := 256;
   XCB_XKB_V_MOD_7 : constant xcb_xkb_v_mod_t := 128;
   XCB_XKB_V_MOD_6 : constant xcb_xkb_v_mod_t := 64;
   XCB_XKB_V_MOD_5 : constant xcb_xkb_v_mod_t := 32;
   XCB_XKB_V_MOD_4 : constant xcb_xkb_v_mod_t := 16;
   XCB_XKB_V_MOD_3 : constant xcb_xkb_v_mod_t := 8;
   XCB_XKB_V_MOD_2 : constant xcb_xkb_v_mod_t := 4;
   XCB_XKB_V_MOD_1 : constant xcb_xkb_v_mod_t := 2;
   XCB_XKB_V_MOD_0 : constant xcb_xkb_v_mod_t := 1;  -- /usr/include/xcb/xkb.h:264

   subtype xcb_xkb_explicit_t is unsigned;
   XCB_XKB_EXPLICIT_V_MOD_MAP : constant xcb_xkb_explicit_t := 128;
   XCB_XKB_EXPLICIT_BEHAVIOR : constant xcb_xkb_explicit_t := 64;
   XCB_XKB_EXPLICIT_AUTO_REPEAT : constant xcb_xkb_explicit_t := 32;
   XCB_XKB_EXPLICIT_INTERPRET : constant xcb_xkb_explicit_t := 16;
   XCB_XKB_EXPLICIT_KEY_TYPE_4 : constant xcb_xkb_explicit_t := 8;
   XCB_XKB_EXPLICIT_KEY_TYPE_3 : constant xcb_xkb_explicit_t := 4;
   XCB_XKB_EXPLICIT_KEY_TYPE_2 : constant xcb_xkb_explicit_t := 2;
   XCB_XKB_EXPLICIT_KEY_TYPE_1 : constant xcb_xkb_explicit_t := 1;  -- /usr/include/xcb/xkb.h:283

   type xcb_xkb_sym_interpret_match_t is
     (XCB_XKB_SYM_INTERPRET_MATCH_NONE_OF,
      XCB_XKB_SYM_INTERPRET_MATCH_ANY_OF_OR_NONE,
      XCB_XKB_SYM_INTERPRET_MATCH_ANY_OF,
      XCB_XKB_SYM_INTERPRET_MATCH_ALL_OF,
      XCB_XKB_SYM_INTERPRET_MATCH_EXACTLY);
   pragma Convention (C, xcb_xkb_sym_interpret_match_t);  -- /usr/include/xcb/xkb.h:294

   subtype xcb_xkb_sym_interp_match_t is unsigned;
   XCB_XKB_SYM_INTERP_MATCH_LEVEL_ONE_ONLY : constant xcb_xkb_sym_interp_match_t := 128;
   XCB_XKB_SYM_INTERP_MATCH_OP_MASK : constant xcb_xkb_sym_interp_match_t := 127;  -- /usr/include/xcb/xkb.h:302

   subtype xcb_xkb_im_flag_t is unsigned;
   XCB_XKB_IM_FLAG_NO_EXPLICIT : constant xcb_xkb_im_flag_t := 128;
   XCB_XKB_IM_FLAG_NO_AUTOMATIC : constant xcb_xkb_im_flag_t := 64;
   XCB_XKB_IM_FLAG_LED_DRIVES_KB : constant xcb_xkb_im_flag_t := 32;  -- /usr/include/xcb/xkb.h:307

   subtype xcb_xkb_im_mods_which_t is unsigned;
   XCB_XKB_IM_MODS_WHICH_USE_COMPAT : constant xcb_xkb_im_mods_which_t := 16;
   XCB_XKB_IM_MODS_WHICH_USE_EFFECTIVE : constant xcb_xkb_im_mods_which_t := 8;
   XCB_XKB_IM_MODS_WHICH_USE_LOCKED : constant xcb_xkb_im_mods_which_t := 4;
   XCB_XKB_IM_MODS_WHICH_USE_LATCHED : constant xcb_xkb_im_mods_which_t := 2;
   XCB_XKB_IM_MODS_WHICH_USE_BASE : constant xcb_xkb_im_mods_which_t := 1;  -- /usr/include/xcb/xkb.h:313

   subtype xcb_xkb_im_groups_which_t is unsigned;
   XCB_XKB_IM_GROUPS_WHICH_USE_COMPAT : constant xcb_xkb_im_groups_which_t := 16;
   XCB_XKB_IM_GROUPS_WHICH_USE_EFFECTIVE : constant xcb_xkb_im_groups_which_t := 8;
   XCB_XKB_IM_GROUPS_WHICH_USE_LOCKED : constant xcb_xkb_im_groups_which_t := 4;
   XCB_XKB_IM_GROUPS_WHICH_USE_LATCHED : constant xcb_xkb_im_groups_which_t := 2;
   XCB_XKB_IM_GROUPS_WHICH_USE_BASE : constant xcb_xkb_im_groups_which_t := 1;  -- /usr/include/xcb/xkb.h:321

   type xcb_xkb_indicator_map_t is record
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:333
      whichGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:334
      groups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:335
      whichMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:336
      mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:337
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:338
      vmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:339
      ctrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:340
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_indicator_map_t);  -- /usr/include/xcb/xkb.h:332

   type xcb_xkb_indicator_map_iterator_t is record
      data : access xcb_xkb_indicator_map_t;  -- /usr/include/xcb/xkb.h:347
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:348
      index : aliased int;  -- /usr/include/xcb/xkb.h:349
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_indicator_map_iterator_t);  -- /usr/include/xcb/xkb.h:346

   subtype xcb_xkb_cm_detail_t is unsigned;
   XCB_XKB_CM_DETAIL_SYM_INTERP : constant xcb_xkb_cm_detail_t := 1;
   XCB_XKB_CM_DETAIL_GROUP_COMPAT : constant xcb_xkb_cm_detail_t := 2;  -- /usr/include/xcb/xkb.h:352

   subtype xcb_xkb_name_detail_t is unsigned;
   XCB_XKB_NAME_DETAIL_KEYCODES : constant xcb_xkb_name_detail_t := 1;
   XCB_XKB_NAME_DETAIL_GEOMETRY : constant xcb_xkb_name_detail_t := 2;
   XCB_XKB_NAME_DETAIL_SYMBOLS : constant xcb_xkb_name_detail_t := 4;
   XCB_XKB_NAME_DETAIL_PHYS_SYMBOLS : constant xcb_xkb_name_detail_t := 8;
   XCB_XKB_NAME_DETAIL_TYPES : constant xcb_xkb_name_detail_t := 16;
   XCB_XKB_NAME_DETAIL_COMPAT : constant xcb_xkb_name_detail_t := 32;
   XCB_XKB_NAME_DETAIL_KEY_TYPE_NAMES : constant xcb_xkb_name_detail_t := 64;
   XCB_XKB_NAME_DETAIL_KT_LEVEL_NAMES : constant xcb_xkb_name_detail_t := 128;
   XCB_XKB_NAME_DETAIL_INDICATOR_NAMES : constant xcb_xkb_name_detail_t := 256;
   XCB_XKB_NAME_DETAIL_KEY_NAMES : constant xcb_xkb_name_detail_t := 512;
   XCB_XKB_NAME_DETAIL_KEY_ALIASES : constant xcb_xkb_name_detail_t := 1024;
   XCB_XKB_NAME_DETAIL_VIRTUAL_MOD_NAMES : constant xcb_xkb_name_detail_t := 2048;
   XCB_XKB_NAME_DETAIL_GROUP_NAMES : constant xcb_xkb_name_detail_t := 4096;
   XCB_XKB_NAME_DETAIL_RG_NAMES : constant xcb_xkb_name_detail_t := 8192;  -- /usr/include/xcb/xkb.h:357

   subtype xcb_xkb_gbn_detail_t is unsigned;
   XCB_XKB_GBN_DETAIL_TYPES : constant xcb_xkb_gbn_detail_t := 1;
   XCB_XKB_GBN_DETAIL_COMPAT_MAP : constant xcb_xkb_gbn_detail_t := 2;
   XCB_XKB_GBN_DETAIL_CLIENT_SYMBOLS : constant xcb_xkb_gbn_detail_t := 4;
   XCB_XKB_GBN_DETAIL_SERVER_SYMBOLS : constant xcb_xkb_gbn_detail_t := 8;
   XCB_XKB_GBN_DETAIL_INDICATOR_MAPS : constant xcb_xkb_gbn_detail_t := 16;
   XCB_XKB_GBN_DETAIL_KEY_NAMES : constant xcb_xkb_gbn_detail_t := 32;
   XCB_XKB_GBN_DETAIL_GEOMETRY : constant xcb_xkb_gbn_detail_t := 64;
   XCB_XKB_GBN_DETAIL_OTHER_NAMES : constant xcb_xkb_gbn_detail_t := 128;  -- /usr/include/xcb/xkb.h:374

   subtype xcb_xkb_xi_feature_t is unsigned;
   XCB_XKB_XI_FEATURE_KEYBOARDS : constant xcb_xkb_xi_feature_t := 1;
   XCB_XKB_XI_FEATURE_BUTTON_ACTIONS : constant xcb_xkb_xi_feature_t := 2;
   XCB_XKB_XI_FEATURE_INDICATOR_NAMES : constant xcb_xkb_xi_feature_t := 4;
   XCB_XKB_XI_FEATURE_INDICATOR_MAPS : constant xcb_xkb_xi_feature_t := 8;
   XCB_XKB_XI_FEATURE_INDICATOR_STATE : constant xcb_xkb_xi_feature_t := 16;  -- /usr/include/xcb/xkb.h:385

   subtype xcb_xkb_per_client_flag_t is unsigned;
   XCB_XKB_PER_CLIENT_FLAG_DETECTABLE_AUTO_REPEAT : constant xcb_xkb_per_client_flag_t := 1;
   XCB_XKB_PER_CLIENT_FLAG_GRABS_USE_XKB_STATE : constant xcb_xkb_per_client_flag_t := 2;
   XCB_XKB_PER_CLIENT_FLAG_AUTO_RESET_CONTROLS : constant xcb_xkb_per_client_flag_t := 4;
   XCB_XKB_PER_CLIENT_FLAG_LOOKUP_STATE_WHEN_GRABBED : constant xcb_xkb_per_client_flag_t := 8;
   XCB_XKB_PER_CLIENT_FLAG_SEND_EVENT_USES_XKB_STATE : constant xcb_xkb_per_client_flag_t := 16;  -- /usr/include/xcb/xkb.h:393

   type xcb_xkb_mod_def_t is record
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:405
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:406
      vmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:407
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_mod_def_t);  -- /usr/include/xcb/xkb.h:404

   type xcb_xkb_mod_def_iterator_t is record
      data : access xcb_xkb_mod_def_t;  -- /usr/include/xcb/xkb.h:414
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:415
      index : aliased int;  -- /usr/include/xcb/xkb.h:416
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_mod_def_iterator_t);  -- /usr/include/xcb/xkb.h:413

   subtype xcb_xkb_key_name_t_name_array is Interfaces.C.char_array (0 .. 3);
   type xcb_xkb_key_name_t is record
      name : aliased xcb_xkb_key_name_t_name_array;  -- /usr/include/xcb/xkb.h:423
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_name_t);  -- /usr/include/xcb/xkb.h:422

   type xcb_xkb_key_name_iterator_t is record
      data : access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:430
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:431
      index : aliased int;  -- /usr/include/xcb/xkb.h:432
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_name_iterator_t);  -- /usr/include/xcb/xkb.h:429

   subtype xcb_xkb_key_alias_t_real_array is Interfaces.C.char_array (0 .. 3);
   subtype xcb_xkb_key_alias_t_alias_array is Interfaces.C.char_array (0 .. 3);
   type xcb_xkb_key_alias_t is record
      real : aliased xcb_xkb_key_alias_t_real_array;  -- /usr/include/xcb/xkb.h:439
      alias : aliased xcb_xkb_key_alias_t_alias_array;  -- /usr/include/xcb/xkb.h:440
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_alias_t);  -- /usr/include/xcb/xkb.h:438

   type xcb_xkb_key_alias_iterator_t is record
      data : access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:447
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:448
      index : aliased int;  -- /usr/include/xcb/xkb.h:449
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_alias_iterator_t);  -- /usr/include/xcb/xkb.h:446

   type xcb_xkb_counted_string_16_t is record
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:456
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_counted_string_16_t);  -- /usr/include/xcb/xkb.h:455

   type xcb_xkb_counted_string_16_iterator_t is record
      data : access xcb_xkb_counted_string_16_t;  -- /usr/include/xcb/xkb.h:463
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:464
      index : aliased int;  -- /usr/include/xcb/xkb.h:465
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_counted_string_16_iterator_t);  -- /usr/include/xcb/xkb.h:462

   type xcb_xkb_kt_map_entry_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_kt_map_entry_t is record
      active : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:472
      mods_mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:473
      level : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:474
      mods_mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:475
      mods_vmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:476
      pad0 : aliased xcb_xkb_kt_map_entry_t_pad0_array;  -- /usr/include/xcb/xkb.h:477
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_kt_map_entry_t);  -- /usr/include/xcb/xkb.h:471

   type xcb_xkb_kt_map_entry_iterator_t is record
      data : access xcb_xkb_kt_map_entry_t;  -- /usr/include/xcb/xkb.h:484
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:485
      index : aliased int;  -- /usr/include/xcb/xkb.h:486
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_kt_map_entry_iterator_t);  -- /usr/include/xcb/xkb.h:483

   type xcb_xkb_key_type_t is record
      mods_mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:493
      mods_mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:494
      mods_vmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:495
      numLevels : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:496
      nMapEntries : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:497
      hasPreserve : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:498
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:499
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_type_t);  -- /usr/include/xcb/xkb.h:492

   type xcb_xkb_key_type_iterator_t is record
      data : access xcb_xkb_key_type_t;  -- /usr/include/xcb/xkb.h:506
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:507
      index : aliased int;  -- /usr/include/xcb/xkb.h:508
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_type_iterator_t);  -- /usr/include/xcb/xkb.h:505

   type xcb_xkb_key_sym_map_t_kt_index_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_key_sym_map_t is record
      kt_index : aliased xcb_xkb_key_sym_map_t_kt_index_array;  -- /usr/include/xcb/xkb.h:515
      groupInfo : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:516
      width : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:517
      nSyms : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:518
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_sym_map_t);  -- /usr/include/xcb/xkb.h:514

   type xcb_xkb_key_sym_map_iterator_t is record
      data : access xcb_xkb_key_sym_map_t;  -- /usr/include/xcb/xkb.h:525
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:526
      index : aliased int;  -- /usr/include/xcb/xkb.h:527
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_sym_map_iterator_t);  -- /usr/include/xcb/xkb.h:524

   type xcb_xkb_common_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:534
      data : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:535
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_common_behavior_t);  -- /usr/include/xcb/xkb.h:533

   type xcb_xkb_common_behavior_iterator_t is record
      data : access xcb_xkb_common_behavior_t;  -- /usr/include/xcb/xkb.h:542
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:543
      index : aliased int;  -- /usr/include/xcb/xkb.h:544
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_common_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:541

   type xcb_xkb_default_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:551
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:552
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_default_behavior_t);  -- /usr/include/xcb/xkb.h:550

   type xcb_xkb_default_behavior_iterator_t is record
      data : access xcb_xkb_default_behavior_t;  -- /usr/include/xcb/xkb.h:559
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:560
      index : aliased int;  -- /usr/include/xcb/xkb.h:561
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_default_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:558

   type xcb_xkb_lock_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:568
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:569
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_lock_behavior_t);  -- /usr/include/xcb/xkb.h:567

   type xcb_xkb_lock_behavior_iterator_t is record
      data : access xcb_xkb_lock_behavior_t;  -- /usr/include/xcb/xkb.h:576
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:577
      index : aliased int;  -- /usr/include/xcb/xkb.h:578
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_lock_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:575

   type xcb_xkb_radio_group_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:585
      group : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:586
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_radio_group_behavior_t);  -- /usr/include/xcb/xkb.h:584

   type xcb_xkb_radio_group_behavior_iterator_t is record
      data : access xcb_xkb_radio_group_behavior_t;  -- /usr/include/xcb/xkb.h:593
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:594
      index : aliased int;  -- /usr/include/xcb/xkb.h:595
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_radio_group_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:592

   type xcb_xkb_overlay_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:602
      key : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:603
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_behavior_t);  -- /usr/include/xcb/xkb.h:601

   type xcb_xkb_overlay_behavior_iterator_t is record
      data : access xcb_xkb_overlay_behavior_t;  -- /usr/include/xcb/xkb.h:610
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:611
      index : aliased int;  -- /usr/include/xcb/xkb.h:612
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:609

   type xcb_xkb_permament_lock_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:619
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:620
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_permament_lock_behavior_t);  -- /usr/include/xcb/xkb.h:618

   type xcb_xkb_permament_lock_behavior_iterator_t is record
      data : access xcb_xkb_permament_lock_behavior_t;  -- /usr/include/xcb/xkb.h:627
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:628
      index : aliased int;  -- /usr/include/xcb/xkb.h:629
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_permament_lock_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:626

   type xcb_xkb_permament_radio_group_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:636
      group : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:637
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_permament_radio_group_behavior_t);  -- /usr/include/xcb/xkb.h:635

   type xcb_xkb_permament_radio_group_behavior_iterator_t is record
      data : access xcb_xkb_permament_radio_group_behavior_t;  -- /usr/include/xcb/xkb.h:644
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:645
      index : aliased int;  -- /usr/include/xcb/xkb.h:646
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_permament_radio_group_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:643

   type xcb_xkb_permament_overlay_behavior_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:653
      key : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:654
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_permament_overlay_behavior_t);  -- /usr/include/xcb/xkb.h:652

   type xcb_xkb_permament_overlay_behavior_iterator_t is record
      data : access xcb_xkb_permament_overlay_behavior_t;  -- /usr/include/xcb/xkb.h:661
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:662
      index : aliased int;  -- /usr/include/xcb/xkb.h:663
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_permament_overlay_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:660

   type xcb_xkb_behavior_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            common : aliased xcb_xkb_common_behavior_t;  -- /usr/include/xcb/xkb.h:670
         when 1 =>
            u_default : aliased xcb_xkb_default_behavior_t;  -- /usr/include/xcb/xkb.h:671
         when 2 =>
            lock : aliased xcb_xkb_lock_behavior_t;  -- /usr/include/xcb/xkb.h:672
         when 3 =>
            radioGroup : aliased xcb_xkb_radio_group_behavior_t;  -- /usr/include/xcb/xkb.h:673
         when 4 =>
            overlay1 : aliased xcb_xkb_overlay_behavior_t;  -- /usr/include/xcb/xkb.h:674
         when 5 =>
            overlay2 : aliased xcb_xkb_overlay_behavior_t;  -- /usr/include/xcb/xkb.h:675
         when 6 =>
            permamentLock : aliased xcb_xkb_permament_lock_behavior_t;  -- /usr/include/xcb/xkb.h:676
         when 7 =>
            permamentRadioGroup : aliased xcb_xkb_permament_radio_group_behavior_t;  -- /usr/include/xcb/xkb.h:677
         when 8 =>
            permamentOverlay1 : aliased xcb_xkb_permament_overlay_behavior_t;  -- /usr/include/xcb/xkb.h:678
         when 9 =>
            permamentOverlay2 : aliased xcb_xkb_permament_overlay_behavior_t;  -- /usr/include/xcb/xkb.h:679
         when others =>
            c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:680
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_behavior_t);
   pragma Unchecked_Union (xcb_xkb_behavior_t);  -- /usr/include/xcb/xkb.h:669

   type xcb_xkb_behavior_iterator_t is record
      data : access xcb_xkb_behavior_t;  -- /usr/include/xcb/xkb.h:687
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:688
      index : aliased int;  -- /usr/include/xcb/xkb.h:689
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:686

   subtype xcb_xkb_behavior_type_t is unsigned;
   XCB_XKB_BEHAVIOR_TYPE_DEFAULT : constant xcb_xkb_behavior_type_t := 0;
   XCB_XKB_BEHAVIOR_TYPE_LOCK : constant xcb_xkb_behavior_type_t := 1;
   XCB_XKB_BEHAVIOR_TYPE_RADIO_GROUP : constant xcb_xkb_behavior_type_t := 2;
   XCB_XKB_BEHAVIOR_TYPE_OVERLAY_1 : constant xcb_xkb_behavior_type_t := 3;
   XCB_XKB_BEHAVIOR_TYPE_OVERLAY_2 : constant xcb_xkb_behavior_type_t := 4;
   XCB_XKB_BEHAVIOR_TYPE_PERMAMENT_LOCK : constant xcb_xkb_behavior_type_t := 129;
   XCB_XKB_BEHAVIOR_TYPE_PERMAMENT_RADIO_GROUP : constant xcb_xkb_behavior_type_t := 130;
   XCB_XKB_BEHAVIOR_TYPE_PERMAMENT_OVERLAY_1 : constant xcb_xkb_behavior_type_t := 131;
   XCB_XKB_BEHAVIOR_TYPE_PERMAMENT_OVERLAY_2 : constant xcb_xkb_behavior_type_t := 132;  -- /usr/include/xcb/xkb.h:692

   type xcb_xkb_set_behavior_t is record
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:708
      behavior : xcb_xkb_behavior_t;  -- /usr/include/xcb/xkb.h:709
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:710
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_behavior_t);  -- /usr/include/xcb/xkb.h:707

   type xcb_xkb_set_behavior_iterator_t is record
      data : access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:717
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:718
      index : aliased int;  -- /usr/include/xcb/xkb.h:719
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:716

   type xcb_xkb_set_explicit_t is record
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:726
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_explicit_t);  -- /usr/include/xcb/xkb.h:725

   type xcb_xkb_set_explicit_iterator_t is record
      data : access xcb_xkb_set_explicit_t;  -- /usr/include/xcb/xkb.h:734
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:735
      index : aliased int;  -- /usr/include/xcb/xkb.h:736
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_explicit_iterator_t);  -- /usr/include/xcb/xkb.h:733

   type xcb_xkb_key_mod_map_t is record
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:743
      mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:744
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_mod_map_t);  -- /usr/include/xcb/xkb.h:742

   type xcb_xkb_key_mod_map_iterator_t is record
      data : access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:751
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:752
      index : aliased int;  -- /usr/include/xcb/xkb.h:753
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_mod_map_iterator_t);  -- /usr/include/xcb/xkb.h:750

   type xcb_xkb_key_v_mod_map_t is record
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:760
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:761
      vmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:762
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_v_mod_map_t);  -- /usr/include/xcb/xkb.h:759

   type xcb_xkb_key_v_mod_map_iterator_t is record
      data : access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:769
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:770
      index : aliased int;  -- /usr/include/xcb/xkb.h:771
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_v_mod_map_iterator_t);  -- /usr/include/xcb/xkb.h:768

   type xcb_xkb_kt_set_map_entry_t is record
      level : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:778
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:779
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:780
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_kt_set_map_entry_t);  -- /usr/include/xcb/xkb.h:777

   type xcb_xkb_kt_set_map_entry_iterator_t is record
      data : access xcb_xkb_kt_set_map_entry_t;  -- /usr/include/xcb/xkb.h:787
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:788
      index : aliased int;  -- /usr/include/xcb/xkb.h:789
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_kt_set_map_entry_iterator_t);  -- /usr/include/xcb/xkb.h:786

   type xcb_xkb_set_key_type_t is record
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:796
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:797
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:798
      numLevels : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:799
      nMapEntries : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:800
      preserve : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:801
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:802
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_key_type_t);  -- /usr/include/xcb/xkb.h:795

   type xcb_xkb_set_key_type_iterator_t is record
      data : access xcb_xkb_set_key_type_t;  -- /usr/include/xcb/xkb.h:809
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:810
      index : aliased int;  -- /usr/include/xcb/xkb.h:811
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_key_type_iterator_t);  -- /usr/include/xcb/xkb.h:808

   subtype xcb_xkb_string8_t is char;  -- /usr/include/xcb/xkb.h:814

   type xcb_xkb_string8_iterator_t is record
      data : access xcb_xkb_string8_t;  -- /usr/include/xcb/xkb.h:820
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:821
      index : aliased int;  -- /usr/include/xcb/xkb.h:822
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_string8_iterator_t);  -- /usr/include/xcb/xkb.h:819

   type xcb_xkb_outline_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_outline_t is record
      nPoints : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:829
      cornerRadius : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:830
      pad0 : aliased xcb_xkb_outline_t_pad0_array;  -- /usr/include/xcb/xkb.h:831
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_outline_t);  -- /usr/include/xcb/xkb.h:828

   type xcb_xkb_outline_iterator_t is record
      data : access xcb_xkb_outline_t;  -- /usr/include/xcb/xkb.h:838
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:839
      index : aliased int;  -- /usr/include/xcb/xkb.h:840
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_outline_iterator_t);  -- /usr/include/xcb/xkb.h:837

   type xcb_xkb_shape_t is record
      name : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:847
      nOutlines : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:848
      primaryNdx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:849
      approxNdx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:850
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:851
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_shape_t);  -- /usr/include/xcb/xkb.h:846

   type xcb_xkb_shape_iterator_t is record
      data : access xcb_xkb_shape_t;  -- /usr/include/xcb/xkb.h:858
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:859
      index : aliased int;  -- /usr/include/xcb/xkb.h:860
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_shape_iterator_t);  -- /usr/include/xcb/xkb.h:857

   type xcb_xkb_key_t_name_array is array (0 .. 3) of aliased xcb_xkb_string8_t;
   type xcb_xkb_key_t is record
      name : aliased xcb_xkb_key_t_name_array;  -- /usr/include/xcb/xkb.h:867
      gap : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:868
      shapeNdx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:869
      colorNdx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:870
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_t);  -- /usr/include/xcb/xkb.h:866

   type xcb_xkb_key_iterator_t is record
      data : access xcb_xkb_key_t;  -- /usr/include/xcb/xkb.h:877
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:878
      index : aliased int;  -- /usr/include/xcb/xkb.h:879
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_key_iterator_t);  -- /usr/include/xcb/xkb.h:876

   type xcb_xkb_overlay_key_t_over_array is array (0 .. 3) of aliased xcb_xkb_string8_t;
   type xcb_xkb_overlay_key_t_under_array is array (0 .. 3) of aliased xcb_xkb_string8_t;
   type xcb_xkb_overlay_key_t is record
      over : aliased xcb_xkb_overlay_key_t_over_array;  -- /usr/include/xcb/xkb.h:886
      under : aliased xcb_xkb_overlay_key_t_under_array;  -- /usr/include/xcb/xkb.h:887
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_key_t);  -- /usr/include/xcb/xkb.h:885

   type xcb_xkb_overlay_key_iterator_t is record
      data : access xcb_xkb_overlay_key_t;  -- /usr/include/xcb/xkb.h:894
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:895
      index : aliased int;  -- /usr/include/xcb/xkb.h:896
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_key_iterator_t);  -- /usr/include/xcb/xkb.h:893

   type xcb_xkb_overlay_row_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_overlay_row_t is record
      rowUnder : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:903
      nKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:904
      pad0 : aliased xcb_xkb_overlay_row_t_pad0_array;  -- /usr/include/xcb/xkb.h:905
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_row_t);  -- /usr/include/xcb/xkb.h:902

   type xcb_xkb_overlay_row_iterator_t is record
      data : access xcb_xkb_overlay_row_t;  -- /usr/include/xcb/xkb.h:912
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:913
      index : aliased int;  -- /usr/include/xcb/xkb.h:914
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_row_iterator_t);  -- /usr/include/xcb/xkb.h:911

   type xcb_xkb_overlay_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_overlay_t is record
      name : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:921
      nRows : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:922
      pad0 : aliased xcb_xkb_overlay_t_pad0_array;  -- /usr/include/xcb/xkb.h:923
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_t);  -- /usr/include/xcb/xkb.h:920

   type xcb_xkb_overlay_iterator_t is record
      data : access xcb_xkb_overlay_t;  -- /usr/include/xcb/xkb.h:930
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:931
      index : aliased int;  -- /usr/include/xcb/xkb.h:932
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_overlay_iterator_t);  -- /usr/include/xcb/xkb.h:929

   type xcb_xkb_row_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_row_t is record
      top : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:939
      left : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:940
      nKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:941
      vertical : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:942
      pad0 : aliased xcb_xkb_row_t_pad0_array;  -- /usr/include/xcb/xkb.h:943
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_row_t);  -- /usr/include/xcb/xkb.h:938

   type xcb_xkb_row_iterator_t is record
      data : access xcb_xkb_row_t;  -- /usr/include/xcb/xkb.h:950
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:951
      index : aliased int;  -- /usr/include/xcb/xkb.h:952
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_row_iterator_t);  -- /usr/include/xcb/xkb.h:949

   subtype xcb_xkb_doodad_type_t is unsigned;
   XCB_XKB_DOODAD_TYPE_OUTLINE : constant xcb_xkb_doodad_type_t := 1;
   XCB_XKB_DOODAD_TYPE_SOLID : constant xcb_xkb_doodad_type_t := 2;
   XCB_XKB_DOODAD_TYPE_TEXT : constant xcb_xkb_doodad_type_t := 3;
   XCB_XKB_DOODAD_TYPE_INDICATOR : constant xcb_xkb_doodad_type_t := 4;
   XCB_XKB_DOODAD_TYPE_LOGO : constant xcb_xkb_doodad_type_t := 5;  -- /usr/include/xcb/xkb.h:955

   type xcb_xkb_listing_t is record
      flags : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:967
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:968
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_listing_t);  -- /usr/include/xcb/xkb.h:966

   type xcb_xkb_listing_iterator_t is record
      data : access xcb_xkb_listing_t;  -- /usr/include/xcb/xkb.h:975
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:976
      index : aliased int;  -- /usr/include/xcb/xkb.h:977
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_listing_iterator_t);  -- /usr/include/xcb/xkb.h:974

   type xcb_xkb_device_led_info_t is record
      ledClass : aliased xcb_xkb_led_class_spec_t;  -- /usr/include/xcb/xkb.h:984
      ledID : aliased xcb_xkb_id_spec_t;  -- /usr/include/xcb/xkb.h:985
      namesPresent : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:986
      mapsPresent : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:987
      physIndicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:988
      state : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:989
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_device_led_info_t);  -- /usr/include/xcb/xkb.h:983

   type xcb_xkb_device_led_info_iterator_t is record
      data : access xcb_xkb_device_led_info_t;  -- /usr/include/xcb/xkb.h:996
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:997
      index : aliased int;  -- /usr/include/xcb/xkb.h:998
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_device_led_info_iterator_t);  -- /usr/include/xcb/xkb.h:995

   subtype xcb_xkb_error_t is unsigned;
   XCB_XKB_ERROR_BAD_DEVICE : constant xcb_xkb_error_t := 255;
   XCB_XKB_ERROR_BAD_CLASS : constant xcb_xkb_error_t := 254;
   XCB_XKB_ERROR_BAD_ID : constant xcb_xkb_error_t := 253;  -- /usr/include/xcb/xkb.h:1001

   type xcb_xkb_keyboard_error_t_pad0_array is array (0 .. 20) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_keyboard_error_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1014
      error_code : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1015
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1016
      value : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1017
      minorOpcode : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1018
      majorOpcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1019
      pad0 : aliased xcb_xkb_keyboard_error_t_pad0_array;  -- /usr/include/xcb/xkb.h:1020
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_keyboard_error_t);  -- /usr/include/xcb/xkb.h:1013

   subtype xcb_xkb_sa_t is unsigned;
   XCB_XKB_SA_CLEAR_LOCKS : constant xcb_xkb_sa_t := 1;
   XCB_XKB_SA_LATCH_TO_LOCK : constant xcb_xkb_sa_t := 2;
   XCB_XKB_SA_USE_MOD_MAP_MODS : constant xcb_xkb_sa_t := 4;
   XCB_XKB_SA_GROUP_ABSOLUTE : constant xcb_xkb_sa_t := 4;  -- /usr/include/xcb/xkb.h:1023

   type xcb_xkb_sa_type_t is
     (XCB_XKB_SA_TYPE_NO_ACTION,
      XCB_XKB_SA_TYPE_SET_MODS,
      XCB_XKB_SA_TYPE_LATCH_MODS,
      XCB_XKB_SA_TYPE_LOCK_MODS,
      XCB_XKB_SA_TYPE_SET_GROUP,
      XCB_XKB_SA_TYPE_LATCH_GROUP,
      XCB_XKB_SA_TYPE_LOCK_GROUP,
      XCB_XKB_SA_TYPE_MOVE_PTR,
      XCB_XKB_SA_TYPE_PTR_BTN,
      XCB_XKB_SA_TYPE_LOCK_PTR_BTN,
      XCB_XKB_SA_TYPE_SET_PTR_DFLT,
      XCB_XKB_SA_TYPE_ISO_LOCK,
      XCB_XKB_SA_TYPE_TERMINATE,
      XCB_XKB_SA_TYPE_SWITCH_SCREEN,
      XCB_XKB_SA_TYPE_SET_CONTROLS,
      XCB_XKB_SA_TYPE_LOCK_CONTROLS,
      XCB_XKB_SA_TYPE_ACTION_MESSAGE,
      XCB_XKB_SA_TYPE_REDIRECT_KEY,
      XCB_XKB_SA_TYPE_DEVICE_BTN,
      XCB_XKB_SA_TYPE_LOCK_DEVICE_BTN,
      XCB_XKB_SA_TYPE_DEVICE_VALUATOR);
   pragma Convention (C, xcb_xkb_sa_type_t);  -- /usr/include/xcb/xkb.h:1030

   type xcb_xkb_sa_no_action_t_pad0_array is array (0 .. 6) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_no_action_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1058
      pad0 : aliased xcb_xkb_sa_no_action_t_pad0_array;  -- /usr/include/xcb/xkb.h:1059
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_no_action_t);  -- /usr/include/xcb/xkb.h:1057

   type xcb_xkb_sa_no_action_iterator_t is record
      data : access xcb_xkb_sa_no_action_t;  -- /usr/include/xcb/xkb.h:1066
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1067
      index : aliased int;  -- /usr/include/xcb/xkb.h:1068
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_no_action_iterator_t);  -- /usr/include/xcb/xkb.h:1065

   type xcb_xkb_sa_set_mods_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_set_mods_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1075
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1076
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1077
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1078
      vmodsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1079
      vmodsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1080
      pad0 : aliased xcb_xkb_sa_set_mods_t_pad0_array;  -- /usr/include/xcb/xkb.h:1081
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_mods_t);  -- /usr/include/xcb/xkb.h:1074

   type xcb_xkb_sa_set_mods_iterator_t is record
      data : access xcb_xkb_sa_set_mods_t;  -- /usr/include/xcb/xkb.h:1088
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1089
      index : aliased int;  -- /usr/include/xcb/xkb.h:1090
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_mods_iterator_t);  -- /usr/include/xcb/xkb.h:1087

   type xcb_xkb_sa_latch_mods_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_latch_mods_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1097
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1098
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1099
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1100
      vmodsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1101
      vmodsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1102
      pad0 : aliased xcb_xkb_sa_latch_mods_t_pad0_array;  -- /usr/include/xcb/xkb.h:1103
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_latch_mods_t);  -- /usr/include/xcb/xkb.h:1096

   type xcb_xkb_sa_latch_mods_iterator_t is record
      data : access xcb_xkb_sa_latch_mods_t;  -- /usr/include/xcb/xkb.h:1110
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1111
      index : aliased int;  -- /usr/include/xcb/xkb.h:1112
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_latch_mods_iterator_t);  -- /usr/include/xcb/xkb.h:1109

   type xcb_xkb_sa_lock_mods_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_lock_mods_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1119
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1120
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1121
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1122
      vmodsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1123
      vmodsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1124
      pad0 : aliased xcb_xkb_sa_lock_mods_t_pad0_array;  -- /usr/include/xcb/xkb.h:1125
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_mods_t);  -- /usr/include/xcb/xkb.h:1118

   type xcb_xkb_sa_lock_mods_iterator_t is record
      data : access xcb_xkb_sa_lock_mods_t;  -- /usr/include/xcb/xkb.h:1132
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1133
      index : aliased int;  -- /usr/include/xcb/xkb.h:1134
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_mods_iterator_t);  -- /usr/include/xcb/xkb.h:1131

   type xcb_xkb_sa_set_group_t_pad0_array is array (0 .. 4) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_set_group_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1141
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1142
      group : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1143
      pad0 : aliased xcb_xkb_sa_set_group_t_pad0_array;  -- /usr/include/xcb/xkb.h:1144
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_group_t);  -- /usr/include/xcb/xkb.h:1140

   type xcb_xkb_sa_set_group_iterator_t is record
      data : access xcb_xkb_sa_set_group_t;  -- /usr/include/xcb/xkb.h:1151
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1152
      index : aliased int;  -- /usr/include/xcb/xkb.h:1153
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_group_iterator_t);  -- /usr/include/xcb/xkb.h:1150

   type xcb_xkb_sa_latch_group_t_pad0_array is array (0 .. 4) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_latch_group_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1160
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1161
      group : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1162
      pad0 : aliased xcb_xkb_sa_latch_group_t_pad0_array;  -- /usr/include/xcb/xkb.h:1163
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_latch_group_t);  -- /usr/include/xcb/xkb.h:1159

   type xcb_xkb_sa_latch_group_iterator_t is record
      data : access xcb_xkb_sa_latch_group_t;  -- /usr/include/xcb/xkb.h:1170
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1171
      index : aliased int;  -- /usr/include/xcb/xkb.h:1172
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_latch_group_iterator_t);  -- /usr/include/xcb/xkb.h:1169

   type xcb_xkb_sa_lock_group_t_pad0_array is array (0 .. 4) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_lock_group_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1179
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1180
      group : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1181
      pad0 : aliased xcb_xkb_sa_lock_group_t_pad0_array;  -- /usr/include/xcb/xkb.h:1182
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_group_t);  -- /usr/include/xcb/xkb.h:1178

   type xcb_xkb_sa_lock_group_iterator_t is record
      data : access xcb_xkb_sa_lock_group_t;  -- /usr/include/xcb/xkb.h:1189
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1190
      index : aliased int;  -- /usr/include/xcb/xkb.h:1191
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_group_iterator_t);  -- /usr/include/xcb/xkb.h:1188

   subtype xcb_xkb_sa_move_ptr_flag_t is unsigned;
   XCB_XKB_SA_MOVE_PTR_FLAG_NO_ACCELERATION : constant xcb_xkb_sa_move_ptr_flag_t := 1;
   XCB_XKB_SA_MOVE_PTR_FLAG_MOVE_ABSOLUTE_X : constant xcb_xkb_sa_move_ptr_flag_t := 2;
   XCB_XKB_SA_MOVE_PTR_FLAG_MOVE_ABSOLUTE_Y : constant xcb_xkb_sa_move_ptr_flag_t := 4;  -- /usr/include/xcb/xkb.h:1194

   type xcb_xkb_sa_move_ptr_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_move_ptr_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1204
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1205
      xHigh : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1206
      xLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1207
      yHigh : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1208
      yLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1209
      pad0 : aliased xcb_xkb_sa_move_ptr_t_pad0_array;  -- /usr/include/xcb/xkb.h:1210
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_move_ptr_t);  -- /usr/include/xcb/xkb.h:1203

   type xcb_xkb_sa_move_ptr_iterator_t is record
      data : access xcb_xkb_sa_move_ptr_t;  -- /usr/include/xcb/xkb.h:1217
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1218
      index : aliased int;  -- /usr/include/xcb/xkb.h:1219
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_move_ptr_iterator_t);  -- /usr/include/xcb/xkb.h:1216

   type xcb_xkb_sa_ptr_btn_t_pad0_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_ptr_btn_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1226
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1227
      count : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1228
      button : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1229
      pad0 : aliased xcb_xkb_sa_ptr_btn_t_pad0_array;  -- /usr/include/xcb/xkb.h:1230
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_ptr_btn_t);  -- /usr/include/xcb/xkb.h:1225

   type xcb_xkb_sa_ptr_btn_iterator_t is record
      data : access xcb_xkb_sa_ptr_btn_t;  -- /usr/include/xcb/xkb.h:1237
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1238
      index : aliased int;  -- /usr/include/xcb/xkb.h:1239
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_ptr_btn_iterator_t);  -- /usr/include/xcb/xkb.h:1236

   type xcb_xkb_sa_lock_ptr_btn_t_pad1_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_lock_ptr_btn_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1246
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1247
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1248
      button : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1249
      pad1 : aliased xcb_xkb_sa_lock_ptr_btn_t_pad1_array;  -- /usr/include/xcb/xkb.h:1250
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_ptr_btn_t);  -- /usr/include/xcb/xkb.h:1245

   type xcb_xkb_sa_lock_ptr_btn_iterator_t is record
      data : access xcb_xkb_sa_lock_ptr_btn_t;  -- /usr/include/xcb/xkb.h:1257
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1258
      index : aliased int;  -- /usr/include/xcb/xkb.h:1259
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_ptr_btn_iterator_t);  -- /usr/include/xcb/xkb.h:1256

   subtype xcb_xkb_sa_set_ptr_dflt_flag_t is unsigned;
   XCB_XKB_SA_SET_PTR_DFLT_FLAG_DFLT_BTN_ABSOLUTE : constant xcb_xkb_sa_set_ptr_dflt_flag_t := 4;
   XCB_XKB_SA_SET_PTR_DFLT_FLAG_AFFECT_DFLT_BUTTON : constant xcb_xkb_sa_set_ptr_dflt_flag_t := 1;  -- /usr/include/xcb/xkb.h:1262

   type xcb_xkb_sa_set_ptr_dflt_t_pad0_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_set_ptr_dflt_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1271
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1272
      affect : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1273
      value : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1274
      pad0 : aliased xcb_xkb_sa_set_ptr_dflt_t_pad0_array;  -- /usr/include/xcb/xkb.h:1275
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_ptr_dflt_t);  -- /usr/include/xcb/xkb.h:1270

   type xcb_xkb_sa_set_ptr_dflt_iterator_t is record
      data : access xcb_xkb_sa_set_ptr_dflt_t;  -- /usr/include/xcb/xkb.h:1282
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1283
      index : aliased int;  -- /usr/include/xcb/xkb.h:1284
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_ptr_dflt_iterator_t);  -- /usr/include/xcb/xkb.h:1281

   subtype xcb_xkb_sa_iso_lock_flag_t is unsigned;
   XCB_XKB_SA_ISO_LOCK_FLAG_NO_LOCK : constant xcb_xkb_sa_iso_lock_flag_t := 1;
   XCB_XKB_SA_ISO_LOCK_FLAG_NO_UNLOCK : constant xcb_xkb_sa_iso_lock_flag_t := 2;
   XCB_XKB_SA_ISO_LOCK_FLAG_USE_MOD_MAP_MODS : constant xcb_xkb_sa_iso_lock_flag_t := 4;
   XCB_XKB_SA_ISO_LOCK_FLAG_GROUP_ABSOLUTE : constant xcb_xkb_sa_iso_lock_flag_t := 4;
   XCB_XKB_SA_ISO_LOCK_FLAG_ISO_DFLT_IS_GROUP : constant xcb_xkb_sa_iso_lock_flag_t := 8;  -- /usr/include/xcb/xkb.h:1287

   subtype xcb_xkb_sa_iso_lock_no_affect_t is unsigned;
   XCB_XKB_SA_ISO_LOCK_NO_AFFECT_CTRLS : constant xcb_xkb_sa_iso_lock_no_affect_t := 8;
   XCB_XKB_SA_ISO_LOCK_NO_AFFECT_PTR : constant xcb_xkb_sa_iso_lock_no_affect_t := 16;
   XCB_XKB_SA_ISO_LOCK_NO_AFFECT_GROUP : constant xcb_xkb_sa_iso_lock_no_affect_t := 32;
   XCB_XKB_SA_ISO_LOCK_NO_AFFECT_MODS : constant xcb_xkb_sa_iso_lock_no_affect_t := 64;  -- /usr/include/xcb/xkb.h:1295

   type xcb_xkb_sa_iso_lock_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1306
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1307
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1308
      realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1309
      group : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1310
      affect : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1311
      vmodsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1312
      vmodsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1313
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_iso_lock_t);  -- /usr/include/xcb/xkb.h:1305

   type xcb_xkb_sa_iso_lock_iterator_t is record
      data : access xcb_xkb_sa_iso_lock_t;  -- /usr/include/xcb/xkb.h:1320
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1321
      index : aliased int;  -- /usr/include/xcb/xkb.h:1322
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_iso_lock_iterator_t);  -- /usr/include/xcb/xkb.h:1319

   type xcb_xkb_sa_terminate_t_pad0_array is array (0 .. 6) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_terminate_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1329
      pad0 : aliased xcb_xkb_sa_terminate_t_pad0_array;  -- /usr/include/xcb/xkb.h:1330
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_terminate_t);  -- /usr/include/xcb/xkb.h:1328

   type xcb_xkb_sa_terminate_iterator_t is record
      data : access xcb_xkb_sa_terminate_t;  -- /usr/include/xcb/xkb.h:1337
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1338
      index : aliased int;  -- /usr/include/xcb/xkb.h:1339
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_terminate_iterator_t);  -- /usr/include/xcb/xkb.h:1336

   subtype xcb_xkb_switch_screen_flag_t is unsigned;
   XCB_XKB_SWITCH_SCREEN_FLAG_APPLICATION : constant xcb_xkb_switch_screen_flag_t := 1;
   XCB_XKB_SWITCH_SCREEN_FLAG_ABSOLUTE : constant xcb_xkb_switch_screen_flag_t := 4;  -- /usr/include/xcb/xkb.h:1342

   type xcb_xkb_sa_switch_screen_t_pad0_array is array (0 .. 4) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_switch_screen_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1351
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1352
      newScreen : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1353
      pad0 : aliased xcb_xkb_sa_switch_screen_t_pad0_array;  -- /usr/include/xcb/xkb.h:1354
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_switch_screen_t);  -- /usr/include/xcb/xkb.h:1350

   type xcb_xkb_sa_switch_screen_iterator_t is record
      data : access xcb_xkb_sa_switch_screen_t;  -- /usr/include/xcb/xkb.h:1361
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1362
      index : aliased int;  -- /usr/include/xcb/xkb.h:1363
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_switch_screen_iterator_t);  -- /usr/include/xcb/xkb.h:1360

   subtype xcb_xkb_bool_ctrls_high_t is unsigned;
   XCB_XKB_BOOL_CTRLS_HIGH_ACCESS_X_FEEDBACK : constant xcb_xkb_bool_ctrls_high_t := 1;
   XCB_XKB_BOOL_CTRLS_HIGH_AUDIBLE_BELL : constant xcb_xkb_bool_ctrls_high_t := 2;
   XCB_XKB_BOOL_CTRLS_HIGH_OVERLAY_1 : constant xcb_xkb_bool_ctrls_high_t := 4;
   XCB_XKB_BOOL_CTRLS_HIGH_OVERLAY_2 : constant xcb_xkb_bool_ctrls_high_t := 8;
   XCB_XKB_BOOL_CTRLS_HIGH_IGNORE_GROUP_LOCK : constant xcb_xkb_bool_ctrls_high_t := 16;  -- /usr/include/xcb/xkb.h:1366

   subtype xcb_xkb_bool_ctrls_low_t is unsigned;
   XCB_XKB_BOOL_CTRLS_LOW_REPEAT_KEYS : constant xcb_xkb_bool_ctrls_low_t := 1;
   XCB_XKB_BOOL_CTRLS_LOW_SLOW_KEYS : constant xcb_xkb_bool_ctrls_low_t := 2;
   XCB_XKB_BOOL_CTRLS_LOW_BOUNCE_KEYS : constant xcb_xkb_bool_ctrls_low_t := 4;
   XCB_XKB_BOOL_CTRLS_LOW_STICKY_KEYS : constant xcb_xkb_bool_ctrls_low_t := 8;
   XCB_XKB_BOOL_CTRLS_LOW_MOUSE_KEYS : constant xcb_xkb_bool_ctrls_low_t := 16;
   XCB_XKB_BOOL_CTRLS_LOW_MOUSE_KEYS_ACCEL : constant xcb_xkb_bool_ctrls_low_t := 32;
   XCB_XKB_BOOL_CTRLS_LOW_ACCESS_X_KEYS : constant xcb_xkb_bool_ctrls_low_t := 64;
   XCB_XKB_BOOL_CTRLS_LOW_ACCESS_X_TIMEOUT : constant xcb_xkb_bool_ctrls_low_t := 128;  -- /usr/include/xcb/xkb.h:1374

   type xcb_xkb_sa_set_controls_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_set_controls_t_pad1_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_set_controls_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1389
      pad0 : aliased xcb_xkb_sa_set_controls_t_pad0_array;  -- /usr/include/xcb/xkb.h:1390
      boolCtrlsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1391
      boolCtrlsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1392
      pad1 : aliased xcb_xkb_sa_set_controls_t_pad1_array;  -- /usr/include/xcb/xkb.h:1393
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_controls_t);  -- /usr/include/xcb/xkb.h:1388

   type xcb_xkb_sa_set_controls_iterator_t is record
      data : access xcb_xkb_sa_set_controls_t;  -- /usr/include/xcb/xkb.h:1400
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1401
      index : aliased int;  -- /usr/include/xcb/xkb.h:1402
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_set_controls_iterator_t);  -- /usr/include/xcb/xkb.h:1399

   type xcb_xkb_sa_lock_controls_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_lock_controls_t_pad1_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_lock_controls_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1409
      pad0 : aliased xcb_xkb_sa_lock_controls_t_pad0_array;  -- /usr/include/xcb/xkb.h:1410
      boolCtrlsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1411
      boolCtrlsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1412
      pad1 : aliased xcb_xkb_sa_lock_controls_t_pad1_array;  -- /usr/include/xcb/xkb.h:1413
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_controls_t);  -- /usr/include/xcb/xkb.h:1408

   type xcb_xkb_sa_lock_controls_iterator_t is record
      data : access xcb_xkb_sa_lock_controls_t;  -- /usr/include/xcb/xkb.h:1420
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1421
      index : aliased int;  -- /usr/include/xcb/xkb.h:1422
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_controls_iterator_t);  -- /usr/include/xcb/xkb.h:1419

   subtype xcb_xkb_action_message_flag_t is unsigned;
   XCB_XKB_ACTION_MESSAGE_FLAG_ON_PRESS : constant xcb_xkb_action_message_flag_t := 1;
   XCB_XKB_ACTION_MESSAGE_FLAG_ON_RELEASE : constant xcb_xkb_action_message_flag_t := 2;
   XCB_XKB_ACTION_MESSAGE_FLAG_GEN_KEY_EVENT : constant xcb_xkb_action_message_flag_t := 4;  -- /usr/include/xcb/xkb.h:1425

   type xcb_xkb_sa_action_message_t_message_array is array (0 .. 5) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_action_message_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1435
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1436
      message : aliased xcb_xkb_sa_action_message_t_message_array;  -- /usr/include/xcb/xkb.h:1437
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_action_message_t);  -- /usr/include/xcb/xkb.h:1434

   type xcb_xkb_sa_action_message_iterator_t is record
      data : access xcb_xkb_sa_action_message_t;  -- /usr/include/xcb/xkb.h:1444
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1445
      index : aliased int;  -- /usr/include/xcb/xkb.h:1446
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_action_message_iterator_t);  -- /usr/include/xcb/xkb.h:1443

   type xcb_xkb_sa_redirect_key_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1453
      newkey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1454
      mask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1455
      realModifiers : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1456
      vmodsMaskHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1457
      vmodsMaskLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1458
      vmodsHigh : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1459
      vmodsLow : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1460
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_redirect_key_t);  -- /usr/include/xcb/xkb.h:1452

   type xcb_xkb_sa_redirect_key_iterator_t is record
      data : access xcb_xkb_sa_redirect_key_t;  -- /usr/include/xcb/xkb.h:1467
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1468
      index : aliased int;  -- /usr/include/xcb/xkb.h:1469
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_redirect_key_iterator_t);  -- /usr/include/xcb/xkb.h:1466

   type xcb_xkb_sa_device_btn_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_device_btn_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1476
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1477
      count : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1478
      button : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1479
      device : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1480
      pad0 : aliased xcb_xkb_sa_device_btn_t_pad0_array;  -- /usr/include/xcb/xkb.h:1481
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_device_btn_t);  -- /usr/include/xcb/xkb.h:1475

   type xcb_xkb_sa_device_btn_iterator_t is record
      data : access xcb_xkb_sa_device_btn_t;  -- /usr/include/xcb/xkb.h:1488
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1489
      index : aliased int;  -- /usr/include/xcb/xkb.h:1490
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_device_btn_iterator_t);  -- /usr/include/xcb/xkb.h:1487

   subtype xcb_xkb_lock_device_flags_t is unsigned;
   XCB_XKB_LOCK_DEVICE_FLAGS_NO_LOCK : constant xcb_xkb_lock_device_flags_t := 1;
   XCB_XKB_LOCK_DEVICE_FLAGS_NO_UNLOCK : constant xcb_xkb_lock_device_flags_t := 2;  -- /usr/include/xcb/xkb.h:1493

   type xcb_xkb_sa_lock_device_btn_t_pad1_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_sa_lock_device_btn_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1502
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1503
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1504
      button : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1505
      device : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1506
      pad1 : aliased xcb_xkb_sa_lock_device_btn_t_pad1_array;  -- /usr/include/xcb/xkb.h:1507
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_device_btn_t);  -- /usr/include/xcb/xkb.h:1501

   type xcb_xkb_sa_lock_device_btn_iterator_t is record
      data : access xcb_xkb_sa_lock_device_btn_t;  -- /usr/include/xcb/xkb.h:1514
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1515
      index : aliased int;  -- /usr/include/xcb/xkb.h:1516
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_lock_device_btn_iterator_t);  -- /usr/include/xcb/xkb.h:1513

   type xcb_xkb_sa_val_what_t is
     (XCB_XKB_SA_VAL_WHAT_IGNORE_VAL,
      XCB_XKB_SA_VAL_WHAT_SET_VAL_MIN,
      XCB_XKB_SA_VAL_WHAT_SET_VAL_CENTER,
      XCB_XKB_SA_VAL_WHAT_SET_VAL_MAX,
      XCB_XKB_SA_VAL_WHAT_SET_VAL_RELATIVE,
      XCB_XKB_SA_VAL_WHAT_SET_VAL_ABSOLUTE);
   pragma Convention (C, xcb_xkb_sa_val_what_t);  -- /usr/include/xcb/xkb.h:1519

   type xcb_xkb_sa_device_valuator_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1532
      device : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1533
      val1what : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1534
      val1index : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1535
      val1value : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1536
      val2what : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1537
      val2index : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1538
      val2value : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1539
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_device_valuator_t);  -- /usr/include/xcb/xkb.h:1531

   type xcb_xkb_sa_device_valuator_iterator_t is record
      data : access xcb_xkb_sa_device_valuator_t;  -- /usr/include/xcb/xkb.h:1546
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1547
      index : aliased int;  -- /usr/include/xcb/xkb.h:1548
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sa_device_valuator_iterator_t);  -- /usr/include/xcb/xkb.h:1545

   type xcb_xkb_si_action_t_data_array is array (0 .. 6) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_si_action_t is record
      c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1555
      data : aliased xcb_xkb_si_action_t_data_array;  -- /usr/include/xcb/xkb.h:1556
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_si_action_t);  -- /usr/include/xcb/xkb.h:1554

   type xcb_xkb_si_action_iterator_t is record
      data : access xcb_xkb_si_action_t;  -- /usr/include/xcb/xkb.h:1563
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1564
      index : aliased int;  -- /usr/include/xcb/xkb.h:1565
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_si_action_iterator_t);  -- /usr/include/xcb/xkb.h:1562

   type xcb_xkb_sym_interpret_t is record
      sym : aliased XCB.XProto.xcb_keysym_t;  -- /usr/include/xcb/xkb.h:1572
      mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1573
      match : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1574
      virtualMod : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1575
      flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1576
      action : aliased xcb_xkb_si_action_t;  -- /usr/include/xcb/xkb.h:1577
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sym_interpret_t);  -- /usr/include/xcb/xkb.h:1571

   type xcb_xkb_sym_interpret_iterator_t is record
      data : access xcb_xkb_sym_interpret_t;  -- /usr/include/xcb/xkb.h:1584
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1585
      index : aliased int;  -- /usr/include/xcb/xkb.h:1586
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_sym_interpret_iterator_t);  -- /usr/include/xcb/xkb.h:1583

   type xcb_xkb_action_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            noaction : aliased xcb_xkb_sa_no_action_t;  -- /usr/include/xcb/xkb.h:1593
         when 1 =>
            setmods : aliased xcb_xkb_sa_set_mods_t;  -- /usr/include/xcb/xkb.h:1594
         when 2 =>
            latchmods : aliased xcb_xkb_sa_latch_mods_t;  -- /usr/include/xcb/xkb.h:1595
         when 3 =>
            lockmods : aliased xcb_xkb_sa_lock_mods_t;  -- /usr/include/xcb/xkb.h:1596
         when 4 =>
            setgroup : aliased xcb_xkb_sa_set_group_t;  -- /usr/include/xcb/xkb.h:1597
         when 5 =>
            latchgroup : aliased xcb_xkb_sa_latch_group_t;  -- /usr/include/xcb/xkb.h:1598
         when 6 =>
            lockgroup : aliased xcb_xkb_sa_lock_group_t;  -- /usr/include/xcb/xkb.h:1599
         when 7 =>
            moveptr : aliased xcb_xkb_sa_move_ptr_t;  -- /usr/include/xcb/xkb.h:1600
         when 8 =>
            ptrbtn : aliased xcb_xkb_sa_ptr_btn_t;  -- /usr/include/xcb/xkb.h:1601
         when 9 =>
            lockptrbtn : aliased xcb_xkb_sa_lock_ptr_btn_t;  -- /usr/include/xcb/xkb.h:1602
         when 10 =>
            setptrdflt : aliased xcb_xkb_sa_set_ptr_dflt_t;  -- /usr/include/xcb/xkb.h:1603
         when 11 =>
            isolock : aliased xcb_xkb_sa_iso_lock_t;  -- /usr/include/xcb/xkb.h:1604
         when 12 =>
            c_terminate : aliased xcb_xkb_sa_terminate_t;  -- /usr/include/xcb/xkb.h:1605
         when 13 =>
            switchscreen : aliased xcb_xkb_sa_switch_screen_t;  -- /usr/include/xcb/xkb.h:1606
         when 14 =>
            setcontrols : aliased xcb_xkb_sa_set_controls_t;  -- /usr/include/xcb/xkb.h:1607
         when 15 =>
            lockcontrols : aliased xcb_xkb_sa_lock_controls_t;  -- /usr/include/xcb/xkb.h:1608
         when 16 =>
            message : aliased xcb_xkb_sa_action_message_t;  -- /usr/include/xcb/xkb.h:1609
         when 17 =>
            redirect : aliased xcb_xkb_sa_redirect_key_t;  -- /usr/include/xcb/xkb.h:1610
         when 18 =>
            devbtn : aliased xcb_xkb_sa_device_btn_t;  -- /usr/include/xcb/xkb.h:1611
         when 19 =>
            lockdevbtn : aliased xcb_xkb_sa_lock_device_btn_t;  -- /usr/include/xcb/xkb.h:1612
         when 20 =>
            devval : aliased xcb_xkb_sa_device_valuator_t;  -- /usr/include/xcb/xkb.h:1613
         when others =>
            c_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1614
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_action_t);
   pragma Unchecked_Union (xcb_xkb_action_t);  -- /usr/include/xcb/xkb.h:1592

   type xcb_xkb_action_iterator_t is record
      data : access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:1621
      c_rem : aliased int;  -- /usr/include/xcb/xkb.h:1622
      index : aliased int;  -- /usr/include/xcb/xkb.h:1623
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_action_iterator_t);  -- /usr/include/xcb/xkb.h:1620

   type xcb_xkb_use_extension_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:1630
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_use_extension_cookie_t);  -- /usr/include/xcb/xkb.h:1629

   type xcb_xkb_use_extension_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1640
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1641
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1642
      wantedMajor : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1643
      wantedMinor : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1644
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_use_extension_request_t);  -- /usr/include/xcb/xkb.h:1639

   type xcb_xkb_use_extension_reply_t_pad0_array is array (0 .. 19) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_use_extension_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1651
      supported : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1652
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1653
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1654
      serverMajor : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1655
      serverMinor : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1656
      pad0 : aliased xcb_xkb_use_extension_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:1657
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_use_extension_reply_t);  -- /usr/include/xcb/xkb.h:1650

   type xcb_xkb_select_events_details_t is record
      affectNewKeyboard : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1664
      newKeyboardDetails : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1665
      affectState : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1666
      stateDetails : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1667
      affectCtrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1668
      ctrlDetails : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1669
      affectIndicatorState : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1670
      indicatorStateDetails : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1671
      affectIndicatorMap : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1672
      indicatorMapDetails : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1673
      affectNames : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1674
      namesDetails : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1675
      affectCompat : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1676
      compatDetails : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1677
      affectBell : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1678
      bellDetails : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1679
      affectMsgDetails : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1680
      msgDetails : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1681
      affectAccessX : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1682
      accessXDetails : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1683
      affectExtDev : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1684
      extdevDetails : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1685
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_select_events_details_t);  -- /usr/include/xcb/xkb.h:1663

   type xcb_xkb_select_events_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1695
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1696
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1697
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1698
      affectWhich : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1699
      clear : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1700
      selectAll : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1701
      affectMap : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1702
      map : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1703
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_select_events_request_t);  -- /usr/include/xcb/xkb.h:1694

   type xcb_xkb_bell_request_t_pad1_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_bell_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1713
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1714
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1715
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1716
      bellClass : aliased xcb_xkb_bell_class_spec_t;  -- /usr/include/xcb/xkb.h:1717
      bellID : aliased xcb_xkb_id_spec_t;  -- /usr/include/xcb/xkb.h:1718
      percent : aliased Libc.Stdint.int8_t;  -- /usr/include/xcb/xkb.h:1719
      forceSound : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1720
      eventOnly : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1721
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1722
      pitch : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:1723
      duration : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:1724
      pad1 : aliased xcb_xkb_bell_request_t_pad1_array;  -- /usr/include/xcb/xkb.h:1725
      name : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:1726
      window : aliased XCB.XProto.xcb_window_t;  -- /usr/include/xcb/xkb.h:1727
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_bell_request_t);  -- /usr/include/xcb/xkb.h:1712

   type xcb_xkb_get_state_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:1734
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_state_cookie_t);  -- /usr/include/xcb/xkb.h:1733

   type xcb_xkb_get_state_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_state_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1744
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1745
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1746
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1747
      pad0 : aliased xcb_xkb_get_state_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:1748
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_state_request_t);  -- /usr/include/xcb/xkb.h:1743

   type xcb_xkb_get_state_reply_t_pad1_array is array (0 .. 5) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_state_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1755
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1756
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1757
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1758
      mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1759
      baseMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1760
      latchedMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1761
      lockedMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1762
      group : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1763
      lockedGroup : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1764
      baseGroup : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:1765
      latchedGroup : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:1766
      compatState : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1767
      grabMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1768
      compatGrabMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1769
      lookupMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1770
      compatLookupMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1771
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1772
      ptrBtnState : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1773
      pad1 : aliased xcb_xkb_get_state_reply_t_pad1_array;  -- /usr/include/xcb/xkb.h:1774
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_state_reply_t);  -- /usr/include/xcb/xkb.h:1754

   type xcb_xkb_latch_lock_state_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1784
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1785
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1786
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1787
      affectModLocks : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1788
      modLocks : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1789
      lockGroup : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1790
      groupLock : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1791
      affectModLatches : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1792
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1793
      latchGroup : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1794
      groupLatch : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1795
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_latch_lock_state_request_t);  -- /usr/include/xcb/xkb.h:1783

   type xcb_xkb_get_controls_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:1802
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_controls_cookie_t);  -- /usr/include/xcb/xkb.h:1801

   type xcb_xkb_get_controls_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_controls_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1812
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1813
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1814
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1815
      pad0 : aliased xcb_xkb_get_controls_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:1816
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_controls_request_t);  -- /usr/include/xcb/xkb.h:1811

   type xcb_xkb_get_controls_reply_t_pad1_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_controls_reply_t_perKeyRepeat_array is array (0 .. 31) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_controls_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1823
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1824
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1825
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1826
      mouseKeysDfltBtn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1827
      numGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1828
      groupsWrap : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1829
      internalModsMask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1830
      ignoreLockModsMask : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1831
      internalModsRealMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1832
      ignoreLockModsRealMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1833
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1834
      internalModsVmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1835
      ignoreLockModsVmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1836
      repeatDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1837
      repeatInterval : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1838
      slowKeysDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1839
      debounceDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1840
      mouseKeysDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1841
      mouseKeysInterval : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1842
      mouseKeysTimeToMax : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1843
      mouseKeysMaxSpeed : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1844
      mouseKeysCurve : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:1845
      accessXOption : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1846
      accessXTimeout : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1847
      accessXTimeoutOptionsMask : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1848
      accessXTimeoutOptionsValues : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1849
      pad1 : aliased xcb_xkb_get_controls_reply_t_pad1_array;  -- /usr/include/xcb/xkb.h:1850
      accessXTimeoutMask : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1851
      accessXTimeoutValues : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1852
      enabledControls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1853
      perKeyRepeat : aliased xcb_xkb_get_controls_reply_t_perKeyRepeat_array;  -- /usr/include/xcb/xkb.h:1854
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_controls_reply_t);  -- /usr/include/xcb/xkb.h:1822

   type xcb_xkb_set_controls_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_controls_request_t_perKeyRepeat_array is array (0 .. 31) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_controls_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1864
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1865
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1866
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1867
      affectInternalRealMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1868
      internalRealMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1869
      affectIgnoreLockRealMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1870
      ignoreLockRealMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1871
      affectInternalVirtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1872
      internalVirtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1873
      affectIgnoreLockVirtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1874
      ignoreLockVirtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1875
      mouseKeysDfltBtn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1876
      groupsWrap : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1877
      accessXOptions : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1878
      pad0 : aliased xcb_xkb_set_controls_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:1879
      affectEnabledControls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1880
      enabledControls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1881
      changeControls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1882
      repeatDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1883
      repeatInterval : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1884
      slowKeysDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1885
      debounceDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1886
      mouseKeysDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1887
      mouseKeysInterval : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1888
      mouseKeysTimeToMax : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1889
      mouseKeysMaxSpeed : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1890
      mouseKeysCurve : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:1891
      accessXTimeout : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1892
      accessXTimeoutMask : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1893
      accessXTimeoutValues : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1894
      accessXTimeoutOptionsMask : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1895
      accessXTimeoutOptionsValues : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1896
      perKeyRepeat : aliased xcb_xkb_set_controls_request_t_perKeyRepeat_array;  -- /usr/include/xcb/xkb.h:1897
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_controls_request_t);  -- /usr/include/xcb/xkb.h:1863

   type xcb_xkb_get_map_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:1904
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_map_cookie_t);  -- /usr/include/xcb/xkb.h:1903

   type xcb_xkb_get_map_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_map_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1914
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1915
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1916
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:1917
      full : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1918
      partial : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1919
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1920
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1921
      firstKeySym : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1922
      nKeySyms : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1923
      firstKeyAction : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1924
      nKeyActions : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1925
      firstKeyBehavior : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1926
      nKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1927
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1928
      firstKeyExplicit : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1929
      nKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1930
      firstModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1931
      nModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1932
      firstVModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1933
      nVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1934
      pad0 : aliased xcb_xkb_get_map_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:1935
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_map_request_t);  -- /usr/include/xcb/xkb.h:1913

   type xcb_xkb_get_map_map_t is record
      types_rtrn : access xcb_xkb_key_type_t;  -- /usr/include/xcb/xkb.h:1942
      syms_rtrn : access xcb_xkb_key_sym_map_t;  -- /usr/include/xcb/xkb.h:1943
      acts_rtrn_count : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1944
      alignment_pad : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1945
      acts_rtrn_acts : access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:1946
      behaviors_rtrn : access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:1947
      vmods_rtrn : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1948
      alignment_pad2 : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1949
      explicit_rtrn : access xcb_xkb_set_explicit_t;  -- /usr/include/xcb/xkb.h:1950
      alignment_pad3 : access Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1951
      modmap_rtrn : access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:1952
      alignment_pad4 : access Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1953
      vmodmap_rtrn : access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:1954
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_map_map_t);  -- /usr/include/xcb/xkb.h:1941

   type xcb_xkb_get_map_reply_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_map_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1961
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1962
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1963
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:1964
      pad0 : aliased xcb_xkb_get_map_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:1965
      minKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1966
      maxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1967
      present : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1968
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1969
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1970
      totalTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1971
      firstKeySym : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1972
      totalSyms : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1973
      nKeySyms : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1974
      firstKeyAction : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1975
      totalActions : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1976
      nKeyActions : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1977
      firstKeyBehavior : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1978
      nKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1979
      totalKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1980
      firstKeyExplicit : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1981
      nKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1982
      totalKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1983
      firstModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1984
      nModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1985
      totalModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1986
      firstVModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:1987
      nVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1988
      totalVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1989
      pad1 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:1990
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:1991
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_map_reply_t);  -- /usr/include/xcb/xkb.h:1960

   type xcb_xkb_set_map_values_t is record
      types : access xcb_xkb_set_key_type_t;  -- /usr/include/xcb/xkb.h:1998
      syms : access xcb_xkb_key_sym_map_t;  -- /usr/include/xcb/xkb.h:1999
      actionsCount : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2000
      actions : access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:2001
      behaviors : access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:2002
      vmods : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2003
      modmap : access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:2005
      vmodmap : access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:2006
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_map_values_t);  -- /usr/include/xcb/xkb.h:1997

   type xcb_xkb_set_map_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2016
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2017
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2018
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2019
      present : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2020
      flags : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2021
      minKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2022
      maxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2023
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2024
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2025
      firstKeySym : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2026
      nKeySyms : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2027
      totalSyms : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2028
      firstKeyAction : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2029
      nKeyActions : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2030
      totalActions : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2031
      firstKeyBehavior : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2032
      nKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2033
      totalKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2034
      firstKeyExplicit : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2035
      nKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2036
      totalKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2037
      firstModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2038
      nModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2039
      totalModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2040
      firstVModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2041
      nVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2042
      totalVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2043
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2044
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_map_request_t);  -- /usr/include/xcb/xkb.h:2015

   type xcb_xkb_get_compat_map_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2051
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_compat_map_cookie_t);  -- /usr/include/xcb/xkb.h:2050

   type xcb_xkb_get_compat_map_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2061
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2062
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2063
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2064
      groups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2065
      getAllSI : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2066
      firstSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2067
      nSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2068
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_compat_map_request_t);  -- /usr/include/xcb/xkb.h:2060

   type xcb_xkb_get_compat_map_reply_t_pad1_array is array (0 .. 15) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_compat_map_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2075
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2076
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2077
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2078
      groupsRtrn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2079
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2080
      firstSIRtrn : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2081
      nSIRtrn : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2082
      nTotalSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2083
      pad1 : aliased xcb_xkb_get_compat_map_reply_t_pad1_array;  -- /usr/include/xcb/xkb.h:2084
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_compat_map_reply_t);  -- /usr/include/xcb/xkb.h:2074

   type xcb_xkb_set_compat_map_request_t_pad1_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_compat_map_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2094
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2095
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2096
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2097
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2098
      recomputeActions : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2099
      truncateSI : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2100
      groups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2101
      firstSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2102
      nSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2103
      pad1 : aliased xcb_xkb_set_compat_map_request_t_pad1_array;  -- /usr/include/xcb/xkb.h:2104
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_compat_map_request_t);  -- /usr/include/xcb/xkb.h:2093

   type xcb_xkb_get_indicator_state_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2111
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_indicator_state_cookie_t);  -- /usr/include/xcb/xkb.h:2110

   type xcb_xkb_get_indicator_state_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_indicator_state_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2121
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2122
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2123
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2124
      pad0 : aliased xcb_xkb_get_indicator_state_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2125
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_indicator_state_request_t);  -- /usr/include/xcb/xkb.h:2120

   type xcb_xkb_get_indicator_state_reply_t_pad0_array is array (0 .. 19) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_indicator_state_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2132
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2133
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2134
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2135
      state : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2136
      pad0 : aliased xcb_xkb_get_indicator_state_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2137
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_indicator_state_reply_t);  -- /usr/include/xcb/xkb.h:2131

   type xcb_xkb_get_indicator_map_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2144
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_indicator_map_cookie_t);  -- /usr/include/xcb/xkb.h:2143

   type xcb_xkb_get_indicator_map_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_indicator_map_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2154
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2155
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2156
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2157
      pad0 : aliased xcb_xkb_get_indicator_map_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2158
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2159
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_indicator_map_request_t);  -- /usr/include/xcb/xkb.h:2153

   type xcb_xkb_get_indicator_map_reply_t_pad0_array is array (0 .. 14) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_indicator_map_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2166
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2167
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2168
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2169
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2170
      realIndicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2171
      nIndicators : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2172
      pad0 : aliased xcb_xkb_get_indicator_map_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2173
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_indicator_map_reply_t);  -- /usr/include/xcb/xkb.h:2165

   type xcb_xkb_set_indicator_map_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_indicator_map_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2183
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2184
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2185
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2186
      pad0 : aliased xcb_xkb_set_indicator_map_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2187
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2188
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_indicator_map_request_t);  -- /usr/include/xcb/xkb.h:2182

   type xcb_xkb_get_named_indicator_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2195
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_named_indicator_cookie_t);  -- /usr/include/xcb/xkb.h:2194

   type xcb_xkb_get_named_indicator_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_named_indicator_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2205
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2206
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2207
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2208
      ledClass : aliased xcb_xkb_led_class_spec_t;  -- /usr/include/xcb/xkb.h:2209
      ledID : aliased xcb_xkb_id_spec_t;  -- /usr/include/xcb/xkb.h:2210
      pad0 : aliased xcb_xkb_get_named_indicator_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2211
      indicator : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2212
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_named_indicator_request_t);  -- /usr/include/xcb/xkb.h:2204

   type xcb_xkb_get_named_indicator_reply_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_named_indicator_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2219
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2220
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2221
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2222
      indicator : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2223
      found : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2224
      on : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2225
      realIndicator : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2226
      ndx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2227
      map_flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2228
      map_whichGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2229
      map_groups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2230
      map_whichMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2231
      map_mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2232
      map_realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2233
      map_vmod : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2234
      map_ctrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2235
      supported : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2236
      pad0 : aliased xcb_xkb_get_named_indicator_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2237
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_named_indicator_reply_t);  -- /usr/include/xcb/xkb.h:2218

   type xcb_xkb_set_named_indicator_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_named_indicator_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2247
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2248
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2249
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2250
      ledClass : aliased xcb_xkb_led_class_spec_t;  -- /usr/include/xcb/xkb.h:2251
      ledID : aliased xcb_xkb_id_spec_t;  -- /usr/include/xcb/xkb.h:2252
      pad0 : aliased xcb_xkb_set_named_indicator_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2253
      indicator : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2254
      setState : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2255
      on : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2256
      setMap : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2257
      createMap : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2258
      pad1 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2259
      map_flags : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2260
      map_whichGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2261
      map_groups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2262
      map_whichMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2263
      map_realMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2264
      map_vmods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2265
      map_ctrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2266
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_named_indicator_request_t);  -- /usr/include/xcb/xkb.h:2246

   type xcb_xkb_get_names_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2273
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_names_cookie_t);  -- /usr/include/xcb/xkb.h:2272

   type xcb_xkb_get_names_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_names_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2283
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2284
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2285
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2286
      pad0 : aliased xcb_xkb_get_names_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2287
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2288
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_names_request_t);  -- /usr/include/xcb/xkb.h:2282

   type xcb_xkb_get_names_value_list_t is record
      keycodesName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2295
      geometryName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2296
      symbolsName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2297
      physSymbolsName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2298
      typesName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2299
      compatName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2300
      typeNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2301
      nLevelsPerType : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2302
      alignment_pad : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2303
      ktLevelNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2304
      indicatorNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2305
      virtualModNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2306
      groups : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2307
      keyNames : access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:2308
      keyAliases : access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:2309
      radioGroupNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2310
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_names_value_list_t);  -- /usr/include/xcb/xkb.h:2294

   type xcb_xkb_get_names_reply_t_pad0_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_names_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2317
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2318
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2319
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2320
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2321
      minKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2322
      maxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2323
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2324
      groupNames : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2325
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2326
      firstKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2327
      nKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2328
      indicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2329
      nRadioGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2330
      nKeyAliases : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2331
      nKTLevels : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2332
      pad0 : aliased xcb_xkb_get_names_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2333
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_names_reply_t);  -- /usr/include/xcb/xkb.h:2316

   type xcb_xkb_set_names_values_t is record
      keycodesName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2340
      geometryName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2341
      symbolsName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2342
      physSymbolsName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2343
      typesName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2344
      compatName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2345
      typeNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2346
      nLevelsPerType : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2347
      ktLevelNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2348
      indicatorNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2349
      virtualModNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2350
      groups : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2351
      keyNames : access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:2352
      keyAliases : access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:2353
      radioGroupNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2354
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_names_values_t);  -- /usr/include/xcb/xkb.h:2339

   type xcb_xkb_set_names_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2364
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2365
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2366
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2367
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2368
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2369
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2370
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2371
      firstKTLevelt : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2372
      nKTLevels : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2373
      indicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2374
      groupNames : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2375
      nRadioGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2376
      firstKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2377
      nKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2378
      nKeyAliases : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2379
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2380
      totalKTLevelNames : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2381
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_names_request_t);  -- /usr/include/xcb/xkb.h:2363

   type xcb_xkb_per_client_flags_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2388
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_per_client_flags_cookie_t);  -- /usr/include/xcb/xkb.h:2387

   type xcb_xkb_per_client_flags_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_per_client_flags_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2398
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2399
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2400
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2401
      pad0 : aliased xcb_xkb_per_client_flags_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2402
      change : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2403
      value : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2404
      ctrlsToChange : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2405
      autoCtrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2406
      autoCtrlsValues : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2407
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_per_client_flags_request_t);  -- /usr/include/xcb/xkb.h:2397

   type xcb_xkb_per_client_flags_reply_t_pad0_array is array (0 .. 7) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_per_client_flags_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2414
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2415
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2416
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2417
      supported : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2418
      value : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2419
      autoCtrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2420
      autoCtrlsValues : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2421
      pad0 : aliased xcb_xkb_per_client_flags_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2422
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_per_client_flags_reply_t);  -- /usr/include/xcb/xkb.h:2413

   type xcb_xkb_list_components_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2429
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_list_components_cookie_t);  -- /usr/include/xcb/xkb.h:2428

   type xcb_xkb_list_components_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2439
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2440
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2441
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2442
      maxNames : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2443
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_list_components_request_t);  -- /usr/include/xcb/xkb.h:2438

   type xcb_xkb_list_components_reply_t_pad0_array is array (0 .. 9) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_list_components_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2450
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2451
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2452
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2453
      nKeymaps : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2454
      nKeycodes : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2455
      nTypes : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2456
      nCompatMaps : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2457
      nSymbols : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2458
      nGeometries : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2459
      extra : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2460
      pad0 : aliased xcb_xkb_list_components_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2461
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_list_components_reply_t);  -- /usr/include/xcb/xkb.h:2449

   type xcb_xkb_get_kbd_by_name_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2468
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_kbd_by_name_cookie_t);  -- /usr/include/xcb/xkb.h:2467

   type xcb_xkb_get_kbd_by_name_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2478
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2479
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2480
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2481
      need : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2482
      want : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2483
      load : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2484
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2485
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_kbd_by_name_request_t);  -- /usr/include/xcb/xkb.h:2477

   type xcb_xkb_get_kbd_by_name_replies_types_map_t is record
      types_rtrn : access xcb_xkb_key_type_t;  -- /usr/include/xcb/xkb.h:2492
      syms_rtrn : access xcb_xkb_key_sym_map_t;  -- /usr/include/xcb/xkb.h:2493
      acts_rtrn_count : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2494
      acts_rtrn_acts : access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:2495
      behaviors_rtrn : access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:2496
      vmods_rtrn : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2497
      explicit_rtrn : access xcb_xkb_set_explicit_t;  -- /usr/include/xcb/xkb.h:2498
      modmap_rtrn : access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:2499
      vmodmap_rtrn : access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:2500
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_kbd_by_name_replies_types_map_t);  -- /usr/include/xcb/xkb.h:2491

   type xcb_xkb_get_kbd_by_name_replies_key_names_value_list_t is record
      keycodesName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2507
      geometryName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2508
      symbolsName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2509
      physSymbolsName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2510
      typesName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2511
      compatName : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2512
      typeNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2513
      nLevelsPerType : access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2514
      ktLevelNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2515
      indicatorNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2516
      virtualModNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2517
      groups : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2518
      keyNames : access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:2519
      keyAliases : access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:2520
      radioGroupNames : access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2521
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_t);  -- /usr/include/xcb/xkb.h:2506

   type xcb_xkb_get_kbd_by_name_replies_t;
   type xcb_xkb_get_kbd_by_name_replies_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type u_types is record
      getmap_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2529
      typeDeviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2530
      getmap_sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2531
      getmap_length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2532
      pad0 : aliased xcb_xkb_get_kbd_by_name_replies_t_pad0_array;  -- /usr/include/xcb/xkb.h:2533
      typeMinKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2534
      typeMaxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2535
      present : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2536
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2537
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2538
      totalTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2539
      firstKeySym : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2540
      totalSyms : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2541
      nKeySyms : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2542
      firstKeyAction : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2543
      totalActions : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2544
      nKeyActions : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2545
      firstKeyBehavior : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2546
      nKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2547
      totalKeyBehaviors : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2548
      firstKeyExplicit : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2549
      nKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2550
      totalKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2551
      firstModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2552
      nModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2553
      totalModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2554
      firstVModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2555
      nVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2556
      totalVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2557
      pad1 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2558
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2559
      map : aliased xcb_xkb_get_kbd_by_name_replies_types_map_t;  -- /usr/include/xcb/xkb.h:2560
   end record;
   pragma Convention (C_Pass_By_Copy, u_types);
   type xcb_xkb_get_kbd_by_name_replies_t_pad1_array is array (0 .. 15) of aliased Libc.Stdint.uint8_t;
   type u_compat_map is record
      compatmap_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2563
      compatDeviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2564
      compatmap_sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2565
      compatmap_length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2566
      groupsRtrn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2567
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2568
      firstSIRtrn : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2569
      nSIRtrn : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2570
      nTotalSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2571
      pad1 : aliased xcb_xkb_get_kbd_by_name_replies_t_pad1_array;  -- /usr/include/xcb/xkb.h:2572
      si_rtrn : access xcb_xkb_sym_interpret_t;  -- /usr/include/xcb/xkb.h:2573
      group_rtrn : access xcb_xkb_mod_def_t;  -- /usr/include/xcb/xkb.h:2574
   end record;
   pragma Convention (C_Pass_By_Copy, u_compat_map);
   type Xcb_Xkb_Get_Kbd_By_Name_Replies_T_Pad0_Array_1 is array (0 .. 14) of aliased Libc.Stdint.uint8_t;
   type u_indicator_maps is record
      indicatormap_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2577
      indicatorDeviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2578
      indicatormap_sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2579
      indicatormap_length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2580
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2581
      realIndicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2582
      nIndicators : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2583
      pad0 : aliased Xcb_Xkb_Get_Kbd_By_Name_Replies_T_Pad0_Array_1;  -- /usr/include/xcb/xkb.h:2584
      maps : access xcb_xkb_indicator_map_t;  -- /usr/include/xcb/xkb.h:2585
   end record;
   pragma Convention (C_Pass_By_Copy, u_indicator_maps);
   type Xcb_Xkb_Get_Kbd_By_Name_Replies_T_Pad0_Array_2 is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type u_key_names is record
      keyname_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2588
      keyDeviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2589
      keyname_sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2590
      keyname_length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2591
      which : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2592
      keyMinKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2593
      keyMaxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2594
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2595
      groupNames : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2596
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2597
      firstKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2598
      nKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2599
      indicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2600
      nRadioGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2601
      nKeyAliases : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2602
      nKTLevels : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2603
      pad0 : aliased Xcb_Xkb_Get_Kbd_By_Name_Replies_T_Pad0_Array_2;  -- /usr/include/xcb/xkb.h:2604
      valueList : aliased xcb_xkb_get_kbd_by_name_replies_key_names_value_list_t;  -- /usr/include/xcb/xkb.h:2605
   end record;
   pragma Convention (C_Pass_By_Copy, u_key_names);
   type u_geometry is record
      geometry_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2608
      geometryDeviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2609
      geometry_sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2610
      geometry_length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2611
      name : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2612
      geometryFound : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2613
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2614
      widthMM : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2615
      heightMM : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2616
      nProperties : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2617
      nColors : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2618
      nShapes : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2619
      nSections : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2620
      nDoodads : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2621
      nKeyAliases : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2622
      baseColorNdx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2623
      labelColorNdx : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2624
      labelFont : access xcb_xkb_counted_string_16_t;  -- /usr/include/xcb/xkb.h:2625
   end record;
   pragma Convention (C_Pass_By_Copy, u_geometry);
   type xcb_xkb_get_kbd_by_name_replies_t is record
      types : aliased u_types;  -- /usr/include/xcb/xkb.h:2561
      compat_map : aliased u_compat_map;  -- /usr/include/xcb/xkb.h:2575
      indicator_maps : aliased u_indicator_maps;  -- /usr/include/xcb/xkb.h:2586
      key_names : aliased u_key_names;  -- /usr/include/xcb/xkb.h:2606
      geometry : aliased u_geometry;  -- /usr/include/xcb/xkb.h:2626
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_kbd_by_name_replies_t);  -- /usr/include/xcb/xkb.h:2527

   function xcb_xkb_get_kbd_by_name_replies_types_map (R : access xcb_xkb_get_kbd_by_name_replies_t) return access xcb_xkb_get_kbd_by_name_replies_types_map_t;  -- /usr/include/xcb/xkb.h:2640
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map, "xcb_xkb_get_kbd_by_name_replies_types_map");

   type xcb_xkb_get_kbd_by_name_reply_t_pad0_array is array (0 .. 15) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_kbd_by_name_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2646
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2647
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2648
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2649
      minKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2650
      maxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2651
      loaded : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2652
      newKeyboard : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2653
      found : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2654
      reported : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2655
      pad0 : aliased xcb_xkb_get_kbd_by_name_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2656
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_kbd_by_name_reply_t);  -- /usr/include/xcb/xkb.h:2645

   type xcb_xkb_get_device_info_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2663
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_device_info_cookie_t);  -- /usr/include/xcb/xkb.h:2662

   type xcb_xkb_get_device_info_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2673
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2674
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2675
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2676
      wanted : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2677
      allButtons : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2678
      firstButton : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2679
      nButtons : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2680
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2681
      ledClass : aliased xcb_xkb_led_class_spec_t;  -- /usr/include/xcb/xkb.h:2682
      ledID : aliased xcb_xkb_id_spec_t;  -- /usr/include/xcb/xkb.h:2683
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_device_info_request_t);  -- /usr/include/xcb/xkb.h:2672

   type xcb_xkb_get_device_info_reply_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_get_device_info_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2690
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2691
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2692
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2693
      present : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2694
      supported : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2695
      unsupported : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2696
      nDeviceLedFBs : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2697
      firstBtnWanted : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2698
      nBtnsWanted : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2699
      firstBtnRtrn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2700
      nBtnsRtrn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2701
      totalBtns : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2702
      hasOwnState : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2703
      dfltKbdFB : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2704
      dfltLedFB : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2705
      pad0 : aliased xcb_xkb_get_device_info_reply_t_pad0_array;  -- /usr/include/xcb/xkb.h:2706
      devType : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2707
      nameLen : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2708
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_get_device_info_reply_t);  -- /usr/include/xcb/xkb.h:2689

   type xcb_xkb_set_device_info_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2718
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2719
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2720
      deviceSpec : aliased xcb_xkb_device_spec_t;  -- /usr/include/xcb/xkb.h:2721
      firstBtn : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2722
      nBtns : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2723
      change : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2724
      nDeviceLedFBs : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2725
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_device_info_request_t);  -- /usr/include/xcb/xkb.h:2717

   type xcb_xkb_set_debugging_flags_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xkb.h:2732
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_debugging_flags_cookie_t);  -- /usr/include/xcb/xkb.h:2731

   type xcb_xkb_set_debugging_flags_request_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_debugging_flags_request_t is record
      major_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2742
      minor_opcode : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2743
      length : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2744
      msgLength : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2745
      pad0 : aliased xcb_xkb_set_debugging_flags_request_t_pad0_array;  -- /usr/include/xcb/xkb.h:2746
      affectFlags : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2747
      flags : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2748
      affectCtrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2749
      ctrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2750
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_debugging_flags_request_t);  -- /usr/include/xcb/xkb.h:2741

   type xcb_xkb_set_debugging_flags_reply_t_pad1_array is array (0 .. 7) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_set_debugging_flags_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2757
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2758
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2759
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2760
      currentFlags : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2761
      currentCtrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2762
      supportedFlags : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2763
      supportedCtrls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2764
      pad1 : aliased xcb_xkb_set_debugging_flags_reply_t_pad1_array;  -- /usr/include/xcb/xkb.h:2765
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_set_debugging_flags_reply_t);  -- /usr/include/xcb/xkb.h:2756

   type xcb_xkb_new_keyboard_notify_event_t_pad0_array is array (0 .. 13) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_new_keyboard_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2775
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2776
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2777
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2778
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2779
      oldDeviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2780
      minKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2781
      maxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2782
      oldMinKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2783
      oldMaxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2784
      requestMajor : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2785
      requestMinor : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2786
      changed : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2787
      pad0 : aliased xcb_xkb_new_keyboard_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2788
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_new_keyboard_notify_event_t);  -- /usr/include/xcb/xkb.h:2774

   type xcb_xkb_map_notify_event_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_map_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2798
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2799
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2800
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2801
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2802
      ptrBtnActions : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2803
      changed : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2804
      minKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2805
      maxKeyCode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2806
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2807
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2808
      firstKeySym : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2809
      nKeySyms : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2810
      firstKeyAct : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2811
      nKeyActs : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2812
      firstKeyBehavior : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2813
      nKeyBehavior : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2814
      firstKeyExplicit : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2815
      nKeyExplicit : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2816
      firstModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2817
      nModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2818
      firstVModMapKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2819
      nVModMapKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2820
      virtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2821
      pad0 : aliased xcb_xkb_map_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2822
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_map_notify_event_t);  -- /usr/include/xcb/xkb.h:2797

   type xcb_xkb_state_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2832
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2833
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2834
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2835
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2836
      mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2837
      baseMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2838
      latchedMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2839
      lockedMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2840
      group : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2841
      baseGroup : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:2842
      latchedGroup : aliased Libc.Stdint.int16_t;  -- /usr/include/xcb/xkb.h:2843
      lockedGroup : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2844
      compatState : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2845
      grabMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2846
      compatGrabMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2847
      lookupMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2848
      compatLoockupMods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2849
      ptrBtnState : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2850
      changed : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2851
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2852
      eventType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2853
      requestMajor : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2854
      requestMinor : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2855
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_state_notify_event_t);  -- /usr/include/xcb/xkb.h:2831

   type xcb_xkb_controls_notify_event_t_pad0_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_controls_notify_event_t_pad1_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_controls_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2865
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2866
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2867
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2868
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2869
      numGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2870
      pad0 : aliased xcb_xkb_controls_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2871
      changedControls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2872
      enabledControls : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2873
      enabledControlChanges : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2874
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2875
      eventType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2876
      requestMajor : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2877
      requestMinor : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2878
      pad1 : aliased xcb_xkb_controls_notify_event_t_pad1_array;  -- /usr/include/xcb/xkb.h:2879
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_controls_notify_event_t);  -- /usr/include/xcb/xkb.h:2864

   type xcb_xkb_indicator_state_notify_event_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_indicator_state_notify_event_t_pad1_array is array (0 .. 11) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_indicator_state_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2889
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2890
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2891
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2892
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2893
      pad0 : aliased xcb_xkb_indicator_state_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2894
      state : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2895
      stateChanged : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2896
      pad1 : aliased xcb_xkb_indicator_state_notify_event_t_pad1_array;  -- /usr/include/xcb/xkb.h:2897
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_indicator_state_notify_event_t);  -- /usr/include/xcb/xkb.h:2888

   type xcb_xkb_indicator_map_notify_event_t_pad0_array is array (0 .. 2) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_indicator_map_notify_event_t_pad1_array is array (0 .. 11) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_indicator_map_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2907
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2908
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2909
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2910
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2911
      pad0 : aliased xcb_xkb_indicator_map_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2912
      state : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2913
      mapChanged : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2914
      pad1 : aliased xcb_xkb_indicator_map_notify_event_t_pad1_array;  -- /usr/include/xcb/xkb.h:2915
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_indicator_map_notify_event_t);  -- /usr/include/xcb/xkb.h:2906

   type xcb_xkb_names_notify_event_t_pad2_array is array (0 .. 3) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_names_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2925
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2926
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2927
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2928
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2929
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2930
      changed : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2931
      firstType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2932
      nTypes : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2933
      firstLevelName : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2934
      nLevelNames : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2935
      pad1 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2936
      nRadioGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2937
      nKeyAliases : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2938
      changedGroupNames : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2939
      changedVirtualMods : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2940
      firstKey : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:2941
      nKeys : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2942
      changedIndicators : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:2943
      pad2 : aliased xcb_xkb_names_notify_event_t_pad2_array;  -- /usr/include/xcb/xkb.h:2944
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_names_notify_event_t);  -- /usr/include/xcb/xkb.h:2924

   type xcb_xkb_compat_map_notify_event_t_pad0_array is array (0 .. 15) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_compat_map_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2954
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2955
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2956
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2957
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2958
      changedGroups : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2959
      firstSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2960
      nSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2961
      nTotalSI : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2962
      pad0 : aliased xcb_xkb_compat_map_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2963
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_compat_map_notify_event_t);  -- /usr/include/xcb/xkb.h:2953

   type xcb_xkb_bell_notify_event_t_pad0_array is array (0 .. 6) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_bell_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2973
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2974
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2975
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2976
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2977
      bellClass : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2978
      bellID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2979
      percent : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2980
      pitch : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2981
      duration : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2982
      name : aliased XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:2983
      window : aliased XCB.XProto.xcb_window_t;  -- /usr/include/xcb/xkb.h:2984
      eventOnly : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2985
      pad0 : aliased xcb_xkb_bell_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:2986
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_bell_notify_event_t);  -- /usr/include/xcb/xkb.h:2972

   type xcb_xkb_action_message_event_t_message_array is array (0 .. 7) of aliased xcb_xkb_string8_t;
   type xcb_xkb_action_message_event_t_pad0_array is array (0 .. 9) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_action_message_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2996
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:2997
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:2998
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:2999
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3000
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:3001
      press : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3002
      keyEventFollows : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3003
      mods : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3004
      group : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3005
      message : aliased xcb_xkb_action_message_event_t_message_array;  -- /usr/include/xcb/xkb.h:3006
      pad0 : aliased xcb_xkb_action_message_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:3007
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_action_message_event_t);  -- /usr/include/xcb/xkb.h:2995

   type xcb_xkb_access_x_notify_event_t_pad0_array is array (0 .. 15) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_access_x_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3017
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3018
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3019
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:3020
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3021
      keycode : aliased XCB.XProto.xcb_keycode_t;  -- /usr/include/xcb/xkb.h:3022
      detailt : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3023
      slowKeysDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3024
      debounceDelay : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3025
      pad0 : aliased xcb_xkb_access_x_notify_event_t_pad0_array;  -- /usr/include/xcb/xkb.h:3026
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_access_x_notify_event_t);  -- /usr/include/xcb/xkb.h:3016

   type xcb_xkb_extension_device_notify_event_t_pad1_array is array (0 .. 1) of aliased Libc.Stdint.uint8_t;
   type xcb_xkb_extension_device_notify_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3036
      xkbType : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3037
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3038
      time : aliased XCB.XProto.xcb_timestamp_t;  -- /usr/include/xcb/xkb.h:3039
      deviceID : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3040
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3041
      reason : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3042
      ledClass : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3043
      ledID : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3044
      ledsDefined : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:3045
      ledState : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xkb.h:3046
      firstButton : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3047
      nButtons : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:3048
      supported : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3049
      unsupported : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:3050
      pad1 : aliased xcb_xkb_extension_device_notify_event_t_pad1_array;  -- /usr/include/xcb/xkb.h:3051
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_xkb_extension_device_notify_event_t);  -- /usr/include/xcb/xkb.h:3035

   procedure xcb_xkb_device_spec_next (i : access xcb_xkb_device_spec_iterator_t);  -- /usr/include/xcb/xkb.h:3073
   pragma Import (C, xcb_xkb_device_spec_next, "xcb_xkb_device_spec_next");

   function xcb_xkb_device_spec_end (i : xcb_xkb_device_spec_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3095
   pragma Import (C, xcb_xkb_device_spec_end, "xcb_xkb_device_spec_end");

   procedure xcb_xkb_led_class_spec_next (i : access xcb_xkb_led_class_spec_iterator_t);  -- /usr/include/xcb/xkb.h:3116
   pragma Import (C, xcb_xkb_led_class_spec_next, "xcb_xkb_led_class_spec_next");

   function xcb_xkb_led_class_spec_end (i : xcb_xkb_led_class_spec_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3138
   pragma Import (C, xcb_xkb_led_class_spec_end, "xcb_xkb_led_class_spec_end");

   procedure xcb_xkb_bell_class_spec_next (i : access xcb_xkb_bell_class_spec_iterator_t);  -- /usr/include/xcb/xkb.h:3159
   pragma Import (C, xcb_xkb_bell_class_spec_next, "xcb_xkb_bell_class_spec_next");

   function xcb_xkb_bell_class_spec_end (i : xcb_xkb_bell_class_spec_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3181
   pragma Import (C, xcb_xkb_bell_class_spec_end, "xcb_xkb_bell_class_spec_end");

   procedure xcb_xkb_id_spec_next (i : access xcb_xkb_id_spec_iterator_t);  -- /usr/include/xcb/xkb.h:3202
   pragma Import (C, xcb_xkb_id_spec_next, "xcb_xkb_id_spec_next");

   function xcb_xkb_id_spec_end (i : xcb_xkb_id_spec_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3224
   pragma Import (C, xcb_xkb_id_spec_end, "xcb_xkb_id_spec_end");

   procedure xcb_xkb_indicator_map_next (i : access xcb_xkb_indicator_map_iterator_t);  -- /usr/include/xcb/xkb.h:3245
   pragma Import (C, xcb_xkb_indicator_map_next, "xcb_xkb_indicator_map_next");

   function xcb_xkb_indicator_map_end (i : xcb_xkb_indicator_map_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3267
   pragma Import (C, xcb_xkb_indicator_map_end, "xcb_xkb_indicator_map_end");

   procedure xcb_xkb_mod_def_next (i : access xcb_xkb_mod_def_iterator_t);  -- /usr/include/xcb/xkb.h:3288
   pragma Import (C, xcb_xkb_mod_def_next, "xcb_xkb_mod_def_next");

   function xcb_xkb_mod_def_end (i : xcb_xkb_mod_def_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3310
   pragma Import (C, xcb_xkb_mod_def_end, "xcb_xkb_mod_def_end");

   procedure xcb_xkb_key_name_next (i : access xcb_xkb_key_name_iterator_t);  -- /usr/include/xcb/xkb.h:3331
   pragma Import (C, xcb_xkb_key_name_next, "xcb_xkb_key_name_next");

   function xcb_xkb_key_name_end (i : xcb_xkb_key_name_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3353
   pragma Import (C, xcb_xkb_key_name_end, "xcb_xkb_key_name_end");

   procedure xcb_xkb_key_alias_next (i : access xcb_xkb_key_alias_iterator_t);  -- /usr/include/xcb/xkb.h:3374
   pragma Import (C, xcb_xkb_key_alias_next, "xcb_xkb_key_alias_next");

   function xcb_xkb_key_alias_end (i : xcb_xkb_key_alias_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3396
   pragma Import (C, xcb_xkb_key_alias_end, "xcb_xkb_key_alias_end");

   function xcb_xkb_counted_string_16_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:3399
   pragma Import (C, xcb_xkb_counted_string_16_sizeof, "xcb_xkb_counted_string_16_sizeof");

   function xcb_xkb_counted_string_16_string (R : access xcb_xkb_counted_string_16_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xkb.h:3412
   pragma Import (C, xcb_xkb_counted_string_16_string, "xcb_xkb_counted_string_16_string");

   function xcb_xkb_counted_string_16_string_length (R : access xcb_xkb_counted_string_16_t) return int;  -- /usr/include/xcb/xkb.h:3425
   pragma Import (C, xcb_xkb_counted_string_16_string_length, "xcb_xkb_counted_string_16_string_length");

   function xcb_xkb_counted_string_16_string_end (R : access xcb_xkb_counted_string_16_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3438
   pragma Import (C, xcb_xkb_counted_string_16_string_end, "xcb_xkb_counted_string_16_string_end");

   function xcb_xkb_counted_string_16_alignment_pad (R : access xcb_xkb_counted_string_16_t) return System.Address;  -- /usr/include/xcb/xkb.h:3451
   pragma Import (C, xcb_xkb_counted_string_16_alignment_pad, "xcb_xkb_counted_string_16_alignment_pad");

   function xcb_xkb_counted_string_16_alignment_pad_length (R : access xcb_xkb_counted_string_16_t) return int;  -- /usr/include/xcb/xkb.h:3464
   pragma Import (C, xcb_xkb_counted_string_16_alignment_pad_length, "xcb_xkb_counted_string_16_alignment_pad_length");

   function xcb_xkb_counted_string_16_alignment_pad_end (R : access xcb_xkb_counted_string_16_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3477
   pragma Import (C, xcb_xkb_counted_string_16_alignment_pad_end, "xcb_xkb_counted_string_16_alignment_pad_end");

   procedure xcb_xkb_counted_string_16_next (i : access xcb_xkb_counted_string_16_iterator_t);  -- /usr/include/xcb/xkb.h:3498
   pragma Import (C, xcb_xkb_counted_string_16_next, "xcb_xkb_counted_string_16_next");

   function xcb_xkb_counted_string_16_end (i : xcb_xkb_counted_string_16_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3520
   pragma Import (C, xcb_xkb_counted_string_16_end, "xcb_xkb_counted_string_16_end");

   procedure xcb_xkb_kt_map_entry_next (i : access xcb_xkb_kt_map_entry_iterator_t);  -- /usr/include/xcb/xkb.h:3541
   pragma Import (C, xcb_xkb_kt_map_entry_next, "xcb_xkb_kt_map_entry_next");

   function xcb_xkb_kt_map_entry_end (i : xcb_xkb_kt_map_entry_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3563
   pragma Import (C, xcb_xkb_kt_map_entry_end, "xcb_xkb_kt_map_entry_end");

   function xcb_xkb_key_type_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:3566
   pragma Import (C, xcb_xkb_key_type_sizeof, "xcb_xkb_key_type_sizeof");

   function xcb_xkb_key_type_map (R : access xcb_xkb_key_type_t) return access xcb_xkb_kt_map_entry_t;  -- /usr/include/xcb/xkb.h:3579
   pragma Import (C, xcb_xkb_key_type_map, "xcb_xkb_key_type_map");

   function xcb_xkb_key_type_map_length (R : access xcb_xkb_key_type_t) return int;  -- /usr/include/xcb/xkb.h:3592
   pragma Import (C, xcb_xkb_key_type_map_length, "xcb_xkb_key_type_map_length");

   function xcb_xkb_key_type_map_iterator (R : access xcb_xkb_key_type_t) return xcb_xkb_kt_map_entry_iterator_t;  -- /usr/include/xcb/xkb.h:3605
   pragma Import (C, xcb_xkb_key_type_map_iterator, "xcb_xkb_key_type_map_iterator");

   function xcb_xkb_key_type_preserve (R : access xcb_xkb_key_type_t) return access xcb_xkb_mod_def_t;  -- /usr/include/xcb/xkb.h:3618
   pragma Import (C, xcb_xkb_key_type_preserve, "xcb_xkb_key_type_preserve");

   function xcb_xkb_key_type_preserve_length (R : access xcb_xkb_key_type_t) return int;  -- /usr/include/xcb/xkb.h:3631
   pragma Import (C, xcb_xkb_key_type_preserve_length, "xcb_xkb_key_type_preserve_length");

   function xcb_xkb_key_type_preserve_iterator (R : access xcb_xkb_key_type_t) return xcb_xkb_mod_def_iterator_t;  -- /usr/include/xcb/xkb.h:3644
   pragma Import (C, xcb_xkb_key_type_preserve_iterator, "xcb_xkb_key_type_preserve_iterator");

   procedure xcb_xkb_key_type_next (i : access xcb_xkb_key_type_iterator_t);  -- /usr/include/xcb/xkb.h:3665
   pragma Import (C, xcb_xkb_key_type_next, "xcb_xkb_key_type_next");

   function xcb_xkb_key_type_end (i : xcb_xkb_key_type_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3687
   pragma Import (C, xcb_xkb_key_type_end, "xcb_xkb_key_type_end");

   function xcb_xkb_key_sym_map_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:3690
   pragma Import (C, xcb_xkb_key_sym_map_sizeof, "xcb_xkb_key_sym_map_sizeof");

   function xcb_xkb_key_sym_map_syms (R : access xcb_xkb_key_sym_map_t) return access XCB.XProto.xcb_keysym_t;  -- /usr/include/xcb/xkb.h:3703
   pragma Import (C, xcb_xkb_key_sym_map_syms, "xcb_xkb_key_sym_map_syms");

   function xcb_xkb_key_sym_map_syms_length (R : access xcb_xkb_key_sym_map_t) return int;  -- /usr/include/xcb/xkb.h:3716
   pragma Import (C, xcb_xkb_key_sym_map_syms_length, "xcb_xkb_key_sym_map_syms_length");

   function xcb_xkb_key_sym_map_syms_end (R : access xcb_xkb_key_sym_map_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3729
   pragma Import (C, xcb_xkb_key_sym_map_syms_end, "xcb_xkb_key_sym_map_syms_end");

   procedure xcb_xkb_key_sym_map_next (i : access xcb_xkb_key_sym_map_iterator_t);  -- /usr/include/xcb/xkb.h:3750
   pragma Import (C, xcb_xkb_key_sym_map_next, "xcb_xkb_key_sym_map_next");

   function xcb_xkb_key_sym_map_end (i : xcb_xkb_key_sym_map_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3772
   pragma Import (C, xcb_xkb_key_sym_map_end, "xcb_xkb_key_sym_map_end");

   procedure xcb_xkb_common_behavior_next (i : access xcb_xkb_common_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:3793
   pragma Import (C, xcb_xkb_common_behavior_next, "xcb_xkb_common_behavior_next");

   function xcb_xkb_common_behavior_end (i : xcb_xkb_common_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3815
   pragma Import (C, xcb_xkb_common_behavior_end, "xcb_xkb_common_behavior_end");

   procedure xcb_xkb_default_behavior_next (i : access xcb_xkb_default_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:3836
   pragma Import (C, xcb_xkb_default_behavior_next, "xcb_xkb_default_behavior_next");

   function xcb_xkb_default_behavior_end (i : xcb_xkb_default_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3858
   pragma Import (C, xcb_xkb_default_behavior_end, "xcb_xkb_default_behavior_end");

   procedure xcb_xkb_lock_behavior_next (i : access xcb_xkb_lock_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:3879
   pragma Import (C, xcb_xkb_lock_behavior_next, "xcb_xkb_lock_behavior_next");

   function xcb_xkb_lock_behavior_end (i : xcb_xkb_lock_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3901
   pragma Import (C, xcb_xkb_lock_behavior_end, "xcb_xkb_lock_behavior_end");

   procedure xcb_xkb_radio_group_behavior_next (i : access xcb_xkb_radio_group_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:3922
   pragma Import (C, xcb_xkb_radio_group_behavior_next, "xcb_xkb_radio_group_behavior_next");

   function xcb_xkb_radio_group_behavior_end (i : xcb_xkb_radio_group_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3944
   pragma Import (C, xcb_xkb_radio_group_behavior_end, "xcb_xkb_radio_group_behavior_end");

   procedure xcb_xkb_overlay_behavior_next (i : access xcb_xkb_overlay_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:3965
   pragma Import (C, xcb_xkb_overlay_behavior_next, "xcb_xkb_overlay_behavior_next");

   function xcb_xkb_overlay_behavior_end (i : xcb_xkb_overlay_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:3987
   pragma Import (C, xcb_xkb_overlay_behavior_end, "xcb_xkb_overlay_behavior_end");

   procedure xcb_xkb_permament_lock_behavior_next (i : access xcb_xkb_permament_lock_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:4008
   pragma Import (C, xcb_xkb_permament_lock_behavior_next, "xcb_xkb_permament_lock_behavior_next");

   function xcb_xkb_permament_lock_behavior_end (i : xcb_xkb_permament_lock_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4030
   pragma Import (C, xcb_xkb_permament_lock_behavior_end, "xcb_xkb_permament_lock_behavior_end");

   procedure xcb_xkb_permament_radio_group_behavior_next (i : access xcb_xkb_permament_radio_group_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:4051
   pragma Import (C, xcb_xkb_permament_radio_group_behavior_next, "xcb_xkb_permament_radio_group_behavior_next");

   function xcb_xkb_permament_radio_group_behavior_end (i : xcb_xkb_permament_radio_group_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4073
   pragma Import (C, xcb_xkb_permament_radio_group_behavior_end, "xcb_xkb_permament_radio_group_behavior_end");

   procedure xcb_xkb_permament_overlay_behavior_next (i : access xcb_xkb_permament_overlay_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:4094
   pragma Import (C, xcb_xkb_permament_overlay_behavior_next, "xcb_xkb_permament_overlay_behavior_next");

   function xcb_xkb_permament_overlay_behavior_end (i : xcb_xkb_permament_overlay_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4116
   pragma Import (C, xcb_xkb_permament_overlay_behavior_end, "xcb_xkb_permament_overlay_behavior_end");

   procedure xcb_xkb_behavior_next (i : access xcb_xkb_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:4137
   pragma Import (C, xcb_xkb_behavior_next, "xcb_xkb_behavior_next");

   function xcb_xkb_behavior_end (i : xcb_xkb_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4159
   pragma Import (C, xcb_xkb_behavior_end, "xcb_xkb_behavior_end");

   procedure xcb_xkb_set_behavior_next (i : access xcb_xkb_set_behavior_iterator_t);  -- /usr/include/xcb/xkb.h:4180
   pragma Import (C, xcb_xkb_set_behavior_next, "xcb_xkb_set_behavior_next");

   function xcb_xkb_set_behavior_end (i : xcb_xkb_set_behavior_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4202
   pragma Import (C, xcb_xkb_set_behavior_end, "xcb_xkb_set_behavior_end");

   procedure xcb_xkb_set_explicit_next (i : access xcb_xkb_set_explicit_iterator_t);  -- /usr/include/xcb/xkb.h:4223
   pragma Import (C, xcb_xkb_set_explicit_next, "xcb_xkb_set_explicit_next");

   function xcb_xkb_set_explicit_end (i : xcb_xkb_set_explicit_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4245
   pragma Import (C, xcb_xkb_set_explicit_end, "xcb_xkb_set_explicit_end");

   procedure xcb_xkb_key_mod_map_next (i : access xcb_xkb_key_mod_map_iterator_t);  -- /usr/include/xcb/xkb.h:4266
   pragma Import (C, xcb_xkb_key_mod_map_next, "xcb_xkb_key_mod_map_next");

   function xcb_xkb_key_mod_map_end (i : xcb_xkb_key_mod_map_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4288
   pragma Import (C, xcb_xkb_key_mod_map_end, "xcb_xkb_key_mod_map_end");

   procedure xcb_xkb_key_v_mod_map_next (i : access xcb_xkb_key_v_mod_map_iterator_t);  -- /usr/include/xcb/xkb.h:4309
   pragma Import (C, xcb_xkb_key_v_mod_map_next, "xcb_xkb_key_v_mod_map_next");

   function xcb_xkb_key_v_mod_map_end (i : xcb_xkb_key_v_mod_map_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4331
   pragma Import (C, xcb_xkb_key_v_mod_map_end, "xcb_xkb_key_v_mod_map_end");

   procedure xcb_xkb_kt_set_map_entry_next (i : access xcb_xkb_kt_set_map_entry_iterator_t);  -- /usr/include/xcb/xkb.h:4352
   pragma Import (C, xcb_xkb_kt_set_map_entry_next, "xcb_xkb_kt_set_map_entry_next");

   function xcb_xkb_kt_set_map_entry_end (i : xcb_xkb_kt_set_map_entry_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4374
   pragma Import (C, xcb_xkb_kt_set_map_entry_end, "xcb_xkb_kt_set_map_entry_end");

   function xcb_xkb_set_key_type_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:4377
   pragma Import (C, xcb_xkb_set_key_type_sizeof, "xcb_xkb_set_key_type_sizeof");

   function xcb_xkb_set_key_type_entries (R : access xcb_xkb_set_key_type_t) return access xcb_xkb_kt_set_map_entry_t;  -- /usr/include/xcb/xkb.h:4390
   pragma Import (C, xcb_xkb_set_key_type_entries, "xcb_xkb_set_key_type_entries");

   function xcb_xkb_set_key_type_entries_length (R : access xcb_xkb_set_key_type_t) return int;  -- /usr/include/xcb/xkb.h:4403
   pragma Import (C, xcb_xkb_set_key_type_entries_length, "xcb_xkb_set_key_type_entries_length");

   function xcb_xkb_set_key_type_entries_iterator (R : access xcb_xkb_set_key_type_t) return xcb_xkb_kt_set_map_entry_iterator_t;  -- /usr/include/xcb/xkb.h:4416
   pragma Import (C, xcb_xkb_set_key_type_entries_iterator, "xcb_xkb_set_key_type_entries_iterator");

   function xcb_xkb_set_key_type_preserve_entries (R : access xcb_xkb_set_key_type_t) return access xcb_xkb_kt_set_map_entry_t;  -- /usr/include/xcb/xkb.h:4429
   pragma Import (C, xcb_xkb_set_key_type_preserve_entries, "xcb_xkb_set_key_type_preserve_entries");

   function xcb_xkb_set_key_type_preserve_entries_length (R : access xcb_xkb_set_key_type_t) return int;  -- /usr/include/xcb/xkb.h:4442
   pragma Import (C, xcb_xkb_set_key_type_preserve_entries_length, "xcb_xkb_set_key_type_preserve_entries_length");

   function xcb_xkb_set_key_type_preserve_entries_iterator (R : access xcb_xkb_set_key_type_t) return xcb_xkb_kt_set_map_entry_iterator_t;  -- /usr/include/xcb/xkb.h:4455
   pragma Import (C, xcb_xkb_set_key_type_preserve_entries_iterator, "xcb_xkb_set_key_type_preserve_entries_iterator");

   procedure xcb_xkb_set_key_type_next (i : access xcb_xkb_set_key_type_iterator_t);  -- /usr/include/xcb/xkb.h:4476
   pragma Import (C, xcb_xkb_set_key_type_next, "xcb_xkb_set_key_type_next");

   function xcb_xkb_set_key_type_end (i : xcb_xkb_set_key_type_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4498
   pragma Import (C, xcb_xkb_set_key_type_end, "xcb_xkb_set_key_type_end");

   procedure xcb_xkb_string8_next (i : access xcb_xkb_string8_iterator_t);  -- /usr/include/xcb/xkb.h:4519
   pragma Import (C, xcb_xkb_string8_next, "xcb_xkb_string8_next");

   function xcb_xkb_string8_end (i : xcb_xkb_string8_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4541
   pragma Import (C, xcb_xkb_string8_end, "xcb_xkb_string8_end");

   function xcb_xkb_outline_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:4544
   pragma Import (C, xcb_xkb_outline_sizeof, "xcb_xkb_outline_sizeof");

   function xcb_xkb_outline_points (R : access xcb_xkb_outline_t) return access XCB.XProto.xcb_point_t;  -- /usr/include/xcb/xkb.h:4557
   pragma Import (C, xcb_xkb_outline_points, "xcb_xkb_outline_points");

   function xcb_xkb_outline_points_length (R : access xcb_xkb_outline_t) return int;  -- /usr/include/xcb/xkb.h:4570
   pragma Import (C, xcb_xkb_outline_points_length, "xcb_xkb_outline_points_length");

   function xcb_xkb_outline_points_iterator (R : access xcb_xkb_outline_t) return XCB.XProto.xcb_point_iterator_t;  -- /usr/include/xcb/xkb.h:4583
   pragma Import (C, xcb_xkb_outline_points_iterator, "xcb_xkb_outline_points_iterator");

   procedure xcb_xkb_outline_next (i : access xcb_xkb_outline_iterator_t);  -- /usr/include/xcb/xkb.h:4604
   pragma Import (C, xcb_xkb_outline_next, "xcb_xkb_outline_next");

   function xcb_xkb_outline_end (i : xcb_xkb_outline_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4626
   pragma Import (C, xcb_xkb_outline_end, "xcb_xkb_outline_end");

   function xcb_xkb_shape_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:4629
   pragma Import (C, xcb_xkb_shape_sizeof, "xcb_xkb_shape_sizeof");

   function xcb_xkb_shape_outlines_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:4642
   pragma Import (C, xcb_xkb_shape_outlines_length, "xcb_xkb_shape_outlines_length");

   function xcb_xkb_shape_outlines_iterator (R : System.Address) return xcb_xkb_outline_iterator_t;  -- /usr/include/xcb/xkb.h:4655
   pragma Import (C, xcb_xkb_shape_outlines_iterator, "xcb_xkb_shape_outlines_iterator");

   procedure xcb_xkb_shape_next (i : access xcb_xkb_shape_iterator_t);  -- /usr/include/xcb/xkb.h:4676
   pragma Import (C, xcb_xkb_shape_next, "xcb_xkb_shape_next");

   function xcb_xkb_shape_end (i : xcb_xkb_shape_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4698
   pragma Import (C, xcb_xkb_shape_end, "xcb_xkb_shape_end");

   procedure xcb_xkb_key_next (i : access xcb_xkb_key_iterator_t);  -- /usr/include/xcb/xkb.h:4719
   pragma Import (C, xcb_xkb_key_next, "xcb_xkb_key_next");

   function xcb_xkb_key_end (i : xcb_xkb_key_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4741
   pragma Import (C, xcb_xkb_key_end, "xcb_xkb_key_end");

   procedure xcb_xkb_overlay_key_next (i : access xcb_xkb_overlay_key_iterator_t);  -- /usr/include/xcb/xkb.h:4762
   pragma Import (C, xcb_xkb_overlay_key_next, "xcb_xkb_overlay_key_next");

   function xcb_xkb_overlay_key_end (i : xcb_xkb_overlay_key_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4784
   pragma Import (C, xcb_xkb_overlay_key_end, "xcb_xkb_overlay_key_end");

   function xcb_xkb_overlay_row_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:4787
   pragma Import (C, xcb_xkb_overlay_row_sizeof, "xcb_xkb_overlay_row_sizeof");

   function xcb_xkb_overlay_row_keys (R : System.Address) return access xcb_xkb_overlay_key_t;  -- /usr/include/xcb/xkb.h:4800
   pragma Import (C, xcb_xkb_overlay_row_keys, "xcb_xkb_overlay_row_keys");

   function xcb_xkb_overlay_row_keys_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:4813
   pragma Import (C, xcb_xkb_overlay_row_keys_length, "xcb_xkb_overlay_row_keys_length");

   function xcb_xkb_overlay_row_keys_iterator (R : System.Address) return xcb_xkb_overlay_key_iterator_t;  -- /usr/include/xcb/xkb.h:4826
   pragma Import (C, xcb_xkb_overlay_row_keys_iterator, "xcb_xkb_overlay_row_keys_iterator");

   procedure xcb_xkb_overlay_row_next (i : access xcb_xkb_overlay_row_iterator_t);  -- /usr/include/xcb/xkb.h:4847
   pragma Import (C, xcb_xkb_overlay_row_next, "xcb_xkb_overlay_row_next");

   function xcb_xkb_overlay_row_end (i : xcb_xkb_overlay_row_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4869
   pragma Import (C, xcb_xkb_overlay_row_end, "xcb_xkb_overlay_row_end");

   function xcb_xkb_overlay_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:4872
   pragma Import (C, xcb_xkb_overlay_sizeof, "xcb_xkb_overlay_sizeof");

   function xcb_xkb_overlay_rows_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:4885
   pragma Import (C, xcb_xkb_overlay_rows_length, "xcb_xkb_overlay_rows_length");

   function xcb_xkb_overlay_rows_iterator (R : System.Address) return xcb_xkb_overlay_row_iterator_t;  -- /usr/include/xcb/xkb.h:4898
   pragma Import (C, xcb_xkb_overlay_rows_iterator, "xcb_xkb_overlay_rows_iterator");

   procedure xcb_xkb_overlay_next (i : access xcb_xkb_overlay_iterator_t);  -- /usr/include/xcb/xkb.h:4919
   pragma Import (C, xcb_xkb_overlay_next, "xcb_xkb_overlay_next");

   function xcb_xkb_overlay_end (i : xcb_xkb_overlay_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:4941
   pragma Import (C, xcb_xkb_overlay_end, "xcb_xkb_overlay_end");

   function xcb_xkb_row_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:4944
   pragma Import (C, xcb_xkb_row_sizeof, "xcb_xkb_row_sizeof");

   function xcb_xkb_row_keys (R : System.Address) return access xcb_xkb_key_t;  -- /usr/include/xcb/xkb.h:4957
   pragma Import (C, xcb_xkb_row_keys, "xcb_xkb_row_keys");

   function xcb_xkb_row_keys_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:4970
   pragma Import (C, xcb_xkb_row_keys_length, "xcb_xkb_row_keys_length");

   function xcb_xkb_row_keys_iterator (R : System.Address) return xcb_xkb_key_iterator_t;  -- /usr/include/xcb/xkb.h:4983
   pragma Import (C, xcb_xkb_row_keys_iterator, "xcb_xkb_row_keys_iterator");

   procedure xcb_xkb_row_next (i : access xcb_xkb_row_iterator_t);  -- /usr/include/xcb/xkb.h:5004
   pragma Import (C, xcb_xkb_row_next, "xcb_xkb_row_next");

   function xcb_xkb_row_end (i : xcb_xkb_row_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5026
   pragma Import (C, xcb_xkb_row_end, "xcb_xkb_row_end");

   function xcb_xkb_listing_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:5029
   pragma Import (C, xcb_xkb_listing_sizeof, "xcb_xkb_listing_sizeof");

   function xcb_xkb_listing_string (R : System.Address) return access xcb_xkb_string8_t;  -- /usr/include/xcb/xkb.h:5042
   pragma Import (C, xcb_xkb_listing_string, "xcb_xkb_listing_string");

   function xcb_xkb_listing_string_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:5055
   pragma Import (C, xcb_xkb_listing_string_length, "xcb_xkb_listing_string_length");

   function xcb_xkb_listing_string_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5068
   pragma Import (C, xcb_xkb_listing_string_end, "xcb_xkb_listing_string_end");

   procedure xcb_xkb_listing_next (i : access xcb_xkb_listing_iterator_t);  -- /usr/include/xcb/xkb.h:5089
   pragma Import (C, xcb_xkb_listing_next, "xcb_xkb_listing_next");

   function xcb_xkb_listing_end (i : xcb_xkb_listing_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5111
   pragma Import (C, xcb_xkb_listing_end, "xcb_xkb_listing_end");

   function xcb_xkb_device_led_info_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:5114
   pragma Import (C, xcb_xkb_device_led_info_sizeof, "xcb_xkb_device_led_info_sizeof");

   function xcb_xkb_device_led_info_names (R : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:5127
   pragma Import (C, xcb_xkb_device_led_info_names, "xcb_xkb_device_led_info_names");

   function xcb_xkb_device_led_info_names_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:5140
   pragma Import (C, xcb_xkb_device_led_info_names_length, "xcb_xkb_device_led_info_names_length");

   function xcb_xkb_device_led_info_names_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5153
   pragma Import (C, xcb_xkb_device_led_info_names_end, "xcb_xkb_device_led_info_names_end");

   function xcb_xkb_device_led_info_maps (R : System.Address) return access xcb_xkb_indicator_map_t;  -- /usr/include/xcb/xkb.h:5166
   pragma Import (C, xcb_xkb_device_led_info_maps, "xcb_xkb_device_led_info_maps");

   function xcb_xkb_device_led_info_maps_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:5179
   pragma Import (C, xcb_xkb_device_led_info_maps_length, "xcb_xkb_device_led_info_maps_length");

   function xcb_xkb_device_led_info_maps_iterator (R : System.Address) return xcb_xkb_indicator_map_iterator_t;  -- /usr/include/xcb/xkb.h:5192
   pragma Import (C, xcb_xkb_device_led_info_maps_iterator, "xcb_xkb_device_led_info_maps_iterator");

   procedure xcb_xkb_device_led_info_next (i : access xcb_xkb_device_led_info_iterator_t);  -- /usr/include/xcb/xkb.h:5213
   pragma Import (C, xcb_xkb_device_led_info_next, "xcb_xkb_device_led_info_next");

   function xcb_xkb_device_led_info_end (i : xcb_xkb_device_led_info_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5235
   pragma Import (C, xcb_xkb_device_led_info_end, "xcb_xkb_device_led_info_end");

   procedure xcb_xkb_sa_no_action_next (i : access xcb_xkb_sa_no_action_iterator_t);  -- /usr/include/xcb/xkb.h:5256
   pragma Import (C, xcb_xkb_sa_no_action_next, "xcb_xkb_sa_no_action_next");

   function xcb_xkb_sa_no_action_end (i : xcb_xkb_sa_no_action_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5278
   pragma Import (C, xcb_xkb_sa_no_action_end, "xcb_xkb_sa_no_action_end");

   procedure xcb_xkb_sa_set_mods_next (i : access xcb_xkb_sa_set_mods_iterator_t);  -- /usr/include/xcb/xkb.h:5299
   pragma Import (C, xcb_xkb_sa_set_mods_next, "xcb_xkb_sa_set_mods_next");

   function xcb_xkb_sa_set_mods_end (i : xcb_xkb_sa_set_mods_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5321
   pragma Import (C, xcb_xkb_sa_set_mods_end, "xcb_xkb_sa_set_mods_end");

   procedure xcb_xkb_sa_latch_mods_next (i : access xcb_xkb_sa_latch_mods_iterator_t);  -- /usr/include/xcb/xkb.h:5342
   pragma Import (C, xcb_xkb_sa_latch_mods_next, "xcb_xkb_sa_latch_mods_next");

   function xcb_xkb_sa_latch_mods_end (i : xcb_xkb_sa_latch_mods_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5364
   pragma Import (C, xcb_xkb_sa_latch_mods_end, "xcb_xkb_sa_latch_mods_end");

   procedure xcb_xkb_sa_lock_mods_next (i : access xcb_xkb_sa_lock_mods_iterator_t);  -- /usr/include/xcb/xkb.h:5385
   pragma Import (C, xcb_xkb_sa_lock_mods_next, "xcb_xkb_sa_lock_mods_next");

   function xcb_xkb_sa_lock_mods_end (i : xcb_xkb_sa_lock_mods_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5407
   pragma Import (C, xcb_xkb_sa_lock_mods_end, "xcb_xkb_sa_lock_mods_end");

   procedure xcb_xkb_sa_set_group_next (i : access xcb_xkb_sa_set_group_iterator_t);  -- /usr/include/xcb/xkb.h:5428
   pragma Import (C, xcb_xkb_sa_set_group_next, "xcb_xkb_sa_set_group_next");

   function xcb_xkb_sa_set_group_end (i : xcb_xkb_sa_set_group_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5450
   pragma Import (C, xcb_xkb_sa_set_group_end, "xcb_xkb_sa_set_group_end");

   procedure xcb_xkb_sa_latch_group_next (i : access xcb_xkb_sa_latch_group_iterator_t);  -- /usr/include/xcb/xkb.h:5471
   pragma Import (C, xcb_xkb_sa_latch_group_next, "xcb_xkb_sa_latch_group_next");

   function xcb_xkb_sa_latch_group_end (i : xcb_xkb_sa_latch_group_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5493
   pragma Import (C, xcb_xkb_sa_latch_group_end, "xcb_xkb_sa_latch_group_end");

   procedure xcb_xkb_sa_lock_group_next (i : access xcb_xkb_sa_lock_group_iterator_t);  -- /usr/include/xcb/xkb.h:5514
   pragma Import (C, xcb_xkb_sa_lock_group_next, "xcb_xkb_sa_lock_group_next");

   function xcb_xkb_sa_lock_group_end (i : xcb_xkb_sa_lock_group_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5536
   pragma Import (C, xcb_xkb_sa_lock_group_end, "xcb_xkb_sa_lock_group_end");

   procedure xcb_xkb_sa_move_ptr_next (i : access xcb_xkb_sa_move_ptr_iterator_t);  -- /usr/include/xcb/xkb.h:5557
   pragma Import (C, xcb_xkb_sa_move_ptr_next, "xcb_xkb_sa_move_ptr_next");

   function xcb_xkb_sa_move_ptr_end (i : xcb_xkb_sa_move_ptr_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5579
   pragma Import (C, xcb_xkb_sa_move_ptr_end, "xcb_xkb_sa_move_ptr_end");

   procedure xcb_xkb_sa_ptr_btn_next (i : access xcb_xkb_sa_ptr_btn_iterator_t);  -- /usr/include/xcb/xkb.h:5600
   pragma Import (C, xcb_xkb_sa_ptr_btn_next, "xcb_xkb_sa_ptr_btn_next");

   function xcb_xkb_sa_ptr_btn_end (i : xcb_xkb_sa_ptr_btn_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5622
   pragma Import (C, xcb_xkb_sa_ptr_btn_end, "xcb_xkb_sa_ptr_btn_end");

   procedure xcb_xkb_sa_lock_ptr_btn_next (i : access xcb_xkb_sa_lock_ptr_btn_iterator_t);  -- /usr/include/xcb/xkb.h:5643
   pragma Import (C, xcb_xkb_sa_lock_ptr_btn_next, "xcb_xkb_sa_lock_ptr_btn_next");

   function xcb_xkb_sa_lock_ptr_btn_end (i : xcb_xkb_sa_lock_ptr_btn_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5665
   pragma Import (C, xcb_xkb_sa_lock_ptr_btn_end, "xcb_xkb_sa_lock_ptr_btn_end");

   procedure xcb_xkb_sa_set_ptr_dflt_next (i : access xcb_xkb_sa_set_ptr_dflt_iterator_t);  -- /usr/include/xcb/xkb.h:5686
   pragma Import (C, xcb_xkb_sa_set_ptr_dflt_next, "xcb_xkb_sa_set_ptr_dflt_next");

   function xcb_xkb_sa_set_ptr_dflt_end (i : xcb_xkb_sa_set_ptr_dflt_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5708
   pragma Import (C, xcb_xkb_sa_set_ptr_dflt_end, "xcb_xkb_sa_set_ptr_dflt_end");

   procedure xcb_xkb_sa_iso_lock_next (i : access xcb_xkb_sa_iso_lock_iterator_t);  -- /usr/include/xcb/xkb.h:5729
   pragma Import (C, xcb_xkb_sa_iso_lock_next, "xcb_xkb_sa_iso_lock_next");

   function xcb_xkb_sa_iso_lock_end (i : xcb_xkb_sa_iso_lock_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5751
   pragma Import (C, xcb_xkb_sa_iso_lock_end, "xcb_xkb_sa_iso_lock_end");

   procedure xcb_xkb_sa_terminate_next (i : access xcb_xkb_sa_terminate_iterator_t);  -- /usr/include/xcb/xkb.h:5772
   pragma Import (C, xcb_xkb_sa_terminate_next, "xcb_xkb_sa_terminate_next");

   function xcb_xkb_sa_terminate_end (i : xcb_xkb_sa_terminate_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5794
   pragma Import (C, xcb_xkb_sa_terminate_end, "xcb_xkb_sa_terminate_end");

   procedure xcb_xkb_sa_switch_screen_next (i : access xcb_xkb_sa_switch_screen_iterator_t);  -- /usr/include/xcb/xkb.h:5815
   pragma Import (C, xcb_xkb_sa_switch_screen_next, "xcb_xkb_sa_switch_screen_next");

   function xcb_xkb_sa_switch_screen_end (i : xcb_xkb_sa_switch_screen_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5837
   pragma Import (C, xcb_xkb_sa_switch_screen_end, "xcb_xkb_sa_switch_screen_end");

   procedure xcb_xkb_sa_set_controls_next (i : access xcb_xkb_sa_set_controls_iterator_t);  -- /usr/include/xcb/xkb.h:5858
   pragma Import (C, xcb_xkb_sa_set_controls_next, "xcb_xkb_sa_set_controls_next");

   function xcb_xkb_sa_set_controls_end (i : xcb_xkb_sa_set_controls_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5880
   pragma Import (C, xcb_xkb_sa_set_controls_end, "xcb_xkb_sa_set_controls_end");

   procedure xcb_xkb_sa_lock_controls_next (i : access xcb_xkb_sa_lock_controls_iterator_t);  -- /usr/include/xcb/xkb.h:5901
   pragma Import (C, xcb_xkb_sa_lock_controls_next, "xcb_xkb_sa_lock_controls_next");

   function xcb_xkb_sa_lock_controls_end (i : xcb_xkb_sa_lock_controls_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5923
   pragma Import (C, xcb_xkb_sa_lock_controls_end, "xcb_xkb_sa_lock_controls_end");

   procedure xcb_xkb_sa_action_message_next (i : access xcb_xkb_sa_action_message_iterator_t);  -- /usr/include/xcb/xkb.h:5944
   pragma Import (C, xcb_xkb_sa_action_message_next, "xcb_xkb_sa_action_message_next");

   function xcb_xkb_sa_action_message_end (i : xcb_xkb_sa_action_message_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:5966
   pragma Import (C, xcb_xkb_sa_action_message_end, "xcb_xkb_sa_action_message_end");

   procedure xcb_xkb_sa_redirect_key_next (i : access xcb_xkb_sa_redirect_key_iterator_t);  -- /usr/include/xcb/xkb.h:5987
   pragma Import (C, xcb_xkb_sa_redirect_key_next, "xcb_xkb_sa_redirect_key_next");

   function xcb_xkb_sa_redirect_key_end (i : xcb_xkb_sa_redirect_key_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6009
   pragma Import (C, xcb_xkb_sa_redirect_key_end, "xcb_xkb_sa_redirect_key_end");

   procedure xcb_xkb_sa_device_btn_next (i : access xcb_xkb_sa_device_btn_iterator_t);  -- /usr/include/xcb/xkb.h:6030
   pragma Import (C, xcb_xkb_sa_device_btn_next, "xcb_xkb_sa_device_btn_next");

   function xcb_xkb_sa_device_btn_end (i : xcb_xkb_sa_device_btn_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6052
   pragma Import (C, xcb_xkb_sa_device_btn_end, "xcb_xkb_sa_device_btn_end");

   procedure xcb_xkb_sa_lock_device_btn_next (i : access xcb_xkb_sa_lock_device_btn_iterator_t);  -- /usr/include/xcb/xkb.h:6073
   pragma Import (C, xcb_xkb_sa_lock_device_btn_next, "xcb_xkb_sa_lock_device_btn_next");

   function xcb_xkb_sa_lock_device_btn_end (i : xcb_xkb_sa_lock_device_btn_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6095
   pragma Import (C, xcb_xkb_sa_lock_device_btn_end, "xcb_xkb_sa_lock_device_btn_end");

   procedure xcb_xkb_sa_device_valuator_next (i : access xcb_xkb_sa_device_valuator_iterator_t);  -- /usr/include/xcb/xkb.h:6116
   pragma Import (C, xcb_xkb_sa_device_valuator_next, "xcb_xkb_sa_device_valuator_next");

   function xcb_xkb_sa_device_valuator_end (i : xcb_xkb_sa_device_valuator_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6138
   pragma Import (C, xcb_xkb_sa_device_valuator_end, "xcb_xkb_sa_device_valuator_end");

   procedure xcb_xkb_si_action_next (i : access xcb_xkb_si_action_iterator_t);  -- /usr/include/xcb/xkb.h:6159
   pragma Import (C, xcb_xkb_si_action_next, "xcb_xkb_si_action_next");

   function xcb_xkb_si_action_end (i : xcb_xkb_si_action_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6181
   pragma Import (C, xcb_xkb_si_action_end, "xcb_xkb_si_action_end");

   procedure xcb_xkb_sym_interpret_next (i : access xcb_xkb_sym_interpret_iterator_t);  -- /usr/include/xcb/xkb.h:6202
   pragma Import (C, xcb_xkb_sym_interpret_next, "xcb_xkb_sym_interpret_next");

   function xcb_xkb_sym_interpret_end (i : xcb_xkb_sym_interpret_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6224
   pragma Import (C, xcb_xkb_sym_interpret_end, "xcb_xkb_sym_interpret_end");

   procedure xcb_xkb_action_next (i : access xcb_xkb_action_iterator_t);  -- /usr/include/xcb/xkb.h:6245
   pragma Import (C, xcb_xkb_action_next, "xcb_xkb_action_next");

   function xcb_xkb_action_end (i : xcb_xkb_action_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:6267
   pragma Import (C, xcb_xkb_action_end, "xcb_xkb_action_end");

   function xcb_xkb_use_extension
     (c : xcb_connection_t_access;
      wantedMajor : Libc.Stdint.uint16_t;
      wantedMinor : Libc.Stdint.uint16_t) return xcb_xkb_use_extension_cookie_t;  -- /usr/include/xcb/xkb.h:6290
   pragma Import (C, xcb_xkb_use_extension, "xcb_xkb_use_extension");

   function xcb_xkb_use_extension_unchecked
     (c : xcb_connection_t_access;
      wantedMajor : Libc.Stdint.uint16_t;
      wantedMinor : Libc.Stdint.uint16_t) return xcb_xkb_use_extension_cookie_t;  -- /usr/include/xcb/xkb.h:6318
   pragma Import (C, xcb_xkb_use_extension_unchecked, "xcb_xkb_use_extension_unchecked");

   function xcb_xkb_use_extension_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_use_extension_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_use_extension_reply_t;  -- /usr/include/xcb/xkb.h:6349
   pragma Import (C, xcb_xkb_use_extension_reply, "xcb_xkb_use_extension_reply");

   function xcb_xkb_select_events_details_serialize
     (u_buffer : System.Address;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:6354
   pragma Import (C, xcb_xkb_select_events_details_serialize, "xcb_xkb_select_events_details_serialize");

   function xcb_xkb_select_events_details_unpack
     (u_buffer : System.Address;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t;
      u_aux : access xcb_xkb_select_events_details_t) return int;  -- /usr/include/xcb/xkb.h:6361
   pragma Import (C, xcb_xkb_select_events_details_unpack, "xcb_xkb_select_events_details_unpack");

   function xcb_xkb_select_events_details_sizeof
     (u_buffer : System.Address;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t) return int;  -- /usr/include/xcb/xkb.h:6368
   pragma Import (C, xcb_xkb_select_events_details_sizeof, "xcb_xkb_select_events_details_sizeof");

   function xcb_xkb_select_events_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t;
      affectMap : Libc.Stdint.uint16_t;
      map : Libc.Stdint.uint16_t;
      details : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6402
   pragma Import (C, xcb_xkb_select_events_checked, "xcb_xkb_select_events_checked");

   function xcb_xkb_select_events
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t;
      affectMap : Libc.Stdint.uint16_t;
      map : Libc.Stdint.uint16_t;
      details : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6437
   pragma Import (C, xcb_xkb_select_events, "xcb_xkb_select_events");

   function xcb_xkb_select_events_aux_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t;
      affectMap : Libc.Stdint.uint16_t;
      map : Libc.Stdint.uint16_t;
      details : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6475
   pragma Import (C, xcb_xkb_select_events_aux_checked, "xcb_xkb_select_events_aux_checked");

   function xcb_xkb_select_events_aux
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectWhich : Libc.Stdint.uint16_t;
      clear : Libc.Stdint.uint16_t;
      selectAll : Libc.Stdint.uint16_t;
      affectMap : Libc.Stdint.uint16_t;
      map : Libc.Stdint.uint16_t;
      details : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6510
   pragma Import (C, xcb_xkb_select_events_aux, "xcb_xkb_select_events_aux");

   function xcb_xkb_bell_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      bellClass : xcb_xkb_bell_class_spec_t;
      bellID : xcb_xkb_id_spec_t;
      percent : Libc.Stdint.int8_t;
      forceSound : Libc.Stdint.uint8_t;
      eventOnly : Libc.Stdint.uint8_t;
      pitch : Libc.Stdint.int16_t;
      duration : Libc.Stdint.int16_t;
      name : XCB.XProto.xcb_atom_t;
      window : XCB.XProto.xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6551
   pragma Import (C, xcb_xkb_bell_checked, "xcb_xkb_bell_checked");

   function xcb_xkb_bell
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      bellClass : xcb_xkb_bell_class_spec_t;
      bellID : xcb_xkb_id_spec_t;
      percent : Libc.Stdint.int8_t;
      forceSound : Libc.Stdint.uint8_t;
      eventOnly : Libc.Stdint.uint8_t;
      pitch : Libc.Stdint.int16_t;
      duration : Libc.Stdint.int16_t;
      name : XCB.XProto.xcb_atom_t;
      window : XCB.XProto.xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6592
   pragma Import (C, xcb_xkb_bell, "xcb_xkb_bell");

   function xcb_xkb_get_state (c : xcb_connection_t_access; deviceSpec : xcb_xkb_device_spec_t) return xcb_xkb_get_state_cookie_t;  -- /usr/include/xcb/xkb.h:6624
   pragma Import (C, xcb_xkb_get_state, "xcb_xkb_get_state");

   function xcb_xkb_get_state_unchecked (c : xcb_connection_t_access; deviceSpec : xcb_xkb_device_spec_t) return xcb_xkb_get_state_cookie_t;  -- /usr/include/xcb/xkb.h:6650
   pragma Import (C, xcb_xkb_get_state_unchecked, "xcb_xkb_get_state_unchecked");

   function xcb_xkb_get_state_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_state_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_state_reply_t;  -- /usr/include/xcb/xkb.h:6680
   pragma Import (C, xcb_xkb_get_state_reply, "xcb_xkb_get_state_reply");

   function xcb_xkb_latch_lock_state_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectModLocks : Libc.Stdint.uint8_t;
      modLocks : Libc.Stdint.uint8_t;
      lockGroup : Libc.Stdint.uint8_t;
      groupLock : Libc.Stdint.uint8_t;
      affectModLatches : Libc.Stdint.uint8_t;
      latchGroup : Libc.Stdint.uint8_t;
      groupLatch : Libc.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6714
   pragma Import (C, xcb_xkb_latch_lock_state_checked, "xcb_xkb_latch_lock_state_checked");

   function xcb_xkb_latch_lock_state
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectModLocks : Libc.Stdint.uint8_t;
      modLocks : Libc.Stdint.uint8_t;
      lockGroup : Libc.Stdint.uint8_t;
      groupLock : Libc.Stdint.uint8_t;
      affectModLatches : Libc.Stdint.uint8_t;
      latchGroup : Libc.Stdint.uint8_t;
      groupLatch : Libc.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6751
   pragma Import (C, xcb_xkb_latch_lock_state, "xcb_xkb_latch_lock_state");

   function xcb_xkb_get_controls (c : xcb_connection_t_access; deviceSpec : xcb_xkb_device_spec_t) return xcb_xkb_get_controls_cookie_t;  -- /usr/include/xcb/xkb.h:6781
   pragma Import (C, xcb_xkb_get_controls, "xcb_xkb_get_controls");

   function xcb_xkb_get_controls_unchecked (c : xcb_connection_t_access; deviceSpec : xcb_xkb_device_spec_t) return xcb_xkb_get_controls_cookie_t;  -- /usr/include/xcb/xkb.h:6807
   pragma Import (C, xcb_xkb_get_controls_unchecked, "xcb_xkb_get_controls_unchecked");

   function xcb_xkb_get_controls_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_controls_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_controls_reply_t;  -- /usr/include/xcb/xkb.h:6837
   pragma Import (C, xcb_xkb_get_controls_reply, "xcb_xkb_get_controls_reply");

   function xcb_xkb_set_controls_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectInternalRealMods : Libc.Stdint.uint8_t;
      internalRealMods : Libc.Stdint.uint8_t;
      affectIgnoreLockRealMods : Libc.Stdint.uint8_t;
      ignoreLockRealMods : Libc.Stdint.uint8_t;
      affectInternalVirtualMods : Libc.Stdint.uint16_t;
      internalVirtualMods : Libc.Stdint.uint16_t;
      affectIgnoreLockVirtualMods : Libc.Stdint.uint16_t;
      ignoreLockVirtualMods : Libc.Stdint.uint16_t;
      mouseKeysDfltBtn : Libc.Stdint.uint8_t;
      groupsWrap : Libc.Stdint.uint8_t;
      accessXOptions : Libc.Stdint.uint16_t;
      affectEnabledControls : Libc.Stdint.uint32_t;
      enabledControls : Libc.Stdint.uint32_t;
      changeControls : Libc.Stdint.uint32_t;
      repeatDelay : Libc.Stdint.uint16_t;
      repeatInterval : Libc.Stdint.uint16_t;
      slowKeysDelay : Libc.Stdint.uint16_t;
      debounceDelay : Libc.Stdint.uint16_t;
      mouseKeysDelay : Libc.Stdint.uint16_t;
      mouseKeysInterval : Libc.Stdint.uint16_t;
      mouseKeysTimeToMax : Libc.Stdint.uint16_t;
      mouseKeysMaxSpeed : Libc.Stdint.uint16_t;
      mouseKeysCurve : Libc.Stdint.int16_t;
      accessXTimeout : Libc.Stdint.uint16_t;
      accessXTimeoutMask : Libc.Stdint.uint32_t;
      accessXTimeoutValues : Libc.Stdint.uint32_t;
      accessXTimeoutOptionsMask : Libc.Stdint.uint16_t;
      accessXTimeoutOptionsValues : Libc.Stdint.uint16_t;
      perKeyRepeat : access Libc.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6893
   pragma Import (C, xcb_xkb_set_controls_checked, "xcb_xkb_set_controls_checked");

   function xcb_xkb_set_controls
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      affectInternalRealMods : Libc.Stdint.uint8_t;
      internalRealMods : Libc.Stdint.uint8_t;
      affectIgnoreLockRealMods : Libc.Stdint.uint8_t;
      ignoreLockRealMods : Libc.Stdint.uint8_t;
      affectInternalVirtualMods : Libc.Stdint.uint16_t;
      internalVirtualMods : Libc.Stdint.uint16_t;
      affectIgnoreLockVirtualMods : Libc.Stdint.uint16_t;
      ignoreLockVirtualMods : Libc.Stdint.uint16_t;
      mouseKeysDfltBtn : Libc.Stdint.uint8_t;
      groupsWrap : Libc.Stdint.uint8_t;
      accessXOptions : Libc.Stdint.uint16_t;
      affectEnabledControls : Libc.Stdint.uint32_t;
      enabledControls : Libc.Stdint.uint32_t;
      changeControls : Libc.Stdint.uint32_t;
      repeatDelay : Libc.Stdint.uint16_t;
      repeatInterval : Libc.Stdint.uint16_t;
      slowKeysDelay : Libc.Stdint.uint16_t;
      debounceDelay : Libc.Stdint.uint16_t;
      mouseKeysDelay : Libc.Stdint.uint16_t;
      mouseKeysInterval : Libc.Stdint.uint16_t;
      mouseKeysTimeToMax : Libc.Stdint.uint16_t;
      mouseKeysMaxSpeed : Libc.Stdint.uint16_t;
      mouseKeysCurve : Libc.Stdint.int16_t;
      accessXTimeout : Libc.Stdint.uint16_t;
      accessXTimeoutMask : Libc.Stdint.uint32_t;
      accessXTimeoutValues : Libc.Stdint.uint32_t;
      accessXTimeoutOptionsMask : Libc.Stdint.uint16_t;
      accessXTimeoutOptionsValues : Libc.Stdint.uint16_t;
      perKeyRepeat : access Libc.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:6974
   pragma Import (C, xcb_xkb_set_controls, "xcb_xkb_set_controls");

   function xcb_xkb_get_map_map_types_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7017
   pragma Import (C, xcb_xkb_get_map_map_types_rtrn_length, "xcb_xkb_get_map_map_types_rtrn_length");

   function xcb_xkb_get_map_map_types_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_type_iterator_t;  -- /usr/include/xcb/xkb.h:7031
   pragma Import (C, xcb_xkb_get_map_map_types_rtrn_iterator, "xcb_xkb_get_map_map_types_rtrn_iterator");

   function xcb_xkb_get_map_map_syms_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7045
   pragma Import (C, xcb_xkb_get_map_map_syms_rtrn_length, "xcb_xkb_get_map_map_syms_rtrn_length");

   function xcb_xkb_get_map_map_syms_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_sym_map_iterator_t;  -- /usr/include/xcb/xkb.h:7059
   pragma Import (C, xcb_xkb_get_map_map_syms_rtrn_iterator, "xcb_xkb_get_map_map_syms_rtrn_iterator");

   function xcb_xkb_get_map_map_acts_rtrn_count (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:7073
   pragma Import (C, xcb_xkb_get_map_map_acts_rtrn_count, "xcb_xkb_get_map_map_acts_rtrn_count");

   function xcb_xkb_get_map_map_acts_rtrn_count_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7086
   pragma Import (C, xcb_xkb_get_map_map_acts_rtrn_count_length, "xcb_xkb_get_map_map_acts_rtrn_count_length");

   function xcb_xkb_get_map_map_acts_rtrn_count_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7100
   pragma Import (C, xcb_xkb_get_map_map_acts_rtrn_count_end, "xcb_xkb_get_map_map_acts_rtrn_count_end");

   function xcb_xkb_get_map_map_alignment_pad (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:7114
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad, "xcb_xkb_get_map_map_alignment_pad");

   function xcb_xkb_get_map_map_alignment_pad_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7127
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_length, "xcb_xkb_get_map_map_alignment_pad_length");

   function xcb_xkb_get_map_map_alignment_pad_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7141
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_end, "xcb_xkb_get_map_map_alignment_pad_end");

   function xcb_xkb_get_map_map_acts_rtrn_acts (S : System.Address) return access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:7155
   pragma Import (C, xcb_xkb_get_map_map_acts_rtrn_acts, "xcb_xkb_get_map_map_acts_rtrn_acts");

   function xcb_xkb_get_map_map_acts_rtrn_acts_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7168
   pragma Import (C, xcb_xkb_get_map_map_acts_rtrn_acts_length, "xcb_xkb_get_map_map_acts_rtrn_acts_length");

   function xcb_xkb_get_map_map_acts_rtrn_acts_iterator (R : System.Address; S : System.Address) return xcb_xkb_action_iterator_t;  -- /usr/include/xcb/xkb.h:7182
   pragma Import (C, xcb_xkb_get_map_map_acts_rtrn_acts_iterator, "xcb_xkb_get_map_map_acts_rtrn_acts_iterator");

   function xcb_xkb_get_map_map_behaviors_rtrn (S : System.Address) return access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:7196
   pragma Import (C, xcb_xkb_get_map_map_behaviors_rtrn, "xcb_xkb_get_map_map_behaviors_rtrn");

   function xcb_xkb_get_map_map_behaviors_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7209
   pragma Import (C, xcb_xkb_get_map_map_behaviors_rtrn_length, "xcb_xkb_get_map_map_behaviors_rtrn_length");

   function xcb_xkb_get_map_map_behaviors_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_behavior_iterator_t;  -- /usr/include/xcb/xkb.h:7223
   pragma Import (C, xcb_xkb_get_map_map_behaviors_rtrn_iterator, "xcb_xkb_get_map_map_behaviors_rtrn_iterator");

   function xcb_xkb_get_map_map_vmods_rtrn (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:7237
   pragma Import (C, xcb_xkb_get_map_map_vmods_rtrn, "xcb_xkb_get_map_map_vmods_rtrn");

   function xcb_xkb_get_map_map_vmods_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7250
   pragma Import (C, xcb_xkb_get_map_map_vmods_rtrn_length, "xcb_xkb_get_map_map_vmods_rtrn_length");

   function xcb_xkb_get_map_map_vmods_rtrn_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7264
   pragma Import (C, xcb_xkb_get_map_map_vmods_rtrn_end, "xcb_xkb_get_map_map_vmods_rtrn_end");

   function xcb_xkb_get_map_map_alignment_pad_2 (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:7278
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_2, "xcb_xkb_get_map_map_alignment_pad_2");

   function xcb_xkb_get_map_map_alignment_pad_2_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7291
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_2_length, "xcb_xkb_get_map_map_alignment_pad_2_length");

   function xcb_xkb_get_map_map_alignment_pad_2_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7305
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_2_end, "xcb_xkb_get_map_map_alignment_pad_2_end");

   function xcb_xkb_get_map_map_explicit_rtrn (S : System.Address) return access xcb_xkb_set_explicit_t;  -- /usr/include/xcb/xkb.h:7319
   pragma Import (C, xcb_xkb_get_map_map_explicit_rtrn, "xcb_xkb_get_map_map_explicit_rtrn");

   function xcb_xkb_get_map_map_explicit_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7332
   pragma Import (C, xcb_xkb_get_map_map_explicit_rtrn_length, "xcb_xkb_get_map_map_explicit_rtrn_length");

   function xcb_xkb_get_map_map_explicit_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_explicit_iterator_t;  -- /usr/include/xcb/xkb.h:7346
   pragma Import (C, xcb_xkb_get_map_map_explicit_rtrn_iterator, "xcb_xkb_get_map_map_explicit_rtrn_iterator");

   function xcb_xkb_get_map_map_alignment_pad_3 (S : System.Address) return access Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:7360
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_3, "xcb_xkb_get_map_map_alignment_pad_3");

   function xcb_xkb_get_map_map_alignment_pad_3_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7373
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_3_length, "xcb_xkb_get_map_map_alignment_pad_3_length");

   function xcb_xkb_get_map_map_alignment_pad_3_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7387
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_3_end, "xcb_xkb_get_map_map_alignment_pad_3_end");

   function xcb_xkb_get_map_map_modmap_rtrn (S : System.Address) return access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:7401
   pragma Import (C, xcb_xkb_get_map_map_modmap_rtrn, "xcb_xkb_get_map_map_modmap_rtrn");

   function xcb_xkb_get_map_map_modmap_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7414
   pragma Import (C, xcb_xkb_get_map_map_modmap_rtrn_length, "xcb_xkb_get_map_map_modmap_rtrn_length");

   function xcb_xkb_get_map_map_modmap_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_mod_map_iterator_t;  -- /usr/include/xcb/xkb.h:7428
   pragma Import (C, xcb_xkb_get_map_map_modmap_rtrn_iterator, "xcb_xkb_get_map_map_modmap_rtrn_iterator");

   function xcb_xkb_get_map_map_alignment_pad_4 (S : System.Address) return access Libc.Stdint.uint16_t;  -- /usr/include/xcb/xkb.h:7442
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_4, "xcb_xkb_get_map_map_alignment_pad_4");

   function xcb_xkb_get_map_map_alignment_pad_4_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7455
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_4_length, "xcb_xkb_get_map_map_alignment_pad_4_length");

   function xcb_xkb_get_map_map_alignment_pad_4_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7469
   pragma Import (C, xcb_xkb_get_map_map_alignment_pad_4_end, "xcb_xkb_get_map_map_alignment_pad_4_end");

   function xcb_xkb_get_map_map_vmodmap_rtrn (S : System.Address) return access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:7483
   pragma Import (C, xcb_xkb_get_map_map_vmodmap_rtrn, "xcb_xkb_get_map_map_vmodmap_rtrn");

   function xcb_xkb_get_map_map_vmodmap_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7496
   pragma Import (C, xcb_xkb_get_map_map_vmodmap_rtrn_length, "xcb_xkb_get_map_map_vmodmap_rtrn_length");

   function xcb_xkb_get_map_map_vmodmap_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_v_mod_map_iterator_t;  -- /usr/include/xcb/xkb.h:7510
   pragma Import (C, xcb_xkb_get_map_map_vmodmap_rtrn_iterator, "xcb_xkb_get_map_map_vmodmap_rtrn_iterator");

   function xcb_xkb_get_map_map_serialize
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:7514
   pragma Import (C, xcb_xkb_get_map_map_serialize, "xcb_xkb_get_map_map_serialize");

   function xcb_xkb_get_map_map_unpack
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t;
      u_aux : access xcb_xkb_get_map_map_t) return int;  -- /usr/include/xcb/xkb.h:7528
   pragma Import (C, xcb_xkb_get_map_map_unpack, "xcb_xkb_get_map_map_unpack");

   function xcb_xkb_get_map_map_sizeof
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t) return int;  -- /usr/include/xcb/xkb.h:7542
   pragma Import (C, xcb_xkb_get_map_map_sizeof, "xcb_xkb_get_map_map_sizeof");

   function xcb_xkb_get_map
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      full : Libc.Stdint.uint16_t;
      partial : Libc.Stdint.uint16_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKeySym : XCB.XProto.xcb_keycode_t;
      nKeySyms : Libc.Stdint.uint8_t;
      firstKeyAction : XCB.XProto.xcb_keycode_t;
      nKeyActions : Libc.Stdint.uint8_t;
      firstKeyBehavior : XCB.XProto.xcb_keycode_t;
      nKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      firstKeyExplicit : XCB.XProto.xcb_keycode_t;
      nKeyExplicit : Libc.Stdint.uint8_t;
      firstModMapKey : XCB.XProto.xcb_keycode_t;
      nModMapKeys : Libc.Stdint.uint8_t;
      firstVModMapKey : XCB.XProto.xcb_keycode_t;
      nVModMapKeys : Libc.Stdint.uint8_t) return xcb_xkb_get_map_cookie_t;  -- /usr/include/xcb/xkb.h:7591
   pragma Import (C, xcb_xkb_get_map, "xcb_xkb_get_map");

   function xcb_xkb_get_map_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      full : Libc.Stdint.uint16_t;
      partial : Libc.Stdint.uint16_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKeySym : XCB.XProto.xcb_keycode_t;
      nKeySyms : Libc.Stdint.uint8_t;
      firstKeyAction : XCB.XProto.xcb_keycode_t;
      nKeyActions : Libc.Stdint.uint8_t;
      firstKeyBehavior : XCB.XProto.xcb_keycode_t;
      nKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      firstKeyExplicit : XCB.XProto.xcb_keycode_t;
      nKeyExplicit : Libc.Stdint.uint8_t;
      firstModMapKey : XCB.XProto.xcb_keycode_t;
      nModMapKeys : Libc.Stdint.uint8_t;
      firstVModMapKey : XCB.XProto.xcb_keycode_t;
      nVModMapKeys : Libc.Stdint.uint8_t) return xcb_xkb_get_map_cookie_t;  -- /usr/include/xcb/xkb.h:7651
   pragma Import (C, xcb_xkb_get_map_unchecked, "xcb_xkb_get_map_unchecked");

   function xcb_xkb_get_map_map (R : System.Address) return System.Address;  -- /usr/include/xcb/xkb.h:7682
   pragma Import (C, xcb_xkb_get_map_map, "xcb_xkb_get_map_map");

   function xcb_xkb_get_map_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_map_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_map_reply_t;  -- /usr/include/xcb/xkb.h:7711
   pragma Import (C, xcb_xkb_get_map_reply, "xcb_xkb_get_map_reply");

   function xcb_xkb_set_map_values_types_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7726
   pragma Import (C, xcb_xkb_set_map_values_types_length, "xcb_xkb_set_map_values_types_length");

   function xcb_xkb_set_map_values_types_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_key_type_iterator_t;  -- /usr/include/xcb/xkb.h:7740
   pragma Import (C, xcb_xkb_set_map_values_types_iterator, "xcb_xkb_set_map_values_types_iterator");

   function xcb_xkb_set_map_values_syms_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7754
   pragma Import (C, xcb_xkb_set_map_values_syms_length, "xcb_xkb_set_map_values_syms_length");

   function xcb_xkb_set_map_values_syms_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_sym_map_iterator_t;  -- /usr/include/xcb/xkb.h:7768
   pragma Import (C, xcb_xkb_set_map_values_syms_iterator, "xcb_xkb_set_map_values_syms_iterator");

   function xcb_xkb_set_map_values_actions_count (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:7782
   pragma Import (C, xcb_xkb_set_map_values_actions_count, "xcb_xkb_set_map_values_actions_count");

   function xcb_xkb_set_map_values_actions_count_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7795
   pragma Import (C, xcb_xkb_set_map_values_actions_count_length, "xcb_xkb_set_map_values_actions_count_length");

   function xcb_xkb_set_map_values_actions_count_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7809
   pragma Import (C, xcb_xkb_set_map_values_actions_count_end, "xcb_xkb_set_map_values_actions_count_end");

   function xcb_xkb_set_map_values_actions (S : System.Address) return access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:7823
   pragma Import (C, xcb_xkb_set_map_values_actions, "xcb_xkb_set_map_values_actions");

   function xcb_xkb_set_map_values_actions_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7836
   pragma Import (C, xcb_xkb_set_map_values_actions_length, "xcb_xkb_set_map_values_actions_length");

   function xcb_xkb_set_map_values_actions_iterator (R : System.Address; S : System.Address) return xcb_xkb_action_iterator_t;  -- /usr/include/xcb/xkb.h:7850
   pragma Import (C, xcb_xkb_set_map_values_actions_iterator, "xcb_xkb_set_map_values_actions_iterator");

   function xcb_xkb_set_map_values_behaviors (S : System.Address) return access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:7864
   pragma Import (C, xcb_xkb_set_map_values_behaviors, "xcb_xkb_set_map_values_behaviors");

   function xcb_xkb_set_map_values_behaviors_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7877
   pragma Import (C, xcb_xkb_set_map_values_behaviors_length, "xcb_xkb_set_map_values_behaviors_length");

   function xcb_xkb_set_map_values_behaviors_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_behavior_iterator_t;  -- /usr/include/xcb/xkb.h:7891
   pragma Import (C, xcb_xkb_set_map_values_behaviors_iterator, "xcb_xkb_set_map_values_behaviors_iterator");

   function xcb_xkb_set_map_values_vmods (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:7905
   pragma Import (C, xcb_xkb_set_map_values_vmods, "xcb_xkb_set_map_values_vmods");

   function xcb_xkb_set_map_values_vmods_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7918
   pragma Import (C, xcb_xkb_set_map_values_vmods_length, "xcb_xkb_set_map_values_vmods_length");

   function xcb_xkb_set_map_values_vmods_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:7932
   pragma Import (C, xcb_xkb_set_map_values_vmods_end, "xcb_xkb_set_map_values_vmods_end");

   function xcb_xkb_set_map_values_explicit (S : System.Address) return access xcb_xkb_set_explicit_t;  -- /usr/include/xcb/xkb.h:7946
   pragma Import (C, xcb_xkb_set_map_values_explicit, "xcb_xkb_set_map_values_explicit");

   function xcb_xkb_set_map_values_explicit_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:7959
   pragma Import (C, xcb_xkb_set_map_values_explicit_length, "xcb_xkb_set_map_values_explicit_length");

   function xcb_xkb_set_map_values_explicit_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_explicit_iterator_t;  -- /usr/include/xcb/xkb.h:7973
   pragma Import (C, xcb_xkb_set_map_values_explicit_iterator, "xcb_xkb_set_map_values_explicit_iterator");

   function xcb_xkb_set_map_values_modmap (S : System.Address) return access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:7987
   pragma Import (C, xcb_xkb_set_map_values_modmap, "xcb_xkb_set_map_values_modmap");

   function xcb_xkb_set_map_values_modmap_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:8000
   pragma Import (C, xcb_xkb_set_map_values_modmap_length, "xcb_xkb_set_map_values_modmap_length");

   function xcb_xkb_set_map_values_modmap_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_mod_map_iterator_t;  -- /usr/include/xcb/xkb.h:8014
   pragma Import (C, xcb_xkb_set_map_values_modmap_iterator, "xcb_xkb_set_map_values_modmap_iterator");

   function xcb_xkb_set_map_values_vmodmap (S : System.Address) return access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:8028
   pragma Import (C, xcb_xkb_set_map_values_vmodmap, "xcb_xkb_set_map_values_vmodmap");

   function xcb_xkb_set_map_values_vmodmap_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:8041
   pragma Import (C, xcb_xkb_set_map_values_vmodmap_length, "xcb_xkb_set_map_values_vmodmap_length");

   function xcb_xkb_set_map_values_vmodmap_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_v_mod_map_iterator_t;  -- /usr/include/xcb/xkb.h:8055
   pragma Import (C, xcb_xkb_set_map_values_vmodmap_iterator, "xcb_xkb_set_map_values_vmodmap_iterator");

   function xcb_xkb_set_map_values_serialize
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:8059
   pragma Import (C, xcb_xkb_set_map_values_serialize, "xcb_xkb_set_map_values_serialize");

   function xcb_xkb_set_map_values_unpack
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t;
      u_aux : access xcb_xkb_set_map_values_t) return int;  -- /usr/include/xcb/xkb.h:8073
   pragma Import (C, xcb_xkb_set_map_values_unpack, "xcb_xkb_set_map_values_unpack");

   function xcb_xkb_set_map_values_sizeof
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t) return int;  -- /usr/include/xcb/xkb.h:8087
   pragma Import (C, xcb_xkb_set_map_values_sizeof, "xcb_xkb_set_map_values_sizeof");

   function xcb_xkb_set_map_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      present : Libc.Stdint.uint16_t;
      flags : Libc.Stdint.uint16_t;
      minKeyCode : XCB.XProto.xcb_keycode_t;
      maxKeyCode : XCB.XProto.xcb_keycode_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKeySym : XCB.XProto.xcb_keycode_t;
      nKeySyms : Libc.Stdint.uint8_t;
      totalSyms : Libc.Stdint.uint16_t;
      firstKeyAction : XCB.XProto.xcb_keycode_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      firstKeyBehavior : XCB.XProto.xcb_keycode_t;
      nKeyBehaviors : Libc.Stdint.uint8_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      firstKeyExplicit : XCB.XProto.xcb_keycode_t;
      nKeyExplicit : Libc.Stdint.uint8_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      firstModMapKey : XCB.XProto.xcb_keycode_t;
      nModMapKeys : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      firstVModMapKey : XCB.XProto.xcb_keycode_t;
      nVModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8148
   pragma Import (C, xcb_xkb_set_map_checked, "xcb_xkb_set_map_checked");

   function xcb_xkb_set_map
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      present : Libc.Stdint.uint16_t;
      flags : Libc.Stdint.uint16_t;
      minKeyCode : XCB.XProto.xcb_keycode_t;
      maxKeyCode : XCB.XProto.xcb_keycode_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKeySym : XCB.XProto.xcb_keycode_t;
      nKeySyms : Libc.Stdint.uint8_t;
      totalSyms : Libc.Stdint.uint16_t;
      firstKeyAction : XCB.XProto.xcb_keycode_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      firstKeyBehavior : XCB.XProto.xcb_keycode_t;
      nKeyBehaviors : Libc.Stdint.uint8_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      firstKeyExplicit : XCB.XProto.xcb_keycode_t;
      nKeyExplicit : Libc.Stdint.uint8_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      firstModMapKey : XCB.XProto.xcb_keycode_t;
      nModMapKeys : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      firstVModMapKey : XCB.XProto.xcb_keycode_t;
      nVModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8223
   pragma Import (C, xcb_xkb_set_map, "xcb_xkb_set_map");

   function xcb_xkb_set_map_aux_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      present : Libc.Stdint.uint16_t;
      flags : Libc.Stdint.uint16_t;
      minKeyCode : XCB.XProto.xcb_keycode_t;
      maxKeyCode : XCB.XProto.xcb_keycode_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKeySym : XCB.XProto.xcb_keycode_t;
      nKeySyms : Libc.Stdint.uint8_t;
      totalSyms : Libc.Stdint.uint16_t;
      firstKeyAction : XCB.XProto.xcb_keycode_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      firstKeyBehavior : XCB.XProto.xcb_keycode_t;
      nKeyBehaviors : Libc.Stdint.uint8_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      firstKeyExplicit : XCB.XProto.xcb_keycode_t;
      nKeyExplicit : Libc.Stdint.uint8_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      firstModMapKey : XCB.XProto.xcb_keycode_t;
      nModMapKeys : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      firstVModMapKey : XCB.XProto.xcb_keycode_t;
      nVModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8301
   pragma Import (C, xcb_xkb_set_map_aux_checked, "xcb_xkb_set_map_aux_checked");

   function xcb_xkb_set_map_aux
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      present : Libc.Stdint.uint16_t;
      flags : Libc.Stdint.uint16_t;
      minKeyCode : XCB.XProto.xcb_keycode_t;
      maxKeyCode : XCB.XProto.xcb_keycode_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKeySym : XCB.XProto.xcb_keycode_t;
      nKeySyms : Libc.Stdint.uint8_t;
      totalSyms : Libc.Stdint.uint16_t;
      firstKeyAction : XCB.XProto.xcb_keycode_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      firstKeyBehavior : XCB.XProto.xcb_keycode_t;
      nKeyBehaviors : Libc.Stdint.uint8_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      firstKeyExplicit : XCB.XProto.xcb_keycode_t;
      nKeyExplicit : Libc.Stdint.uint8_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      firstModMapKey : XCB.XProto.xcb_keycode_t;
      nModMapKeys : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      firstVModMapKey : XCB.XProto.xcb_keycode_t;
      nVModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8376
   pragma Import (C, xcb_xkb_set_map_aux, "xcb_xkb_set_map_aux");

   function xcb_xkb_get_compat_map_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:8406
   pragma Import (C, xcb_xkb_get_compat_map_sizeof, "xcb_xkb_get_compat_map_sizeof");

   function xcb_xkb_get_compat_map
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      groups : Libc.Stdint.uint8_t;
      getAllSI : Libc.Stdint.uint8_t;
      firstSI : Libc.Stdint.uint16_t;
      nSI : Libc.Stdint.uint16_t) return xcb_xkb_get_compat_map_cookie_t;  -- /usr/include/xcb/xkb.h:8432
   pragma Import (C, xcb_xkb_get_compat_map, "xcb_xkb_get_compat_map");

   function xcb_xkb_get_compat_map_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      groups : Libc.Stdint.uint8_t;
      getAllSI : Libc.Stdint.uint8_t;
      firstSI : Libc.Stdint.uint16_t;
      nSI : Libc.Stdint.uint16_t) return xcb_xkb_get_compat_map_cookie_t;  -- /usr/include/xcb/xkb.h:8466
   pragma Import (C, xcb_xkb_get_compat_map_unchecked, "xcb_xkb_get_compat_map_unchecked");

   function xcb_xkb_get_compat_map_si_rtrn (R : System.Address) return access xcb_xkb_sym_interpret_t;  -- /usr/include/xcb/xkb.h:8484
   pragma Import (C, xcb_xkb_get_compat_map_si_rtrn, "xcb_xkb_get_compat_map_si_rtrn");

   function xcb_xkb_get_compat_map_si_rtrn_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:8497
   pragma Import (C, xcb_xkb_get_compat_map_si_rtrn_length, "xcb_xkb_get_compat_map_si_rtrn_length");

   function xcb_xkb_get_compat_map_si_rtrn_iterator (R : System.Address) return xcb_xkb_sym_interpret_iterator_t;  -- /usr/include/xcb/xkb.h:8510
   pragma Import (C, xcb_xkb_get_compat_map_si_rtrn_iterator, "xcb_xkb_get_compat_map_si_rtrn_iterator");

   function xcb_xkb_get_compat_map_group_rtrn (R : System.Address) return access xcb_xkb_mod_def_t;  -- /usr/include/xcb/xkb.h:8523
   pragma Import (C, xcb_xkb_get_compat_map_group_rtrn, "xcb_xkb_get_compat_map_group_rtrn");

   function xcb_xkb_get_compat_map_group_rtrn_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:8536
   pragma Import (C, xcb_xkb_get_compat_map_group_rtrn_length, "xcb_xkb_get_compat_map_group_rtrn_length");

   function xcb_xkb_get_compat_map_group_rtrn_iterator (R : System.Address) return xcb_xkb_mod_def_iterator_t;  -- /usr/include/xcb/xkb.h:8549
   pragma Import (C, xcb_xkb_get_compat_map_group_rtrn_iterator, "xcb_xkb_get_compat_map_group_rtrn_iterator");

   function xcb_xkb_get_compat_map_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_compat_map_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_compat_map_reply_t;  -- /usr/include/xcb/xkb.h:8578
   pragma Import (C, xcb_xkb_get_compat_map_reply, "xcb_xkb_get_compat_map_reply");

   function xcb_xkb_set_compat_map_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:8583
   pragma Import (C, xcb_xkb_set_compat_map_sizeof, "xcb_xkb_set_compat_map_sizeof");

   function xcb_xkb_set_compat_map_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      recomputeActions : Libc.Stdint.uint8_t;
      truncateSI : Libc.Stdint.uint8_t;
      groups : Libc.Stdint.uint8_t;
      firstSI : Libc.Stdint.uint16_t;
      nSI : Libc.Stdint.uint16_t;
      si : System.Address;
      groupMaps : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8615
   pragma Import (C, xcb_xkb_set_compat_map_checked, "xcb_xkb_set_compat_map_checked");

   function xcb_xkb_set_compat_map
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      recomputeActions : Libc.Stdint.uint8_t;
      truncateSI : Libc.Stdint.uint8_t;
      groups : Libc.Stdint.uint8_t;
      firstSI : Libc.Stdint.uint16_t;
      nSI : Libc.Stdint.uint16_t;
      si : System.Address;
      groupMaps : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8652
   pragma Import (C, xcb_xkb_set_compat_map, "xcb_xkb_set_compat_map");

   function xcb_xkb_get_indicator_state (c : xcb_connection_t_access; deviceSpec : xcb_xkb_device_spec_t) return xcb_xkb_get_indicator_state_cookie_t;  -- /usr/include/xcb/xkb.h:8682
   pragma Import (C, xcb_xkb_get_indicator_state, "xcb_xkb_get_indicator_state");

   function xcb_xkb_get_indicator_state_unchecked (c : xcb_connection_t_access; deviceSpec : xcb_xkb_device_spec_t) return xcb_xkb_get_indicator_state_cookie_t;  -- /usr/include/xcb/xkb.h:8708
   pragma Import (C, xcb_xkb_get_indicator_state_unchecked, "xcb_xkb_get_indicator_state_unchecked");

   function xcb_xkb_get_indicator_state_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_indicator_state_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_indicator_state_reply_t;  -- /usr/include/xcb/xkb.h:8738
   pragma Import (C, xcb_xkb_get_indicator_state_reply, "xcb_xkb_get_indicator_state_reply");

   function xcb_xkb_get_indicator_map_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:8743
   pragma Import (C, xcb_xkb_get_indicator_map_sizeof, "xcb_xkb_get_indicator_map_sizeof");

   function xcb_xkb_get_indicator_map
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      which : Libc.Stdint.uint32_t) return xcb_xkb_get_indicator_map_cookie_t;  -- /usr/include/xcb/xkb.h:8766
   pragma Import (C, xcb_xkb_get_indicator_map, "xcb_xkb_get_indicator_map");

   function xcb_xkb_get_indicator_map_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      which : Libc.Stdint.uint32_t) return xcb_xkb_get_indicator_map_cookie_t;  -- /usr/include/xcb/xkb.h:8794
   pragma Import (C, xcb_xkb_get_indicator_map_unchecked, "xcb_xkb_get_indicator_map_unchecked");

   function xcb_xkb_get_indicator_map_maps (R : System.Address) return access xcb_xkb_indicator_map_t;  -- /usr/include/xcb/xkb.h:8809
   pragma Import (C, xcb_xkb_get_indicator_map_maps, "xcb_xkb_get_indicator_map_maps");

   function xcb_xkb_get_indicator_map_maps_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:8822
   pragma Import (C, xcb_xkb_get_indicator_map_maps_length, "xcb_xkb_get_indicator_map_maps_length");

   function xcb_xkb_get_indicator_map_maps_iterator (R : System.Address) return xcb_xkb_indicator_map_iterator_t;  -- /usr/include/xcb/xkb.h:8835
   pragma Import (C, xcb_xkb_get_indicator_map_maps_iterator, "xcb_xkb_get_indicator_map_maps_iterator");

   function xcb_xkb_get_indicator_map_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_indicator_map_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_indicator_map_reply_t;  -- /usr/include/xcb/xkb.h:8864
   pragma Import (C, xcb_xkb_get_indicator_map_reply, "xcb_xkb_get_indicator_map_reply");

   function xcb_xkb_set_indicator_map_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:8869
   pragma Import (C, xcb_xkb_set_indicator_map_sizeof, "xcb_xkb_set_indicator_map_sizeof");

   function xcb_xkb_set_indicator_map_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      which : Libc.Stdint.uint32_t;
      maps : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8896
   pragma Import (C, xcb_xkb_set_indicator_map_checked, "xcb_xkb_set_indicator_map_checked");

   function xcb_xkb_set_indicator_map
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      which : Libc.Stdint.uint32_t;
      maps : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:8923
   pragma Import (C, xcb_xkb_set_indicator_map, "xcb_xkb_set_indicator_map");

   function xcb_xkb_get_named_indicator
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      ledClass : xcb_xkb_led_class_spec_t;
      ledID : xcb_xkb_id_spec_t;
      indicator : XCB.XProto.xcb_atom_t) return xcb_xkb_get_named_indicator_cookie_t;  -- /usr/include/xcb/xkb.h:8951
   pragma Import (C, xcb_xkb_get_named_indicator, "xcb_xkb_get_named_indicator");

   function xcb_xkb_get_named_indicator_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      ledClass : xcb_xkb_led_class_spec_t;
      ledID : xcb_xkb_id_spec_t;
      indicator : XCB.XProto.xcb_atom_t) return xcb_xkb_get_named_indicator_cookie_t;  -- /usr/include/xcb/xkb.h:8983
   pragma Import (C, xcb_xkb_get_named_indicator_unchecked, "xcb_xkb_get_named_indicator_unchecked");

   function xcb_xkb_get_named_indicator_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_named_indicator_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_named_indicator_reply_t;  -- /usr/include/xcb/xkb.h:9016
   pragma Import (C, xcb_xkb_get_named_indicator_reply, "xcb_xkb_get_named_indicator_reply");

   function xcb_xkb_set_named_indicator_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      ledClass : xcb_xkb_led_class_spec_t;
      ledID : xcb_xkb_id_spec_t;
      indicator : XCB.XProto.xcb_atom_t;
      setState : Libc.Stdint.uint8_t;
      on : Libc.Stdint.uint8_t;
      setMap : Libc.Stdint.uint8_t;
      createMap : Libc.Stdint.uint8_t;
      map_flags : Libc.Stdint.uint8_t;
      map_whichGroups : Libc.Stdint.uint8_t;
      map_groups : Libc.Stdint.uint8_t;
      map_whichMods : Libc.Stdint.uint8_t;
      map_realMods : Libc.Stdint.uint8_t;
      map_vmods : Libc.Stdint.uint16_t;
      map_ctrls : Libc.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:9057
   pragma Import (C, xcb_xkb_set_named_indicator_checked, "xcb_xkb_set_named_indicator_checked");

   function xcb_xkb_set_named_indicator
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      ledClass : xcb_xkb_led_class_spec_t;
      ledID : xcb_xkb_id_spec_t;
      indicator : XCB.XProto.xcb_atom_t;
      setState : Libc.Stdint.uint8_t;
      on : Libc.Stdint.uint8_t;
      setMap : Libc.Stdint.uint8_t;
      createMap : Libc.Stdint.uint8_t;
      map_flags : Libc.Stdint.uint8_t;
      map_whichGroups : Libc.Stdint.uint8_t;
      map_groups : Libc.Stdint.uint8_t;
      map_whichMods : Libc.Stdint.uint8_t;
      map_realMods : Libc.Stdint.uint8_t;
      map_vmods : Libc.Stdint.uint16_t;
      map_ctrls : Libc.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:9108
   pragma Import (C, xcb_xkb_set_named_indicator, "xcb_xkb_set_named_indicator");

   function xcb_xkb_get_names_value_list_type_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9136
   pragma Import (C, xcb_xkb_get_names_value_list_type_names, "xcb_xkb_get_names_value_list_type_names");

   function xcb_xkb_get_names_value_list_type_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9149
   pragma Import (C, xcb_xkb_get_names_value_list_type_names_length, "xcb_xkb_get_names_value_list_type_names_length");

   function xcb_xkb_get_names_value_list_type_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9163
   pragma Import (C, xcb_xkb_get_names_value_list_type_names_end, "xcb_xkb_get_names_value_list_type_names_end");

   function xcb_xkb_get_names_value_list_n_levels_per_type (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:9177
   pragma Import (C, xcb_xkb_get_names_value_list_n_levels_per_type, "xcb_xkb_get_names_value_list_n_levels_per_type");

   function xcb_xkb_get_names_value_list_n_levels_per_type_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9190
   pragma Import (C, xcb_xkb_get_names_value_list_n_levels_per_type_length, "xcb_xkb_get_names_value_list_n_levels_per_type_length");

   function xcb_xkb_get_names_value_list_n_levels_per_type_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9204
   pragma Import (C, xcb_xkb_get_names_value_list_n_levels_per_type_end, "xcb_xkb_get_names_value_list_n_levels_per_type_end");

   function xcb_xkb_get_names_value_list_alignment_pad (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:9218
   pragma Import (C, xcb_xkb_get_names_value_list_alignment_pad, "xcb_xkb_get_names_value_list_alignment_pad");

   function xcb_xkb_get_names_value_list_alignment_pad_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9231
   pragma Import (C, xcb_xkb_get_names_value_list_alignment_pad_length, "xcb_xkb_get_names_value_list_alignment_pad_length");

   function xcb_xkb_get_names_value_list_alignment_pad_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9245
   pragma Import (C, xcb_xkb_get_names_value_list_alignment_pad_end, "xcb_xkb_get_names_value_list_alignment_pad_end");

   function xcb_xkb_get_names_value_list_kt_level_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9259
   pragma Import (C, xcb_xkb_get_names_value_list_kt_level_names, "xcb_xkb_get_names_value_list_kt_level_names");

   function xcb_xkb_get_names_value_list_kt_level_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9272
   pragma Import (C, xcb_xkb_get_names_value_list_kt_level_names_length, "xcb_xkb_get_names_value_list_kt_level_names_length");

   function xcb_xkb_get_names_value_list_kt_level_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9286
   pragma Import (C, xcb_xkb_get_names_value_list_kt_level_names_end, "xcb_xkb_get_names_value_list_kt_level_names_end");

   function xcb_xkb_get_names_value_list_indicator_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9300
   pragma Import (C, xcb_xkb_get_names_value_list_indicator_names, "xcb_xkb_get_names_value_list_indicator_names");

   function xcb_xkb_get_names_value_list_indicator_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9313
   pragma Import (C, xcb_xkb_get_names_value_list_indicator_names_length, "xcb_xkb_get_names_value_list_indicator_names_length");

   function xcb_xkb_get_names_value_list_indicator_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9327
   pragma Import (C, xcb_xkb_get_names_value_list_indicator_names_end, "xcb_xkb_get_names_value_list_indicator_names_end");

   function xcb_xkb_get_names_value_list_virtual_mod_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9341
   pragma Import (C, xcb_xkb_get_names_value_list_virtual_mod_names, "xcb_xkb_get_names_value_list_virtual_mod_names");

   function xcb_xkb_get_names_value_list_virtual_mod_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9354
   pragma Import (C, xcb_xkb_get_names_value_list_virtual_mod_names_length, "xcb_xkb_get_names_value_list_virtual_mod_names_length");

   function xcb_xkb_get_names_value_list_virtual_mod_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9368
   pragma Import (C, xcb_xkb_get_names_value_list_virtual_mod_names_end, "xcb_xkb_get_names_value_list_virtual_mod_names_end");

   function xcb_xkb_get_names_value_list_groups (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9382
   pragma Import (C, xcb_xkb_get_names_value_list_groups, "xcb_xkb_get_names_value_list_groups");

   function xcb_xkb_get_names_value_list_groups_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9395
   pragma Import (C, xcb_xkb_get_names_value_list_groups_length, "xcb_xkb_get_names_value_list_groups_length");

   function xcb_xkb_get_names_value_list_groups_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9409
   pragma Import (C, xcb_xkb_get_names_value_list_groups_end, "xcb_xkb_get_names_value_list_groups_end");

   function xcb_xkb_get_names_value_list_key_names (S : System.Address) return access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:9423
   pragma Import (C, xcb_xkb_get_names_value_list_key_names, "xcb_xkb_get_names_value_list_key_names");

   function xcb_xkb_get_names_value_list_key_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9436
   pragma Import (C, xcb_xkb_get_names_value_list_key_names_length, "xcb_xkb_get_names_value_list_key_names_length");

   function xcb_xkb_get_names_value_list_key_names_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_name_iterator_t;  -- /usr/include/xcb/xkb.h:9450
   pragma Import (C, xcb_xkb_get_names_value_list_key_names_iterator, "xcb_xkb_get_names_value_list_key_names_iterator");

   function xcb_xkb_get_names_value_list_key_aliases (S : System.Address) return access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:9464
   pragma Import (C, xcb_xkb_get_names_value_list_key_aliases, "xcb_xkb_get_names_value_list_key_aliases");

   function xcb_xkb_get_names_value_list_key_aliases_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9477
   pragma Import (C, xcb_xkb_get_names_value_list_key_aliases_length, "xcb_xkb_get_names_value_list_key_aliases_length");

   function xcb_xkb_get_names_value_list_key_aliases_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_alias_iterator_t;  -- /usr/include/xcb/xkb.h:9491
   pragma Import (C, xcb_xkb_get_names_value_list_key_aliases_iterator, "xcb_xkb_get_names_value_list_key_aliases_iterator");

   function xcb_xkb_get_names_value_list_radio_group_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9505
   pragma Import (C, xcb_xkb_get_names_value_list_radio_group_names, "xcb_xkb_get_names_value_list_radio_group_names");

   function xcb_xkb_get_names_value_list_radio_group_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9518
   pragma Import (C, xcb_xkb_get_names_value_list_radio_group_names_length, "xcb_xkb_get_names_value_list_radio_group_names_length");

   function xcb_xkb_get_names_value_list_radio_group_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9532
   pragma Import (C, xcb_xkb_get_names_value_list_radio_group_names_end, "xcb_xkb_get_names_value_list_radio_group_names_end");

   function xcb_xkb_get_names_value_list_serialize
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:9536
   pragma Import (C, xcb_xkb_get_names_value_list_serialize, "xcb_xkb_get_names_value_list_serialize");

   function xcb_xkb_get_names_value_list_unpack
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t;
      u_aux : access xcb_xkb_get_names_value_list_t) return int;  -- /usr/include/xcb/xkb.h:9548
   pragma Import (C, xcb_xkb_get_names_value_list_unpack, "xcb_xkb_get_names_value_list_unpack");

   function xcb_xkb_get_names_value_list_sizeof
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t) return int;  -- /usr/include/xcb/xkb.h:9560
   pragma Import (C, xcb_xkb_get_names_value_list_sizeof, "xcb_xkb_get_names_value_list_sizeof");

   function xcb_xkb_get_names
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      which : Libc.Stdint.uint32_t) return xcb_xkb_get_names_cookie_t;  -- /usr/include/xcb/xkb.h:9591
   pragma Import (C, xcb_xkb_get_names, "xcb_xkb_get_names");

   function xcb_xkb_get_names_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      which : Libc.Stdint.uint32_t) return xcb_xkb_get_names_cookie_t;  -- /usr/include/xcb/xkb.h:9619
   pragma Import (C, xcb_xkb_get_names_unchecked, "xcb_xkb_get_names_unchecked");

   function xcb_xkb_get_names_value_list (R : System.Address) return System.Address;  -- /usr/include/xcb/xkb.h:9634
   pragma Import (C, xcb_xkb_get_names_value_list, "xcb_xkb_get_names_value_list");

   function xcb_xkb_get_names_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_names_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_names_reply_t;  -- /usr/include/xcb/xkb.h:9663
   pragma Import (C, xcb_xkb_get_names_reply, "xcb_xkb_get_names_reply");

   function xcb_xkb_set_names_values_type_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9678
   pragma Import (C, xcb_xkb_set_names_values_type_names, "xcb_xkb_set_names_values_type_names");

   function xcb_xkb_set_names_values_type_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9691
   pragma Import (C, xcb_xkb_set_names_values_type_names_length, "xcb_xkb_set_names_values_type_names_length");

   function xcb_xkb_set_names_values_type_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9705
   pragma Import (C, xcb_xkb_set_names_values_type_names_end, "xcb_xkb_set_names_values_type_names_end");

   function xcb_xkb_set_names_values_n_levels_per_type (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:9719
   pragma Import (C, xcb_xkb_set_names_values_n_levels_per_type, "xcb_xkb_set_names_values_n_levels_per_type");

   function xcb_xkb_set_names_values_n_levels_per_type_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9732
   pragma Import (C, xcb_xkb_set_names_values_n_levels_per_type_length, "xcb_xkb_set_names_values_n_levels_per_type_length");

   function xcb_xkb_set_names_values_n_levels_per_type_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9746
   pragma Import (C, xcb_xkb_set_names_values_n_levels_per_type_end, "xcb_xkb_set_names_values_n_levels_per_type_end");

   function xcb_xkb_set_names_values_kt_level_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9760
   pragma Import (C, xcb_xkb_set_names_values_kt_level_names, "xcb_xkb_set_names_values_kt_level_names");

   function xcb_xkb_set_names_values_kt_level_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9773
   pragma Import (C, xcb_xkb_set_names_values_kt_level_names_length, "xcb_xkb_set_names_values_kt_level_names_length");

   function xcb_xkb_set_names_values_kt_level_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9787
   pragma Import (C, xcb_xkb_set_names_values_kt_level_names_end, "xcb_xkb_set_names_values_kt_level_names_end");

   function xcb_xkb_set_names_values_indicator_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9801
   pragma Import (C, xcb_xkb_set_names_values_indicator_names, "xcb_xkb_set_names_values_indicator_names");

   function xcb_xkb_set_names_values_indicator_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9814
   pragma Import (C, xcb_xkb_set_names_values_indicator_names_length, "xcb_xkb_set_names_values_indicator_names_length");

   function xcb_xkb_set_names_values_indicator_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9828
   pragma Import (C, xcb_xkb_set_names_values_indicator_names_end, "xcb_xkb_set_names_values_indicator_names_end");

   function xcb_xkb_set_names_values_virtual_mod_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9842
   pragma Import (C, xcb_xkb_set_names_values_virtual_mod_names, "xcb_xkb_set_names_values_virtual_mod_names");

   function xcb_xkb_set_names_values_virtual_mod_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9855
   pragma Import (C, xcb_xkb_set_names_values_virtual_mod_names_length, "xcb_xkb_set_names_values_virtual_mod_names_length");

   function xcb_xkb_set_names_values_virtual_mod_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9869
   pragma Import (C, xcb_xkb_set_names_values_virtual_mod_names_end, "xcb_xkb_set_names_values_virtual_mod_names_end");

   function xcb_xkb_set_names_values_groups (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:9883
   pragma Import (C, xcb_xkb_set_names_values_groups, "xcb_xkb_set_names_values_groups");

   function xcb_xkb_set_names_values_groups_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9896
   pragma Import (C, xcb_xkb_set_names_values_groups_length, "xcb_xkb_set_names_values_groups_length");

   function xcb_xkb_set_names_values_groups_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:9910
   pragma Import (C, xcb_xkb_set_names_values_groups_end, "xcb_xkb_set_names_values_groups_end");

   function xcb_xkb_set_names_values_key_names (S : System.Address) return access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:9924
   pragma Import (C, xcb_xkb_set_names_values_key_names, "xcb_xkb_set_names_values_key_names");

   function xcb_xkb_set_names_values_key_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9937
   pragma Import (C, xcb_xkb_set_names_values_key_names_length, "xcb_xkb_set_names_values_key_names_length");

   function xcb_xkb_set_names_values_key_names_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_name_iterator_t;  -- /usr/include/xcb/xkb.h:9951
   pragma Import (C, xcb_xkb_set_names_values_key_names_iterator, "xcb_xkb_set_names_values_key_names_iterator");

   function xcb_xkb_set_names_values_key_aliases (S : System.Address) return access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:9965
   pragma Import (C, xcb_xkb_set_names_values_key_aliases, "xcb_xkb_set_names_values_key_aliases");

   function xcb_xkb_set_names_values_key_aliases_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:9978
   pragma Import (C, xcb_xkb_set_names_values_key_aliases_length, "xcb_xkb_set_names_values_key_aliases_length");

   function xcb_xkb_set_names_values_key_aliases_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_alias_iterator_t;  -- /usr/include/xcb/xkb.h:9992
   pragma Import (C, xcb_xkb_set_names_values_key_aliases_iterator, "xcb_xkb_set_names_values_key_aliases_iterator");

   function xcb_xkb_set_names_values_radio_group_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:10006
   pragma Import (C, xcb_xkb_set_names_values_radio_group_names, "xcb_xkb_set_names_values_radio_group_names");

   function xcb_xkb_set_names_values_radio_group_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10019
   pragma Import (C, xcb_xkb_set_names_values_radio_group_names_length, "xcb_xkb_set_names_values_radio_group_names_length");

   function xcb_xkb_set_names_values_radio_group_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:10033
   pragma Import (C, xcb_xkb_set_names_values_radio_group_names_end, "xcb_xkb_set_names_values_radio_group_names_end");

   function xcb_xkb_set_names_values_serialize
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:10037
   pragma Import (C, xcb_xkb_set_names_values_serialize, "xcb_xkb_set_names_values_serialize");

   function xcb_xkb_set_names_values_unpack
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t;
      u_aux : access xcb_xkb_set_names_values_t) return int;  -- /usr/include/xcb/xkb.h:10050
   pragma Import (C, xcb_xkb_set_names_values_unpack, "xcb_xkb_set_names_values_unpack");

   function xcb_xkb_set_names_values_sizeof
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t) return int;  -- /usr/include/xcb/xkb.h:10063
   pragma Import (C, xcb_xkb_set_names_values_sizeof, "xcb_xkb_set_names_values_sizeof");

   function xcb_xkb_set_names_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      virtualMods : Libc.Stdint.uint16_t;
      which : Libc.Stdint.uint32_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKTLevelt : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      groupNames : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      firstKey : XCB.XProto.xcb_keycode_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      totalKTLevelNames : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:10111
   pragma Import (C, xcb_xkb_set_names_checked, "xcb_xkb_set_names_checked");

   function xcb_xkb_set_names
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      virtualMods : Libc.Stdint.uint16_t;
      which : Libc.Stdint.uint32_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKTLevelt : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      groupNames : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      firstKey : XCB.XProto.xcb_keycode_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      totalKTLevelNames : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:10162
   pragma Import (C, xcb_xkb_set_names, "xcb_xkb_set_names");

   function xcb_xkb_set_names_aux_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      virtualMods : Libc.Stdint.uint16_t;
      which : Libc.Stdint.uint32_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKTLevelt : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      groupNames : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      firstKey : XCB.XProto.xcb_keycode_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      totalKTLevelNames : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:10216
   pragma Import (C, xcb_xkb_set_names_aux_checked, "xcb_xkb_set_names_aux_checked");

   function xcb_xkb_set_names_aux
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      virtualMods : Libc.Stdint.uint16_t;
      which : Libc.Stdint.uint32_t;
      firstType : Libc.Stdint.uint8_t;
      nTypes : Libc.Stdint.uint8_t;
      firstKTLevelt : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint8_t;
      indicators : Libc.Stdint.uint32_t;
      groupNames : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      firstKey : XCB.XProto.xcb_keycode_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      totalKTLevelNames : Libc.Stdint.uint16_t;
      values : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:10267
   pragma Import (C, xcb_xkb_set_names_aux, "xcb_xkb_set_names_aux");

   function xcb_xkb_per_client_flags
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      change : Libc.Stdint.uint32_t;
      value : Libc.Stdint.uint32_t;
      ctrlsToChange : Libc.Stdint.uint32_t;
      autoCtrls : Libc.Stdint.uint32_t;
      autoCtrlsValues : Libc.Stdint.uint32_t) return xcb_xkb_per_client_flags_cookie_t;  -- /usr/include/xcb/xkb.h:10309
   pragma Import (C, xcb_xkb_per_client_flags, "xcb_xkb_per_client_flags");

   function xcb_xkb_per_client_flags_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      change : Libc.Stdint.uint32_t;
      value : Libc.Stdint.uint32_t;
      ctrlsToChange : Libc.Stdint.uint32_t;
      autoCtrls : Libc.Stdint.uint32_t;
      autoCtrlsValues : Libc.Stdint.uint32_t) return xcb_xkb_per_client_flags_cookie_t;  -- /usr/include/xcb/xkb.h:10345
   pragma Import (C, xcb_xkb_per_client_flags_unchecked, "xcb_xkb_per_client_flags_unchecked");

   function xcb_xkb_per_client_flags_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_per_client_flags_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_per_client_flags_reply_t;  -- /usr/include/xcb/xkb.h:10380
   pragma Import (C, xcb_xkb_per_client_flags_reply, "xcb_xkb_per_client_flags_reply");

   function xcb_xkb_list_components_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:10385
   pragma Import (C, xcb_xkb_list_components_sizeof, "xcb_xkb_list_components_sizeof");

   function xcb_xkb_list_components
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      maxNames : Libc.Stdint.uint16_t) return xcb_xkb_list_components_cookie_t;  -- /usr/include/xcb/xkb.h:10408
   pragma Import (C, xcb_xkb_list_components, "xcb_xkb_list_components");

   function xcb_xkb_list_components_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      maxNames : Libc.Stdint.uint16_t) return xcb_xkb_list_components_cookie_t;  -- /usr/include/xcb/xkb.h:10436
   pragma Import (C, xcb_xkb_list_components_unchecked, "xcb_xkb_list_components_unchecked");

   function xcb_xkb_list_components_keymaps_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:10451
   pragma Import (C, xcb_xkb_list_components_keymaps_length, "xcb_xkb_list_components_keymaps_length");

   function xcb_xkb_list_components_keymaps_iterator (R : System.Address) return xcb_xkb_listing_iterator_t;  -- /usr/include/xcb/xkb.h:10464
   pragma Import (C, xcb_xkb_list_components_keymaps_iterator, "xcb_xkb_list_components_keymaps_iterator");

   function xcb_xkb_list_components_keycodes_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:10477
   pragma Import (C, xcb_xkb_list_components_keycodes_length, "xcb_xkb_list_components_keycodes_length");

   function xcb_xkb_list_components_keycodes_iterator (R : System.Address) return xcb_xkb_listing_iterator_t;  -- /usr/include/xcb/xkb.h:10490
   pragma Import (C, xcb_xkb_list_components_keycodes_iterator, "xcb_xkb_list_components_keycodes_iterator");

   function xcb_xkb_list_components_types_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:10503
   pragma Import (C, xcb_xkb_list_components_types_length, "xcb_xkb_list_components_types_length");

   function xcb_xkb_list_components_types_iterator (R : System.Address) return xcb_xkb_listing_iterator_t;  -- /usr/include/xcb/xkb.h:10516
   pragma Import (C, xcb_xkb_list_components_types_iterator, "xcb_xkb_list_components_types_iterator");

   function xcb_xkb_list_components_compat_maps_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:10529
   pragma Import (C, xcb_xkb_list_components_compat_maps_length, "xcb_xkb_list_components_compat_maps_length");

   function xcb_xkb_list_components_compat_maps_iterator (R : System.Address) return xcb_xkb_listing_iterator_t;  -- /usr/include/xcb/xkb.h:10542
   pragma Import (C, xcb_xkb_list_components_compat_maps_iterator, "xcb_xkb_list_components_compat_maps_iterator");

   function xcb_xkb_list_components_symbols_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:10555
   pragma Import (C, xcb_xkb_list_components_symbols_length, "xcb_xkb_list_components_symbols_length");

   function xcb_xkb_list_components_symbols_iterator (R : System.Address) return xcb_xkb_listing_iterator_t;  -- /usr/include/xcb/xkb.h:10568
   pragma Import (C, xcb_xkb_list_components_symbols_iterator, "xcb_xkb_list_components_symbols_iterator");

   function xcb_xkb_list_components_geometries_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:10581
   pragma Import (C, xcb_xkb_list_components_geometries_length, "xcb_xkb_list_components_geometries_length");

   function xcb_xkb_list_components_geometries_iterator (R : System.Address) return xcb_xkb_listing_iterator_t;  -- /usr/include/xcb/xkb.h:10594
   pragma Import (C, xcb_xkb_list_components_geometries_iterator, "xcb_xkb_list_components_geometries_iterator");

   function xcb_xkb_list_components_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_list_components_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_list_components_reply_t;  -- /usr/include/xcb/xkb.h:10623
   pragma Import (C, xcb_xkb_list_components_reply, "xcb_xkb_list_components_reply");

   function xcb_xkb_get_kbd_by_name_replies_types_map_types_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10638
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_types_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_types_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_types_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_type_iterator_t;  -- /usr/include/xcb/xkb.h:10652
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_types_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_types_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_syms_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10666
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_syms_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_syms_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_syms_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_sym_map_iterator_t;  -- /usr/include/xcb/xkb.h:10680
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_syms_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_syms_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:10694
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count, "xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count");

   function xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10707
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count_length, "xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:10721
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count_end, "xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_count_end");

   function xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts (S : System.Address) return access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:10735
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts, "xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts");

   function xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10748
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts_length, "xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts_iterator (R : System.Address; S : System.Address) return xcb_xkb_action_iterator_t;  -- /usr/include/xcb/xkb.h:10762
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_acts_rtrn_acts_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn (S : System.Address) return access xcb_xkb_set_behavior_t;  -- /usr/include/xcb/xkb.h:10776
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn, "xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10789
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_behavior_iterator_t;  -- /usr/include/xcb/xkb.h:10803
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_behaviors_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:10817
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn, "xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10830
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:10844
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn_end, "xcb_xkb_get_kbd_by_name_replies_types_map_vmods_rtrn_end");

   function xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn (S : System.Address) return access xcb_xkb_set_explicit_t;  -- /usr/include/xcb/xkb.h:10858
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn, "xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10871
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_set_explicit_iterator_t;  -- /usr/include/xcb/xkb.h:10885
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_explicit_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn (S : System.Address) return access xcb_xkb_key_mod_map_t;  -- /usr/include/xcb/xkb.h:10899
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn, "xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10912
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_mod_map_iterator_t;  -- /usr/include/xcb/xkb.h:10926
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_modmap_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn (S : System.Address) return access xcb_xkb_key_v_mod_map_t;  -- /usr/include/xcb/xkb.h:10940
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn, "xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:10953
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_v_mod_map_iterator_t;  -- /usr/include/xcb/xkb.h:10967
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_types_map_vmodmap_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_types_map_serialize
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:10971
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_serialize, "xcb_xkb_get_kbd_by_name_replies_types_map_serialize");

   function xcb_xkb_get_kbd_by_name_replies_types_map_unpack
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t;
      u_aux : access xcb_xkb_get_kbd_by_name_replies_types_map_t) return int;  -- /usr/include/xcb/xkb.h:10985
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_unpack, "xcb_xkb_get_kbd_by_name_replies_types_map_unpack");

   function xcb_xkb_get_kbd_by_name_replies_types_map_sizeof
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKeySyms : Libc.Stdint.uint8_t;
      nKeyActions : Libc.Stdint.uint8_t;
      totalActions : Libc.Stdint.uint16_t;
      totalKeyBehaviors : Libc.Stdint.uint8_t;
      virtualMods : Libc.Stdint.uint16_t;
      totalKeyExplicit : Libc.Stdint.uint8_t;
      totalModMapKeys : Libc.Stdint.uint8_t;
      totalVModMapKeys : Libc.Stdint.uint8_t;
      present : Libc.Stdint.uint16_t) return int;  -- /usr/include/xcb/xkb.h:10999
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_types_map_sizeof, "xcb_xkb_get_kbd_by_name_replies_types_map_sizeof");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:11022
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11035
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11049
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_type_names_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type (S : System.Address) return access Libc.Stdint.uint8_t;  -- /usr/include/xcb/xkb.h:11063
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11076
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11090
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_n_levels_per_type_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:11104
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11117
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11131
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_kt_level_names_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:11145
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11158
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11172
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_indicator_names_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:11186
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11199
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11213
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_virtual_mod_names_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:11227
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11240
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11254
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_groups_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names (S : System.Address) return access xcb_xkb_key_name_t;  -- /usr/include/xcb/xkb.h:11268
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11281
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_name_iterator_t;  -- /usr/include/xcb/xkb.h:11295
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names_iterator, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_names_iterator");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases (S : System.Address) return access xcb_xkb_key_alias_t;  -- /usr/include/xcb/xkb.h:11309
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11322
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases_iterator (R : System.Address; S : System.Address) return xcb_xkb_key_alias_iterator_t;  -- /usr/include/xcb/xkb.h:11336
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases_iterator, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_key_aliases_iterator");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names (S : System.Address) return access XCB.XProto.xcb_atom_t;  -- /usr/include/xcb/xkb.h:11350
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11363
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names_length, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names_length");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names_end (R : System.Address; S : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11377
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names_end, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_radio_group_names_end");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_serialize
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint16_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:11381
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_serialize, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_serialize");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_unpack
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint16_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t;
      u_aux : access xcb_xkb_get_kbd_by_name_replies_key_names_value_list_t) return int;  -- /usr/include/xcb/xkb.h:11394
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_unpack, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_unpack");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list_sizeof
     (u_buffer : System.Address;
      nTypes : Libc.Stdint.uint8_t;
      nKTLevels : Libc.Stdint.uint16_t;
      indicators : Libc.Stdint.uint32_t;
      virtualMods : Libc.Stdint.uint16_t;
      groupNames : Libc.Stdint.uint8_t;
      nKeys : Libc.Stdint.uint8_t;
      nKeyAliases : Libc.Stdint.uint8_t;
      nRadioGroups : Libc.Stdint.uint8_t;
      which : Libc.Stdint.uint32_t) return int;  -- /usr/include/xcb/xkb.h:11407
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list_sizeof, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list_sizeof");

   function xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn (S : System.Address) return access xcb_xkb_sym_interpret_t;  -- /usr/include/xcb/xkb.h:11429
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn, "xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11442
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_sym_interpret_iterator_t;  -- /usr/include/xcb/xkb.h:11456
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_compat_map_si_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn (S : System.Address) return access xcb_xkb_mod_def_t;  -- /usr/include/xcb/xkb.h:11470
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn, "xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn");

   function xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11483
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn_length, "xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn_length");

   function xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn_iterator (R : System.Address; S : System.Address) return xcb_xkb_mod_def_iterator_t;  -- /usr/include/xcb/xkb.h:11497
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn_iterator, "xcb_xkb_get_kbd_by_name_replies_compat_map_group_rtrn_iterator");

   function xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps (S : System.Address) return access xcb_xkb_indicator_map_t;  -- /usr/include/xcb/xkb.h:11511
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps, "xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps");

   function xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps_length (R : System.Address; S : System.Address) return int;  -- /usr/include/xcb/xkb.h:11524
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps_length, "xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps_length");

   function xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps_iterator (R : System.Address; S : System.Address) return xcb_xkb_indicator_map_iterator_t;  -- /usr/include/xcb/xkb.h:11538
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps_iterator, "xcb_xkb_get_kbd_by_name_replies_indicator_maps_maps_iterator");

   function xcb_xkb_get_kbd_by_name_replies_key_names_value_list (R : System.Address) return access xcb_xkb_get_kbd_by_name_replies_key_names_value_list_t;  -- /usr/include/xcb/xkb.h:11552
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_key_names_value_list, "xcb_xkb_get_kbd_by_name_replies_key_names_value_list");

   function xcb_xkb_get_kbd_by_name_replies_geometry_label_font (R : System.Address) return access xcb_xkb_counted_string_16_t;  -- /usr/include/xcb/xkb.h:11565
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_geometry_label_font, "xcb_xkb_get_kbd_by_name_replies_geometry_label_font");

   function xcb_xkb_get_kbd_by_name_replies_serialize
     (u_buffer : System.Address;
      reported : Libc.Stdint.uint16_t;
      u_aux : System.Address) return int;  -- /usr/include/xcb/xkb.h:11568
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_serialize, "xcb_xkb_get_kbd_by_name_replies_serialize");

   function xcb_xkb_get_kbd_by_name_replies_unpack
     (u_buffer : System.Address;
      reported : Libc.Stdint.uint16_t;
      u_aux : access xcb_xkb_get_kbd_by_name_replies_t) return int;  -- /usr/include/xcb/xkb.h:11573
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_unpack, "xcb_xkb_get_kbd_by_name_replies_unpack");

   function xcb_xkb_get_kbd_by_name_replies_sizeof (u_buffer : System.Address; reported : Libc.Stdint.uint16_t) return int;  -- /usr/include/xcb/xkb.h:11578
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies_sizeof, "xcb_xkb_get_kbd_by_name_replies_sizeof");

   function xcb_xkb_get_kbd_by_name
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      need : Libc.Stdint.uint16_t;
      want : Libc.Stdint.uint16_t;
      load : Libc.Stdint.uint8_t) return xcb_xkb_get_kbd_by_name_cookie_t;  -- /usr/include/xcb/xkb.h:11604
   pragma Import (C, xcb_xkb_get_kbd_by_name, "xcb_xkb_get_kbd_by_name");

   function xcb_xkb_get_kbd_by_name_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      need : Libc.Stdint.uint16_t;
      want : Libc.Stdint.uint16_t;
      load : Libc.Stdint.uint8_t) return xcb_xkb_get_kbd_by_name_cookie_t;  -- /usr/include/xcb/xkb.h:11636
   pragma Import (C, xcb_xkb_get_kbd_by_name_unchecked, "xcb_xkb_get_kbd_by_name_unchecked");

   function xcb_xkb_get_kbd_by_name_replies (R : System.Address) return System.Address;  -- /usr/include/xcb/xkb.h:11653
   pragma Import (C, xcb_xkb_get_kbd_by_name_replies, "xcb_xkb_get_kbd_by_name_replies");

   function xcb_xkb_get_kbd_by_name_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_kbd_by_name_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_kbd_by_name_reply_t;  -- /usr/include/xcb/xkb.h:11682
   pragma Import (C, xcb_xkb_get_kbd_by_name_reply, "xcb_xkb_get_kbd_by_name_reply");

   function xcb_xkb_get_device_info_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:11687
   pragma Import (C, xcb_xkb_get_device_info_sizeof, "xcb_xkb_get_device_info_sizeof");

   function xcb_xkb_get_device_info
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      wanted : Libc.Stdint.uint16_t;
      allButtons : Libc.Stdint.uint8_t;
      firstButton : Libc.Stdint.uint8_t;
      nButtons : Libc.Stdint.uint8_t;
      ledClass : xcb_xkb_led_class_spec_t;
      ledID : xcb_xkb_id_spec_t) return xcb_xkb_get_device_info_cookie_t;  -- /usr/include/xcb/xkb.h:11715
   pragma Import (C, xcb_xkb_get_device_info, "xcb_xkb_get_device_info");

   function xcb_xkb_get_device_info_unchecked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      wanted : Libc.Stdint.uint16_t;
      allButtons : Libc.Stdint.uint8_t;
      firstButton : Libc.Stdint.uint8_t;
      nButtons : Libc.Stdint.uint8_t;
      ledClass : xcb_xkb_led_class_spec_t;
      ledID : xcb_xkb_id_spec_t) return xcb_xkb_get_device_info_cookie_t;  -- /usr/include/xcb/xkb.h:11753
   pragma Import (C, xcb_xkb_get_device_info_unchecked, "xcb_xkb_get_device_info_unchecked");

   function xcb_xkb_get_device_info_name (R : System.Address) return access xcb_xkb_string8_t;  -- /usr/include/xcb/xkb.h:11773
   pragma Import (C, xcb_xkb_get_device_info_name, "xcb_xkb_get_device_info_name");

   function xcb_xkb_get_device_info_name_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:11786
   pragma Import (C, xcb_xkb_get_device_info_name_length, "xcb_xkb_get_device_info_name_length");

   function xcb_xkb_get_device_info_name_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xkb.h:11799
   pragma Import (C, xcb_xkb_get_device_info_name_end, "xcb_xkb_get_device_info_name_end");

   function xcb_xkb_get_device_info_btn_actions (R : System.Address) return access xcb_xkb_action_t;  -- /usr/include/xcb/xkb.h:11812
   pragma Import (C, xcb_xkb_get_device_info_btn_actions, "xcb_xkb_get_device_info_btn_actions");

   function xcb_xkb_get_device_info_btn_actions_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:11825
   pragma Import (C, xcb_xkb_get_device_info_btn_actions_length, "xcb_xkb_get_device_info_btn_actions_length");

   function xcb_xkb_get_device_info_btn_actions_iterator (R : System.Address) return xcb_xkb_action_iterator_t;  -- /usr/include/xcb/xkb.h:11838
   pragma Import (C, xcb_xkb_get_device_info_btn_actions_iterator, "xcb_xkb_get_device_info_btn_actions_iterator");

   function xcb_xkb_get_device_info_leds_length (R : System.Address) return int;  -- /usr/include/xcb/xkb.h:11851
   pragma Import (C, xcb_xkb_get_device_info_leds_length, "xcb_xkb_get_device_info_leds_length");

   function xcb_xkb_get_device_info_leds_iterator (R : System.Address) return xcb_xkb_device_led_info_iterator_t;  -- /usr/include/xcb/xkb.h:11864
   pragma Import (C, xcb_xkb_get_device_info_leds_iterator, "xcb_xkb_get_device_info_leds_iterator");

   function xcb_xkb_get_device_info_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_get_device_info_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_get_device_info_reply_t;  -- /usr/include/xcb/xkb.h:11893
   pragma Import (C, xcb_xkb_get_device_info_reply, "xcb_xkb_get_device_info_reply");

   function xcb_xkb_set_device_info_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:11898
   pragma Import (C, xcb_xkb_set_device_info_sizeof, "xcb_xkb_set_device_info_sizeof");

   function xcb_xkb_set_device_info_checked
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      firstBtn : Libc.Stdint.uint8_t;
      nBtns : Libc.Stdint.uint8_t;
      change : Libc.Stdint.uint16_t;
      nDeviceLedFBs : Libc.Stdint.uint16_t;
      btnActions : System.Address;
      leds : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:11929
   pragma Import (C, xcb_xkb_set_device_info_checked, "xcb_xkb_set_device_info_checked");

   function xcb_xkb_set_device_info
     (c : xcb_connection_t_access;
      deviceSpec : xcb_xkb_device_spec_t;
      firstBtn : Libc.Stdint.uint8_t;
      nBtns : Libc.Stdint.uint8_t;
      change : Libc.Stdint.uint16_t;
      nDeviceLedFBs : Libc.Stdint.uint16_t;
      btnActions : System.Address;
      leds : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xkb.h:11964
   pragma Import (C, xcb_xkb_set_device_info, "xcb_xkb_set_device_info");

   function xcb_xkb_set_debugging_flags_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xkb.h:11974
   pragma Import (C, xcb_xkb_set_debugging_flags_sizeof, "xcb_xkb_set_debugging_flags_sizeof");

   function xcb_xkb_set_debugging_flags
     (c : xcb_connection_t_access;
      msgLength : Libc.Stdint.uint16_t;
      affectFlags : Libc.Stdint.uint32_t;
      flags : Libc.Stdint.uint32_t;
      affectCtrls : Libc.Stdint.uint32_t;
      ctrls : Libc.Stdint.uint32_t;
      message : access xcb_xkb_string8_t) return xcb_xkb_set_debugging_flags_cookie_t;  -- /usr/include/xcb/xkb.h:12001
   pragma Import (C, xcb_xkb_set_debugging_flags, "xcb_xkb_set_debugging_flags");

   function xcb_xkb_set_debugging_flags_unchecked
     (c : xcb_connection_t_access;
      msgLength : Libc.Stdint.uint16_t;
      affectFlags : Libc.Stdint.uint32_t;
      flags : Libc.Stdint.uint32_t;
      affectCtrls : Libc.Stdint.uint32_t;
      ctrls : Libc.Stdint.uint32_t;
      message : access xcb_xkb_string8_t) return xcb_xkb_set_debugging_flags_cookie_t;  -- /usr/include/xcb/xkb.h:12037
   pragma Import (C, xcb_xkb_set_debugging_flags_unchecked, "xcb_xkb_set_debugging_flags_unchecked");

   function xcb_xkb_set_debugging_flags_reply
     (c : xcb_connection_t_access;
      cookie : xcb_xkb_set_debugging_flags_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_xkb_set_debugging_flags_reply_t;  -- /usr/include/xcb/xkb.h:12072
   pragma Import (C, xcb_xkb_set_debugging_flags_reply, "xcb_xkb_set_debugging_flags_reply");

end XCB.XKB;
