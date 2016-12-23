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
with XCB;
with XKB;

package XKB.X11 is
   pragma Preelaborate;
   pragma Link_With ("-lxkbcommon-x11");

   XKB_X11_MIN_MAJOR_XKB_VERSION : constant := 1;
   XKB_X11_MIN_MINOR_XKB_VERSION : constant := 0;

   type xkb_x11_setup_xkb_extension_flags is
     (XKB_X11_SETUP_XKB_EXTENSION_NO_FLAGS);
   pragma Convention
     (C,
      xkb_x11_setup_xkb_extension_flags);  -- /usr/include/xkbcommon/xkbcommon-x11.h:58

   function xkb_x11_setup_xkb_extension
     (connection : XCB.xcb_connection_t_access;
      major_xkb_version : Libc.Stdint.uint16_t;
      minor_xkb_version : Libc.Stdint.uint16_t;
      flags : xkb_x11_setup_xkb_extension_flags;
      major_xkb_version_out : access Libc.Stdint.uint16_t;
      minor_xkb_version_out : access Libc.Stdint.uint16_t;
      base_event_out : access Libc.Stdint.uint8_t;
      base_error_out : access Libc.Stdint.uint8_t)
     return int;  -- /usr/include/xkbcommon/xkbcommon-x11.h:98
   pragma Import
     (C,
      xkb_x11_setup_xkb_extension,
      "xkb_x11_setup_xkb_extension");

   function xkb_x11_get_core_keyboard_device_id
     (connection : XCB.xcb_connection_t)
     return Libc.Stdint.int32_t;  -- /usr/include/xkbcommon/xkbcommon-x11.h:116
   pragma Import
     (C,
      xkb_x11_get_core_keyboard_device_id,
      "xkb_x11_get_core_keyboard_device_id");

   function xkb_x11_keymap_new_from_device
     (context : XKB.xkb_context_access;
      connection : XCB.xcb_connection_t_access;
      device_id : Libc.Stdint.int32_t;
      flags : XKB.xkb_keymap_compile_flags)
     return XKB
       .xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon-x11.h:140
   pragma Import
     (C,
      xkb_x11_keymap_new_from_device,
      "xkb_x11_keymap_new_from_device");

   function xkb_x11_state_new_from_device
     (keymap : XKB.xkb_keymap_access;
      connection : XCB.xcb_connection_t_access;
      device_id : Libc.Stdint.int32_t)
     return XKB
       .xkb_state_access;  -- /usr/include/xkbcommon/xkbcommon-x11.h:164
   pragma Import
     (C,
      xkb_x11_state_new_from_device,
      "xkb_x11_state_new_from_device");

end XKB.X11;
