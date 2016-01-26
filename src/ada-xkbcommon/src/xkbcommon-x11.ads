pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with C99.Stdint;
with Xkbcommon;

package Xkbcommon.X11 is
   pragma Preelaborate;

   XKB_X11_MIN_MAJOR_XKB_VERSION : constant := 1;
   XKB_X11_MIN_MINOR_XKB_VERSION : constant := 0;

   type xkb_x11_setup_xkb_extension_flags is
     (XKB_X11_SETUP_XKB_EXTENSION_NO_FLAGS);
   pragma Convention (C, xkb_x11_setup_xkb_extension_flags);  -- /usr/include/xkbcommon/xkbcommon-x11.h:58

   function xkb_x11_setup_xkb_extension
     (connection : XCB.xcb_connection_t;
      major_xkb_version : Xkbcommon.stdint_h.uint16_t;
      minor_xkb_version : Xkbcommon.stdint_h.uint16_t;
      flags : xkb_x11_setup_xkb_extension_flags;
      major_xkb_version_out : access C99.Stdint.uint16_t;
      minor_xkb_version_out : access C99.Stdint.uint16_t;
      base_event_out : access C99.Stdint.uint8_t;
      base_error_out : access C99.Stdint.uint8_t) return int;  -- /usr/include/xkbcommon/xkbcommon-x11.h:98
   pragma Import (C, xkb_x11_setup_xkb_extension, "xkb_x11_setup_xkb_extension");

   function xkb_x11_get_core_keyboard_device_id (connection : XCB.xcb_connection_t) return C99.Stdint.int32_t;  -- /usr/include/xkbcommon/xkbcommon-x11.h:116
   pragma Import (C, xkb_x11_get_core_keyboard_device_id, "xkb_x11_get_core_keyboard_device_id");

   function xkb_x11_keymap_new_from_device
     (context : Xkbcommon.xkb_context_access;
      connection : XCB.xcb_connection_t;
      device_id : C99.Stdint.int32_t;
      flags : Xkbcommon.xkb_keymap_compile_flags) return xkbcommon.xkb_keymap_access;  -- /usr/include/xkbcommon/xkbcommon-x11.h:140
   pragma Import (C, xkb_x11_keymap_new_from_device, "xkb_x11_keymap_new_from_device");

   function xkb_x11_state_new_from_device
     (keymap : xkbcommon.xkb_keymap_access;
      connection : XCB.xcb_connection_t;
      device_id : C99.Stdint.int32_t) return Xkbcommon.xkb_state_access;  -- /usr/include/xkbcommon/xkbcommon-x11.h:164
   pragma Import (C, xkb_x11_state_new_from_device, "xkb_x11_state_new_from_device");

end Xkbcommon.X11;
