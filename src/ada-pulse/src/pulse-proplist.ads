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

package Pulse.Proplist is

   --  unsupported macro: PA_PROP_MEDIA_NAME "media.name"
   --  unsupported macro: PA_PROP_MEDIA_TITLE "media.title"
   --  unsupported macro: PA_PROP_MEDIA_ARTIST "media.artist"
   --  unsupported macro: PA_PROP_MEDIA_COPYRIGHT "media.copyright"
   --  unsupported macro: PA_PROP_MEDIA_SOFTWARE "media.software"
   --  unsupported macro: PA_PROP_MEDIA_LANGUAGE "media.language"
   --  unsupported macro: PA_PROP_MEDIA_FILENAME "media.filename"
   --  unsupported macro: PA_PROP_MEDIA_ICON "media.icon"
   --  unsupported macro: PA_PROP_MEDIA_ICON_NAME "media.icon_name"
   --  unsupported macro: PA_PROP_MEDIA_ROLE "media.role"
   --  unsupported macro: PA_PROP_FILTER_WANT "filter.want"
   --  unsupported macro: PA_PROP_FILTER_APPLY "filter.apply"
   --  unsupported macro: PA_PROP_FILTER_SUPPRESS "filter.suppress"
   --  unsupported macro: PA_PROP_EVENT_ID "event.id"
   --  unsupported macro: PA_PROP_EVENT_DESCRIPTION "event.description"
   --  unsupported macro: PA_PROP_EVENT_MOUSE_X "event.mouse.x"
   --  unsupported macro: PA_PROP_EVENT_MOUSE_Y "event.mouse.y"
   --  unsupported macro: PA_PROP_EVENT_MOUSE_HPOS "event.mouse.hpos"
   --  unsupported macro: PA_PROP_EVENT_MOUSE_VPOS "event.mouse.vpos"
   --  unsupported macro: PA_PROP_EVENT_MOUSE_BUTTON "event.mouse.button"
   --  unsupported macro: PA_PROP_WINDOW_NAME "window.name"
   --  unsupported macro: PA_PROP_WINDOW_ID "window.id"
   --  unsupported macro: PA_PROP_WINDOW_ICON "window.icon"
   --  unsupported macro: PA_PROP_WINDOW_ICON_NAME "window.icon_name"
   --  unsupported macro: PA_PROP_WINDOW_X "window.x"
   --  unsupported macro: PA_PROP_WINDOW_Y "window.y"
   --  unsupported macro: PA_PROP_WINDOW_WIDTH "window.width"
   --  unsupported macro: PA_PROP_WINDOW_HEIGHT "window.height"
   --  unsupported macro: PA_PROP_WINDOW_HPOS "window.hpos"
   --  unsupported macro: PA_PROP_WINDOW_VPOS "window.vpos"
   --  unsupported macro: PA_PROP_WINDOW_DESKTOP "window.desktop"
   --  unsupported macro: PA_PROP_WINDOW_X11_DISPLAY "window.x11.display"
   --  unsupported macro: PA_PROP_WINDOW_X11_SCREEN "window.x11.screen"
   --  unsupported macro: PA_PROP_WINDOW_X11_MONITOR "window.x11.monitor"
   --  unsupported macro: PA_PROP_WINDOW_X11_XID "window.x11.xid"
   --  unsupported macro: PA_PROP_APPLICATION_NAME "application.name"
   --  unsupported macro: PA_PROP_APPLICATION_ID "application.id"
   --  unsupported macro: PA_PROP_APPLICATION_VERSION "application.version"
   --  unsupported macro: PA_PROP_APPLICATION_ICON "application.icon"
   --  unsupported macro: PA_PROP_APPLICATION_ICON_NAME "application.icon_name"
   --  unsupported macro: PA_PROP_APPLICATION_LANGUAGE "application.language"
   --  unsupported macro: PA_PROP_APPLICATION_PROCESS_ID "application.process.id"
   --  unsupported macro: PA_PROP_APPLICATION_PROCESS_BINARY "application.process.binary"
   --  unsupported macro: PA_PROP_APPLICATION_PROCESS_USER "application.process.user"
   --  unsupported macro: PA_PROP_APPLICATION_PROCESS_HOST "application.process.host"
   --  unsupported macro: PA_PROP_APPLICATION_PROCESS_MACHINE_ID "application.process.machine_id"
   --  unsupported macro: PA_PROP_APPLICATION_PROCESS_SESSION_ID "application.process.session_id"
   --  unsupported macro: PA_PROP_DEVICE_STRING "device.string"
   --  unsupported macro: PA_PROP_DEVICE_API "device.api"
   --  unsupported macro: PA_PROP_DEVICE_DESCRIPTION "device.description"
   --  unsupported macro: PA_PROP_DEVICE_BUS_PATH "device.bus_path"
   --  unsupported macro: PA_PROP_DEVICE_SERIAL "device.serial"
   --  unsupported macro: PA_PROP_DEVICE_VENDOR_ID "device.vendor.id"
   --  unsupported macro: PA_PROP_DEVICE_VENDOR_NAME "device.vendor.name"
   --  unsupported macro: PA_PROP_DEVICE_PRODUCT_ID "device.product.id"
   --  unsupported macro: PA_PROP_DEVICE_PRODUCT_NAME "device.product.name"
   --  unsupported macro: PA_PROP_DEVICE_CLASS "device.class"
   --  unsupported macro: PA_PROP_DEVICE_FORM_FACTOR "device.form_factor"
   --  unsupported macro: PA_PROP_DEVICE_BUS "device.bus"
   --  unsupported macro: PA_PROP_DEVICE_ICON "device.icon"
   --  unsupported macro: PA_PROP_DEVICE_ICON_NAME "device.icon_name"
   --  unsupported macro: PA_PROP_DEVICE_ACCESS_MODE "device.access_mode"
   --  unsupported macro: PA_PROP_DEVICE_MASTER_DEVICE "device.master_device"
   --  unsupported macro: PA_PROP_DEVICE_BUFFERING_BUFFER_SIZE "device.buffering.buffer_size"
   --  unsupported macro: PA_PROP_DEVICE_BUFFERING_FRAGMENT_SIZE "device.buffering.fragment_size"
   --  unsupported macro: PA_PROP_DEVICE_PROFILE_NAME "device.profile.name"
   --  unsupported macro: PA_PROP_DEVICE_INTENDED_ROLES "device.intended_roles"
   --  unsupported macro: PA_PROP_DEVICE_PROFILE_DESCRIPTION "device.profile.description"
   --  unsupported macro: PA_PROP_MODULE_AUTHOR "module.author"
   --  unsupported macro: PA_PROP_MODULE_DESCRIPTION "module.description"
   --  unsupported macro: PA_PROP_MODULE_USAGE "module.usage"
   --  unsupported macro: PA_PROP_MODULE_VERSION "module.version"
   --  unsupported macro: PA_PROP_FORMAT_SAMPLE_FORMAT "format.sample_format"
   --  unsupported macro: PA_PROP_FORMAT_RATE "format.rate"
   --  unsupported macro: PA_PROP_FORMAT_CHANNELS "format.channels"
   --  unsupported macro: PA_PROP_FORMAT_CHANNEL_MAP "format.channel_map"
   --  unsupported macro: PA_UPDATE_SET PA_UPDATE_SET
   --  unsupported macro: PA_UPDATE_MERGE PA_UPDATE_MERGE
   --  unsupported macro: PA_UPDATE_REPLACE PA_UPDATE_REPLACE

   type pa_proplist is limited private;
   type pa_proplist_access is access all pa_proplist;

   function pa_proplist_new return pa_proplist_access;  -- /usr/include/pulse/proplist.h:277
   pragma Import (C, pa_proplist_new, "pa_proplist_new");

   procedure pa_proplist_free (p : pa_proplist_access);  -- /usr/include/pulse/proplist.h:280
   pragma Import (C, pa_proplist_free, "pa_proplist_free");

   function pa_proplist_key_valid (key : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/pulse/proplist.h:283
   pragma Import (C, pa_proplist_key_valid, "pa_proplist_key_valid");

   function pa_proplist_sets
     (p : pa_proplist_access;
      key : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/pulse/proplist.h:289
   pragma Import (C, pa_proplist_sets, "pa_proplist_sets");

   function pa_proplist_setp (p : pa_proplist_access; pair : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/pulse/proplist.h:297
   pragma Import (C, pa_proplist_setp, "pa_proplist_setp");

   function pa_proplist_setf
     (p : pa_proplist_access;
      key : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/pulse/proplist.h:304
   pragma Import (C, pa_proplist_setf, "pa_proplist_setf");

   function pa_proplist_set
     (p : pa_proplist_access;
      key : Interfaces.C.Strings.chars_ptr;
      data : System.Address;
      nbytes : Libc.Stddef.size_t) return int;  -- /usr/include/pulse/proplist.h:309
   pragma Import (C, pa_proplist_set, "pa_proplist_set");

   function pa_proplist_gets (p : pa_proplist_access; key : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/proplist.h:315
   pragma Import (C, pa_proplist_gets, "pa_proplist_gets");

   function pa_proplist_get
     (p : pa_proplist_access;
      key : Interfaces.C.Strings.chars_ptr;
      data : System.Address;
      nbytes : access Libc.Stddef.size_t) return int;  -- /usr/include/pulse/proplist.h:322
   pragma Import (C, pa_proplist_get, "pa_proplist_get");

   type pa_update_mode is
     (PA_UPDATE_SET,
      PA_UPDATE_MERGE,
      PA_UPDATE_REPLACE);
   pragma Convention (C, pa_update_mode);  -- /usr/include/pulse/proplist.h:325

   subtype pa_update_mode_t is pa_update_mode;

   procedure pa_proplist_update
     (p : pa_proplist_access;
      mode : pa_update_mode_t;
      other : System.Address);  -- /usr/include/pulse/proplist.h:349
   pragma Import (C, pa_proplist_update, "pa_proplist_update");

   function pa_proplist_unset (p : pa_proplist_access; key : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/pulse/proplist.h:353
   pragma Import (C, pa_proplist_unset, "pa_proplist_unset");

   function pa_proplist_unset_many (p : pa_proplist_access; keys : System.Address) return int;  -- /usr/include/pulse/proplist.h:360
   pragma Import (C, pa_proplist_unset_many, "pa_proplist_unset_many");

   function pa_proplist_iterate (p : pa_proplist_access; state : System.Address) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/proplist.h:371
   pragma Import (C, pa_proplist_iterate, "pa_proplist_iterate");

   function pa_proplisto_string (p : pa_proplist_access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/proplist.h:377
   pragma Import (C, pa_proplisto_string, "pa_proplisto_string");

   function pa_proplisto_string_sep (p : pa_proplist_access; sep : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/proplist.h:382
   pragma Import (C, pa_proplisto_string_sep, "pa_proplisto_string_sep");

   function pa_proplist_from_string (str : Interfaces.C.Strings.chars_ptr) return System.Address;  -- /usr/include/pulse/proplist.h:386
   pragma Import (C, pa_proplist_from_string, "pa_proplist_from_string");

   function pa_proplist_contains (p : pa_proplist_access; key : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/pulse/proplist.h:390
   pragma Import (C, pa_proplist_contains, "pa_proplist_contains");

   procedure pa_proplist_clear (p : pa_proplist_access);  -- /usr/include/pulse/proplist.h:393
   pragma Import (C, pa_proplist_clear, "pa_proplist_clear");

   function pa_proplist_copy (p : pa_proplist_access) return System.Address;  -- /usr/include/pulse/proplist.h:397
   pragma Import (C, pa_proplist_copy, "pa_proplist_copy");

   function pa_proplist_size (p : pa_proplist_access) return unsigned;  -- /usr/include/pulse/proplist.h:400
   pragma Import (C, pa_proplist_size, "pa_proplist_size");

   function pa_proplist_isempty (p : pa_proplist_access) return int;  -- /usr/include/pulse/proplist.h:403
   pragma Import (C, pa_proplist_isempty, "pa_proplist_isempty");

   function pa_proplist_equal (a : System.Address; b : System.Address) return int;  -- /usr/include/pulse/proplist.h:407
   pragma Import (C, pa_proplist_equal, "pa_proplist_equal");

private
   type pa_proplist is limited record null; end record;
end Pulse.Proplist;
