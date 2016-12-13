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
with System;
with Libc.Stdint;
with Interfaces.C.Strings;
limited with XCB.XProto;

package XCB with SPARK_Mode => Off is
   pragma Preelaborate;
   pragma Link_With ("-lxcb");

   X_PROTOCOL : constant := 11;
   X_PROTOCOL_REVISION : constant := 0;
   X_TCP_PORT : constant := 6000;

   XCB_CONN_ERROR : constant := 1;
   XCB_CONN_CLOSED_EXT_NOTSUPPORTED : constant := 2;
   XCB_CONN_CLOSED_MEM_INSUFFICIENT : constant := 3;
   XCB_CONN_CLOSED_REQ_LEN_EXCEED : constant := 4;
   XCB_CONN_CLOSED_PARSE_ERR : constant := 5;
   XCB_CONN_CLOSED_INVALID_SCREEN : constant := 6;
   XCB_CONN_CLOSED_FDPASSING_FAILED : constant := 7;

   XCB_NONE : constant := 0;
   XCB_COPY_FROM_PARENT : constant := 0;
   XCB_CURRENT_TIME : constant := 0;
   XCB_NO_SYMBOL : constant := 0;

   type xcb_connection_t is limited private;
   type xcb_connection_t_access is access all xcb_connection_t;

   type xcb_generic_iterator_t is record
      data : System.Address;  -- /usr/include/xcb/xcb.h:115
      c_rem : aliased int;  -- /usr/include/xcb/xcb.h:116
      index : aliased int;  -- /usr/include/xcb/xcb.h:117
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_generic_iterator_t);  -- /usr/include/xcb/xcb.h:118

   --  skipped anonymous struct anon_26

   type xcb_generic_reply_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:126
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:127
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:128
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:129
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_generic_reply_t);  -- /usr/include/xcb/xcb.h:130

   type xcb_generic_event_t_pad_array is array (0 .. 6) of aliased Libc.Stdint.uint32_t;
   type xcb_generic_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:138
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:139
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:140
      pad : aliased xcb_generic_event_t_pad_array;  -- /usr/include/xcb/xcb.h:141
      full_sequence : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:142
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_generic_event_t);  -- /usr/include/xcb/xcb.h:143

   type xcb_ge_event_t_pad_array is array (0 .. 4) of aliased Libc.Stdint.uint32_t;
   type xcb_ge_event_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:155
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:156
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:157
      length : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:158
      event_type : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:159
      pad1 : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:160
      pad : aliased xcb_ge_event_t_pad_array;  -- /usr/include/xcb/xcb.h:161
      full_sequence : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:162
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ge_event_t);  -- /usr/include/xcb/xcb.h:163

   type xcb_generic_error_t_pad_array is array (0 .. 4) of aliased Libc.Stdint.uint32_t;
   type xcb_generic_error_t is record
      response_type : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:171
      error_code : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:172
      sequence : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:173
      resource_id : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:174
      minor_code : aliased Libc.Stdint.uint16_t;  -- /usr/include/xcb/xcb.h:175
      major_code : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:176
      pad0 : aliased Libc.Stdint.uint8_t;  -- /usr/include/xcb/xcb.h:177
      pad : aliased xcb_generic_error_t_pad_array;  -- /usr/include/xcb/xcb.h:178
      full_sequence : aliased Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:179
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_generic_error_t);  -- /usr/include/xcb/xcb.h:180
   type xcb_generic_error_t_access is access all xcb_generic_error_t;

   type xcb_void_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xcb.h:188
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_void_cookie_t);  -- /usr/include/xcb/xcb.h:189

   type xcb_auth_info_t is record
      namelen : aliased int;  -- /usr/include/xcb/xcb.h:217
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xcb.h:218
      datalen : aliased int;  -- /usr/include/xcb/xcb.h:219
      data : Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xcb.h:220
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_auth_info_t);  -- /usr/include/xcb/xcb.h:216

   function xcb_flush (c : xcb_connection_t_access) return int;  -- /usr/include/xcb/xcb.h:234
   pragma Import (C, xcb_flush, "xcb_flush");

   function xcb_get_maximum_request_length (c : xcb_connection_t_access) return Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:251
   pragma Import (C, xcb_get_maximum_request_length, "xcb_get_maximum_request_length");

   procedure xcb_prefetch_maximum_request_length (c : xcb_connection_t_access);  -- /usr/include/xcb/xcb.h:270
   pragma Import (C, xcb_prefetch_maximum_request_length, "xcb_prefetch_maximum_request_length");

   function xcb_wait_for_event (c : xcb_connection_t_access) return access xcb_generic_event_t;  -- /usr/include/xcb/xcb.h:284
   pragma Import (C, xcb_wait_for_event, "xcb_wait_for_event");

   function xcb_poll_for_event (c : xcb_connection_t_access) return access xcb_generic_event_t;  -- /usr/include/xcb/xcb.h:297
   pragma Import (C, xcb_poll_for_event, "xcb_poll_for_event");

   function xcb_poll_for_queued_event (c : xcb_connection_t_access) return access xcb_generic_event_t;  -- /usr/include/xcb/xcb.h:313
   pragma Import (C, xcb_poll_for_queued_event, "xcb_poll_for_queued_event");

   type xcb_special_event_t is limited private;
   type xcb_special_event_t_access is access all xcb_special_event_t;

   function xcb_poll_for_special_event (c : xcb_connection_t_access; se : xcb_special_event_t) return access xcb_generic_event_t;  -- /usr/include/xcb/xcb.h:320
   pragma Import (C, xcb_poll_for_special_event, "xcb_poll_for_special_event");

   function xcb_wait_for_special_event (c : xcb_connection_t_access; se : xcb_special_event_t) return access xcb_generic_event_t;  -- /usr/include/xcb/xcb.h:326
   pragma Import (C, xcb_wait_for_special_event, "xcb_wait_for_special_event");

   type xcb_extension_t is limited private;
   type xcb_extension_t_access is access all xcb_extension_t;

   function xcb_register_for_special_xge
     (c : xcb_connection_t_access;
      ext : xcb_extension_t_access;
      eid : Libc.Stdint.uint32_t;
      stamp : access Libc.Stdint.uint32_t) return xcb_special_event_t_access;  -- /usr/include/xcb/xcb.h:337
   pragma Import (C, xcb_register_for_special_xge, "xcb_register_for_special_xge");

   procedure xcb_unregister_for_special_event (c : xcb_connection_t_access; se : xcb_special_event_t);  -- /usr/include/xcb/xcb.h:345
   pragma Import (C, xcb_unregister_for_special_event, "xcb_unregister_for_special_event");

   function xcb_request_check (c : xcb_connection_t_access; cookie : xcb_void_cookie_t) return access xcb_generic_error_t;  -- /usr/include/xcb/xcb.h:364
   pragma Import (C, xcb_request_check, "xcb_request_check");

   procedure xcb_discard_reply (c : xcb_connection_t_access; sequence : unsigned);  -- /usr/include/xcb/xcb.h:380
   pragma Import (C, xcb_discard_reply, "xcb_discard_reply");

   function xcb_get_extension_data (c : xcb_connection_t_access; ext : xcb_extension_t) return access constant XCB.XProto.xcb_query_extension_reply_t;  -- /usr/include/xcb/xcb.h:401
   pragma Import (C, xcb_get_extension_data, "xcb_get_extension_data");

   procedure xcb_prefetch_extension_data (c : xcb_connection_t_access; ext : xcb_extension_t);  -- /usr/include/xcb/xcb.h:414
   pragma Import (C, xcb_prefetch_extension_data, "xcb_prefetch_extension_data");

   function xcb_get_setup (c : xcb_connection_t_access) return access constant XCB.XProto.xcb_setup_t;  -- /usr/include/xcb/xcb.h:437
   pragma Import (C, xcb_get_setup, "xcb_get_setup");

   function xcb_get_file_descriptor (c : xcb_connection_t_access) return int;  -- /usr/include/xcb/xcb.h:447
   pragma Import (C, xcb_get_file_descriptor, "xcb_get_file_descriptor");

   function xcb_connection_has_error (c : xcb_connection_t_access) return int;  -- /usr/include/xcb/xcb.h:466
   pragma Import (C, xcb_connection_has_error, "xcb_connection_has_error");

   function xcb_connect_to_fd (fd : int; auth_info : access xcb_auth_info_t) return xcb_connection_t_access;  -- /usr/include/xcb/xcb.h:480
   pragma Import (C, xcb_connect_to_fd, "xcb_connect_to_fd");

   procedure xcb_disconnect (c : xcb_connection_t_access);  -- /usr/include/xcb/xcb.h:489
   pragma Import (C, xcb_disconnect, "xcb_disconnect");

   function xcb_parse_display
     (name : Interfaces.C.Strings.chars_ptr;
      host : System.Address;
      display : access int;
      screen : access int) return int;  -- /usr/include/xcb/xcb.h:511
   pragma Import (C, xcb_parse_display, "xcb_parse_display");

   function xcb_connect (displayname : Interfaces.C.Strings.chars_ptr; screenp : access int) return xcb_connection_t_access;  -- /usr/include/xcb/xcb.h:525
   pragma Import (C, xcb_connect, "xcb_connect");

   function xcb_connect_to_display_with_auth_info
     (display : Interfaces.C.Strings.chars_ptr;
      auth : access xcb_auth_info_t;
      screen : access int) return xcb_connection_t_access;  -- /usr/include/xcb/xcb.h:539
   pragma Import (C, xcb_connect_to_display_with_auth_info, "xcb_connect_to_display_with_auth_info");

   function xcb_generate_id (c : xcb_connection_t_access) return Libc.Stdint.uint32_t;  -- /usr/include/xcb/xcb.h:552
   pragma Import (C, xcb_generate_id, "xcb_generate_id");
private
   type xcb_connection_t is limited record null; end record;
   type xcb_special_event_t is limited record null; end record;
   type xcb_extension_t is limited record null; end record;
end XCB;
