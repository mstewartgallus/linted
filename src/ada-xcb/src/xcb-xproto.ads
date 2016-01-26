pragma Ada_2005;
pragma Style_Checks (Off);

with C99.Stdint;
with xcb;
with System;
with Interfaces.C.Strings;

package XCB.XProto is
   pragma Preelaborate;

   XCB_KEY_PRESS : constant := 2;
   XCB_KEY_RELEASE : constant := 3;
   XCB_BUTTON_PRESS : constant := 4;
   XCB_BUTTON_RELEASE : constant := 5;
   XCB_MOTION_NOTIFY : constant := 6;
   XCB_ENTER_NOTIFY : constant := 7;
   XCB_LEAVE_NOTIFY : constant := 8;
   XCB_FOCUS_IN : constant := 9;
   XCB_FOCUS_OUT : constant := 10;
   XCB_KEYMAP_NOTIFY : constant := 11;
   XCB_EXPOSE : constant := 12;
   XCB_GRAPHICS_EXPOSURE : constant := 13;
   XCB_NO_EXPOSURE : constant := 14;
   XCB_VISIBILITY_NOTIFY : constant := 15;
   XCB_CREATE_NOTIFY : constant := 16;
   XCB_DESTROY_NOTIFY : constant := 17;
   XCB_UNMAP_NOTIFY : constant := 18;
   XCB_MAP_NOTIFY : constant := 19;
   XCB_MAP_REQUEST : constant := 20;
   XCB_REPARENT_NOTIFY : constant := 21;
   XCB_CONFIGURE_NOTIFY : constant := 22;
   XCB_CONFIGURE_REQUEST : constant := 23;
   XCB_GRAVITY_NOTIFY : constant := 24;
   XCB_RESIZE_REQUEST : constant := 25;
   XCB_CIRCULATE_NOTIFY : constant := 26;
   XCB_CIRCULATE_REQUEST : constant := 27;
   XCB_PROPERTY_NOTIFY : constant := 28;
   XCB_SELECTION_CLEAR : constant := 29;
   XCB_SELECTION_REQUEST : constant := 30;
   XCB_SELECTION_NOTIFY : constant := 31;
   XCB_COLORMAP_NOTIFY : constant := 32;
   XCB_CLIENT_MESSAGE : constant := 33;
   XCB_MAPPING_NOTIFY : constant := 34;
   XCB_GE_GENERIC : constant := 35;
   XCB_REQUEST : constant := 1;
   XCB_VALUE : constant := 2;
   XCB_WINDOW : constant := 3;
   XCB_PIXMAP : constant := 4;
   XCB_ATOM : constant := 5;
   XCB_CURSOR : constant := 6;
   XCB_FONT : constant := 7;
   XCB_MATCH : constant := 8;
   XCB_DRAWABLE : constant := 9;
   XCB_ACCESS : constant := 10;
   XCB_ALLOC : constant := 11;
   XCB_COLORMAP : constant := 12;
   XCB_G_CONTEXT : constant := 13;
   XCB_ID_CHOICE : constant := 14;
   XCB_NAME : constant := 15;
   XCB_LENGTH : constant := 16;
   XCB_IMPLEMENTATION : constant := 17;
   --  XCB_CREATE_WINDOW : constant := 1;
   --  XCB_CHANGE_WINDOW_ATTRIBUTES : constant := 2;
   --  XCB_GET_WINDOW_ATTRIBUTES : constant := 3;
   --  XCB_DESTROY_WINDOW : constant := 4;
   --  XCB_DESTROY_SUBWINDOWS : constant := 5;
   --  XCB_CHANGE_SAVE_SET : constant := 6;
   --  XCB_REPARENT_WINDOW : constant := 7;
   --  XCB_MAP_WINDOW : constant := 8;
   --  XCB_MAP_SUBWINDOWS : constant := 9;
   --  XCB_UNMAP_WINDOW : constant := 10;
   --  XCB_UNMAP_SUBWINDOWS : constant := 11;
   --  XCB_CONFIGURE_WINDOW : constant := 12;
   --  XCB_CIRCULATE_WINDOW : constant := 13;
   --  XCB_GET_GEOMETRY : constant := 14;
   --  XCB_QUERY_TREE : constant := 15;
   --  XCB_INTERN_ATOM : constant := 16;
   --  XCB_GET_ATOM_NAME : constant := 17;
   --  XCB_CHANGE_PROPERTY : constant := 18;
   --  XCB_DELETE_PROPERTY : constant := 19;
   --  XCB_GET_PROPERTY : constant := 20;
   --  XCB_LIST_PROPERTIES : constant := 21;
   --  XCB_SET_SELECTION_OWNER : constant := 22;
   --  XCB_GET_SELECTION_OWNER : constant := 23;
   --  XCB_CONVERT_SELECTION : constant := 24;
   --  XCB_SEND_EVENT : constant := 25;
   --  XCB_GRAB_POINTER : constant := 26;
   --  XCB_UNGRAB_POINTER : constant := 27;
   --  XCB_GRAB_BUTTON : constant := 28;
   --  XCB_UNGRAB_BUTTON : constant := 29;
   --  XCB_CHANGE_ACTIVE_POINTER_GRAB : constant := 30;
   --  XCB_GRAB_KEYBOARD : constant := 31;
   --  XCB_UNGRAB_KEYBOARD : constant := 32;
   --  XCB_GRAB_KEY : constant := 33;
   --  XCB_UNGRAB_KEY : constant := 34;
   --  XCB_ALLOW_EVENTS : constant := 35;
   --  XCB_GRAB_SERVER : constant := 36;
   --  XCB_UNGRAB_SERVER : constant := 37;
   --  XCB_QUERY_POINTER : constant := 38;
   --  XCB_GET_MOTION_EVENTS : constant := 39;
   --  XCB_TRANSLATE_COORDINATES : constant := 40;
   --  XCB_WARP_POINTER : constant := 41;
   --  XCB_SET_INPUT_FOCUS : constant := 42;
   --  XCB_GET_INPUT_FOCUS : constant := 43;
   --  XCB_QUERY_KEYMAP : constant := 44;
   --  XCB_OPEN_FONT : constant := 45;
   --  XCB_CLOSE_FONT : constant := 46;
   --  XCB_QUERY_FONT : constant := 47;
   --  XCB_QUERY_TEXT_EXTENTS : constant := 48;
   --  XCB_LIST_FONTS : constant := 49;
   --  XCB_LIST_FONTS_WITH_INFO : constant := 50;
   --  XCB_SET_FONT_PATH : constant := 51;
   --  XCB_GET_FONT_PATH : constant := 52;
   --  XCB_CREATE_PIXMAP : constant := 53;
   --  XCB_FREE_PIXMAP : constant := 54;
   --  XCB_CREATE_GC : constant := 55;
   --  XCB_CHANGE_GC : constant := 56;
   --  XCB_COPY_GC : constant := 57;
   --  XCB_SET_DASHES : constant := 58;
   --  XCB_SET_CLIP_RECTANGLES : constant := 59;
   --  XCB_FREE_GC : constant := 60;
   --  XCB_CLEAR_AREA : constant := 61;
   --  XCB_COPY_AREA : constant := 62;
   --  XCB_COPY_PLANE : constant := 63;
   --  XCB_POLY_POINT : constant := 64;
   --  XCB_POLY_LINE : constant := 65;
   --  XCB_POLY_SEGMENT : constant := 66;
   --  XCB_POLY_RECTANGLE : constant := 67;
   --  XCB_POLY_ARC : constant := 68;
   --  XCB_FILL_POLY : constant := 69;
   --  XCB_POLY_FILL_RECTANGLE : constant := 70;
   --  XCB_POLY_FILL_ARC : constant := 71;
   --  XCB_PUT_IMAGE : constant := 72;
   --  XCB_GET_IMAGE : constant := 73;
   --  XCB_POLY_TEXT_8 : constant := 74;
   --  XCB_POLY_TEXT_16 : constant := 75;
   --  XCB_IMAGE_TEXT_8 : constant := 76;
   --  XCB_IMAGE_TEXT_16 : constant := 77;
   --  XCB_CREATE_COLORMAP : constant := 78;
   --  XCB_FREE_COLORMAP : constant := 79;
   --  XCB_COPY_COLORMAP_AND_FREE : constant := 80;
   --  XCB_INSTALL_COLORMAP : constant := 81;
   --  XCB_UNINSTALL_COLORMAP : constant := 82;
   --  XCB_LIST_INSTALLED_COLORMAPS : constant := 83;
   --  XCB_ALLOC_COLOR : constant := 84;
   --  XCB_ALLOC_NAMED_COLOR : constant := 85;
   --  XCB_ALLOC_COLOR_CELLS : constant := 86;
   --  XCB_ALLOC_COLOR_PLANES : constant := 87;
   --  XCB_FREE_COLORS : constant := 88;
   --  XCB_STORE_COLORS : constant := 89;
   --  XCB_STORE_NAMED_COLOR : constant := 90;
   --  XCB_QUERY_COLORS : constant := 91;
   --  XCB_LOOKUP_COLOR : constant := 92;
   --  XCB_CREATE_CURSOR : constant := 93;
   --  XCB_CREATE_GLYPH_CURSOR : constant := 94;
   --  XCB_FREE_CURSOR : constant := 95;
   --  XCB_RECOLOR_CURSOR : constant := 96;
   --  XCB_QUERY_BEST_SIZE : constant := 97;
   --  XCB_QUERY_EXTENSION : constant := 98;
   --  XCB_LIST_EXTENSIONS : constant := 99;
   --  XCB_CHANGE_KEYBOARD_MAPPING : constant := 100;
   --  XCB_GET_KEYBOARD_MAPPING : constant := 101;
   --  XCB_CHANGE_KEYBOARD_CONTROL : constant := 102;
   --  XCB_GET_KEYBOARD_CONTROL : constant := 103;
   --  XCB_BELL : constant := 104;
   --  XCB_CHANGE_POINTER_CONTROL : constant := 105;
   --  XCB_GET_POINTER_CONTROL : constant := 106;
   --  XCB_SET_SCREEN_SAVER : constant := 107;
   --  XCB_GET_SCREEN_SAVER : constant := 108;
   --  XCB_CHANGE_HOSTS : constant := 109;
   --  XCB_LIST_HOSTS : constant := 110;
   --  XCB_SET_ACCESS_CONTROL : constant := 111;
   --  XCB_SET_CLOSE_DOWN_MODE : constant := 112;
   --  XCB_KILL_CLIENT : constant := 113;
   --  XCB_ROTATE_PROPERTIES : constant := 114;
   --  XCB_FORCE_SCREEN_SAVER : constant := 115;
   --  XCB_SET_POINTER_MAPPING : constant := 116;
   --  XCB_GET_POINTER_MAPPING : constant := 117;
   --  XCB_SET_MODIFIER_MAPPING : constant := 118;
   --  XCB_GET_MODIFIER_MAPPING : constant := 119;
   --  XCB_NO_OPERATION : constant := 127;

   type xcb_char2b_t is record
      byte1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:25
      byte2 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:26
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_char2b_t);  -- /usr/include/xcb/xproto.h:24

   type xcb_char2b_iterator_t is record
      data : access xcb_char2b_t;  -- /usr/include/xcb/xproto.h:33
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:34
      index : aliased int;  -- /usr/include/xcb/xproto.h:35
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_char2b_iterator_t);  -- /usr/include/xcb/xproto.h:32

   subtype xcb_window_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:38

   type xcb_window_iterator_t is record
      data : access xcb_window_t;  -- /usr/include/xcb/xproto.h:44
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:45
      index : aliased int;  -- /usr/include/xcb/xproto.h:46
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_window_iterator_t);  -- /usr/include/xcb/xproto.h:43

   subtype xcb_pixmap_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:49

   type xcb_pixmap_iterator_t is record
      data : access xcb_pixmap_t;  -- /usr/include/xcb/xproto.h:55
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:56
      index : aliased int;  -- /usr/include/xcb/xproto.h:57
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_pixmap_iterator_t);  -- /usr/include/xcb/xproto.h:54

   subtype xcb_cursor_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:60

   type xcb_cursor_iterator_t is record
      data : access xcb_cursor_t;  -- /usr/include/xcb/xproto.h:66
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:67
      index : aliased int;  -- /usr/include/xcb/xproto.h:68
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_cursor_iterator_t);  -- /usr/include/xcb/xproto.h:65

   subtype xcb_font_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:71

   type xcb_font_iterator_t is record
      data : access xcb_font_t;  -- /usr/include/xcb/xproto.h:77
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:78
      index : aliased int;  -- /usr/include/xcb/xproto.h:79
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_font_iterator_t);  -- /usr/include/xcb/xproto.h:76

   subtype xcb_gcontext_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:82

   type xcb_gcontext_iterator_t is record
      data : access xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:88
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:89
      index : aliased int;  -- /usr/include/xcb/xproto.h:90
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_gcontext_iterator_t);  -- /usr/include/xcb/xproto.h:87

   subtype xcb_colormap_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:93

   type xcb_colormap_iterator_t is record
      data : access xcb_colormap_t;  -- /usr/include/xcb/xproto.h:99
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:100
      index : aliased int;  -- /usr/include/xcb/xproto.h:101
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_colormap_iterator_t);  -- /usr/include/xcb/xproto.h:98

   subtype xcb_atom_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:104

   type xcb_atom_iterator_t is record
      data : access xcb_atom_t;  -- /usr/include/xcb/xproto.h:110
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:111
      index : aliased int;  -- /usr/include/xcb/xproto.h:112
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_atom_iterator_t);  -- /usr/include/xcb/xproto.h:109

   subtype xcb_drawable_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:115

   type xcb_drawable_iterator_t is record
      data : access xcb_drawable_t;  -- /usr/include/xcb/xproto.h:121
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:122
      index : aliased int;  -- /usr/include/xcb/xproto.h:123
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_drawable_iterator_t);  -- /usr/include/xcb/xproto.h:120

   subtype xcb_fontable_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:126

   type xcb_fontable_iterator_t is record
      data : access xcb_fontable_t;  -- /usr/include/xcb/xproto.h:132
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:133
      index : aliased int;  -- /usr/include/xcb/xproto.h:134
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_fontable_iterator_t);  -- /usr/include/xcb/xproto.h:131

   subtype xcb_visualid_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:137

   type xcb_visualid_iterator_t is record
      data : access xcb_visualid_t;  -- /usr/include/xcb/xproto.h:143
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:144
      index : aliased int;  -- /usr/include/xcb/xproto.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_visualid_iterator_t);  -- /usr/include/xcb/xproto.h:142

   subtype xcb_timestamp_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:148

   type xcb_timestamp_iterator_t is record
      data : access xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:154
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:155
      index : aliased int;  -- /usr/include/xcb/xproto.h:156
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_timestamp_iterator_t);  -- /usr/include/xcb/xproto.h:153

   subtype xcb_keysym_t is C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:159

   type xcb_keysym_iterator_t is record
      data : access xcb_keysym_t;  -- /usr/include/xcb/xproto.h:165
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:166
      index : aliased int;  -- /usr/include/xcb/xproto.h:167
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_keysym_iterator_t);  -- /usr/include/xcb/xproto.h:164

   subtype xcb_keycode_t is C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:170

   type xcb_keycode_iterator_t is record
      data : access xcb_keycode_t;  -- /usr/include/xcb/xproto.h:176
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:177
      index : aliased int;  -- /usr/include/xcb/xproto.h:178
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_keycode_iterator_t);  -- /usr/include/xcb/xproto.h:175

   subtype xcb_button_t is C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:181

   type xcb_button_iterator_t is record
      data : access xcb_button_t;  -- /usr/include/xcb/xproto.h:187
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:188
      index : aliased int;  -- /usr/include/xcb/xproto.h:189
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_button_iterator_t);  -- /usr/include/xcb/xproto.h:186

   type xcb_point_t is record
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:196
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:197
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_point_t);  -- /usr/include/xcb/xproto.h:195

   type xcb_point_iterator_t is record
      data : access xcb_point_t;  -- /usr/include/xcb/xproto.h:204
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:205
      index : aliased int;  -- /usr/include/xcb/xproto.h:206
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_point_iterator_t);  -- /usr/include/xcb/xproto.h:203

   type xcb_rectangle_t is record
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:213
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:214
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:215
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:216
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_rectangle_t);  -- /usr/include/xcb/xproto.h:212

   type xcb_rectangle_iterator_t is record
      data : access xcb_rectangle_t;  -- /usr/include/xcb/xproto.h:223
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:224
      index : aliased int;  -- /usr/include/xcb/xproto.h:225
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_rectangle_iterator_t);  -- /usr/include/xcb/xproto.h:222

   type xcb_arc_t is record
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:232
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:233
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:234
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:235
      angle1 : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:236
      angle2 : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:237
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_arc_t);  -- /usr/include/xcb/xproto.h:231

   type xcb_arc_iterator_t is record
      data : access xcb_arc_t;  -- /usr/include/xcb/xproto.h:244
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:245
      index : aliased int;  -- /usr/include/xcb/xproto.h:246
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_arc_iterator_t);  -- /usr/include/xcb/xproto.h:243

   type xcb_format_t_pad0_array is array (0 .. 4) of aliased C99.Stdint.uint8_t;
   type xcb_format_t is record
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:253
      bits_per_pixel : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:254
      scanline_pad : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:255
      pad0 : aliased xcb_format_t_pad0_array;  -- /usr/include/xcb/xproto.h:256
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_format_t);  -- /usr/include/xcb/xproto.h:252

   type xcb_format_iterator_t is record
      data : access xcb_format_t;  -- /usr/include/xcb/xproto.h:263
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:264
      index : aliased int;  -- /usr/include/xcb/xproto.h:265
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_format_iterator_t);  -- /usr/include/xcb/xproto.h:262

   type xcb_visual_class_t is
     (XCB_VISUAL_CLASS_STATIC_GRAY,
      XCB_VISUAL_CLASS_GRAY_SCALE,
      XCB_VISUAL_CLASS_STATIC_COLOR,
      XCB_VISUAL_CLASS_PSEUDO_COLOR,
      XCB_VISUAL_CLASS_TRUE_COLOR,
      XCB_VISUAL_CLASS_DIRECT_COLOR);
   pragma Convention (C, xcb_visual_class_t);  -- /usr/include/xcb/xproto.h:268

   type xcb_visualtype_t_pad0_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_visualtype_t is record
      visual_id : aliased xcb_visualid_t;  -- /usr/include/xcb/xproto.h:281
      u_class : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:282
      bits_per_rgb_value : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:283
      colormap_entries : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:284
      red_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:285
      green_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:286
      blue_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:287
      pad0 : aliased xcb_visualtype_t_pad0_array;  -- /usr/include/xcb/xproto.h:288
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_visualtype_t);  -- /usr/include/xcb/xproto.h:280

   type xcb_visualtype_iterator_t is record
      data : access xcb_visualtype_t;  -- /usr/include/xcb/xproto.h:295
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:296
      index : aliased int;  -- /usr/include/xcb/xproto.h:297
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_visualtype_iterator_t);  -- /usr/include/xcb/xproto.h:294

   type xcb_depth_t_pad1_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_depth_t is record
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:304
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:305
      visuals_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:306
      pad1 : aliased xcb_depth_t_pad1_array;  -- /usr/include/xcb/xproto.h:307
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_depth_t);  -- /usr/include/xcb/xproto.h:303

   type xcb_depth_iterator_t is record
      data : access xcb_depth_t;  -- /usr/include/xcb/xproto.h:314
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:315
      index : aliased int;  -- /usr/include/xcb/xproto.h:316
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_depth_iterator_t);  -- /usr/include/xcb/xproto.h:313

   subtype xcb_event_mask_t is unsigned;
   XCB_EVENT_MASK_NO_EVENT : constant xcb_event_mask_t := 0;
   XCB_EVENT_MASK_KEY_PRESS : constant xcb_event_mask_t := 1;
   XCB_EVENT_MASK_KEY_RELEASE : constant xcb_event_mask_t := 2;
   XCB_EVENT_MASK_BUTTON_PRESS : constant xcb_event_mask_t := 4;
   XCB_EVENT_MASK_BUTTON_RELEASE : constant xcb_event_mask_t := 8;
   XCB_EVENT_MASK_ENTER_WINDOW : constant xcb_event_mask_t := 16;
   XCB_EVENT_MASK_LEAVE_WINDOW : constant xcb_event_mask_t := 32;
   XCB_EVENT_MASK_POINTER_MOTION : constant xcb_event_mask_t := 64;
   XCB_EVENT_MASK_POINTER_MOTION_HINT : constant xcb_event_mask_t := 128;
   XCB_EVENT_MASK_BUTTON_1_MOTION : constant xcb_event_mask_t := 256;
   XCB_EVENT_MASK_BUTTON_2_MOTION : constant xcb_event_mask_t := 512;
   XCB_EVENT_MASK_BUTTON_3_MOTION : constant xcb_event_mask_t := 1024;
   XCB_EVENT_MASK_BUTTON_4_MOTION : constant xcb_event_mask_t := 2048;
   XCB_EVENT_MASK_BUTTON_5_MOTION : constant xcb_event_mask_t := 4096;
   XCB_EVENT_MASK_BUTTON_MOTION : constant xcb_event_mask_t := 8192;
   XCB_EVENT_MASK_KEYMAP_STATE : constant xcb_event_mask_t := 16384;
   XCB_EVENT_MASK_EXPOSURE : constant xcb_event_mask_t := 32768;
   XCB_EVENT_MASK_VISIBILITY_CHANGE : constant xcb_event_mask_t := 65536;
   XCB_EVENT_MASK_STRUCTURE_NOTIFY : constant xcb_event_mask_t := 131072;
   XCB_EVENT_MASK_RESIZE_REDIRECT : constant xcb_event_mask_t := 262144;
   XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY : constant xcb_event_mask_t := 524288;
   XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT : constant xcb_event_mask_t := 1048576;
   XCB_EVENT_MASK_FOCUS_CHANGE : constant xcb_event_mask_t := 2097152;
   XCB_EVENT_MASK_PROPERTY_CHANGE : constant xcb_event_mask_t := 4194304;
   XCB_EVENT_MASK_COLOR_MAP_CHANGE : constant xcb_event_mask_t := 8388608;
   XCB_EVENT_MASK_OWNER_GRAB_BUTTON : constant xcb_event_mask_t := 16777216;  -- /usr/include/xcb/xproto.h:319

   type xcb_backing_store_t is
     (XCB_BACKING_STORE_NOT_USEFUL,
      XCB_BACKING_STORE_WHEN_MAPPED,
      XCB_BACKING_STORE_ALWAYS);
   pragma Convention (C, xcb_backing_store_t);  -- /usr/include/xcb/xproto.h:348

   type xcb_screen_t is record
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:358
      default_colormap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:359
      white_pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:360
      black_pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:361
      current_input_masks : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:362
      width_in_pixels : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:363
      height_in_pixels : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:364
      width_in_millimeters : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:365
      height_in_millimeters : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:366
      min_installed_maps : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:367
      max_installed_maps : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:368
      root_visual : aliased xcb_visualid_t;  -- /usr/include/xcb/xproto.h:369
      backing_stores : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:370
      save_unders : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:371
      root_depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:372
      allowed_depths_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:373
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_screen_t);  -- /usr/include/xcb/xproto.h:357

   type xcb_screen_iterator_t is record
      data : access xcb_screen_t;  -- /usr/include/xcb/xproto.h:380
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:381
      index : aliased int;  -- /usr/include/xcb/xproto.h:382
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_screen_iterator_t);  -- /usr/include/xcb/xproto.h:379

   type xcb_setup_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_setup_request_t is record
      byte_order : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:389
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:390
      protocol_major_version : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:391
      protocol_minor_version : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:392
      authorization_protocol_name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:393
      authorization_protocol_data_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:394
      pad1 : aliased xcb_setup_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:395
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_request_t);  -- /usr/include/xcb/xproto.h:388

   type xcb_setup_request_iterator_t is record
      data : access xcb_setup_request_t;  -- /usr/include/xcb/xproto.h:402
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:403
      index : aliased int;  -- /usr/include/xcb/xproto.h:404
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_request_iterator_t);  -- /usr/include/xcb/xproto.h:401

   type xcb_setup_failed_t is record
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:411
      reason_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:412
      protocol_major_version : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:413
      protocol_minor_version : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:414
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:415
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_failed_t);  -- /usr/include/xcb/xproto.h:410

   type xcb_setup_failed_iterator_t is record
      data : access xcb_setup_failed_t;  -- /usr/include/xcb/xproto.h:422
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:423
      index : aliased int;  -- /usr/include/xcb/xproto.h:424
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_failed_iterator_t);  -- /usr/include/xcb/xproto.h:421

   type xcb_setup_authenticate_t_pad0_array is array (0 .. 4) of aliased C99.Stdint.uint8_t;
   type xcb_setup_authenticate_t is record
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:431
      pad0 : aliased xcb_setup_authenticate_t_pad0_array;  -- /usr/include/xcb/xproto.h:432
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:433
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_authenticate_t);  -- /usr/include/xcb/xproto.h:430

   type xcb_setup_authenticate_iterator_t is record
      data : access xcb_setup_authenticate_t;  -- /usr/include/xcb/xproto.h:440
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:441
      index : aliased int;  -- /usr/include/xcb/xproto.h:442
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_authenticate_iterator_t);  -- /usr/include/xcb/xproto.h:439

   type xcb_image_order_t is
     (XCB_IMAGE_ORDER_LSB_FIRST,
      XCB_IMAGE_ORDER_MSB_FIRST);
   pragma Convention (C, xcb_image_order_t);  -- /usr/include/xcb/xproto.h:445

   type xcb_setup_t_pad1_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_setup_t is record
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:454
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:455
      protocol_major_version : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:456
      protocol_minor_version : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:457
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:458
      release_number : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:459
      resource_id_base : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:460
      resource_id_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:461
      motion_buffer_size : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:462
      vendor_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:463
      maximum_request_length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:464
      roots_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:465
      pixmap_formats_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:466
      image_byte_order : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:467
      bitmap_format_bit_order : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:468
      bitmap_format_scanline_unit : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:469
      bitmap_format_scanline_pad : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:470
      min_keycode : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:471
      max_keycode : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:472
      pad1 : aliased xcb_setup_t_pad1_array;  -- /usr/include/xcb/xproto.h:473
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_t);  -- /usr/include/xcb/xproto.h:453

   type xcb_setup_iterator_t is record
      data : access xcb_setup_t;  -- /usr/include/xcb/xproto.h:480
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:481
      index : aliased int;  -- /usr/include/xcb/xproto.h:482
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_setup_iterator_t);  -- /usr/include/xcb/xproto.h:479

   subtype xcb_mod_mask_t is unsigned;
   XCB_MOD_MASK_SHIFT : constant xcb_mod_mask_t := 1;
   XCB_MOD_MASK_LOCK : constant xcb_mod_mask_t := 2;
   XCB_MOD_MASK_CONTROL : constant xcb_mod_mask_t := 4;
   XCB_MOD_MASK_1 : constant xcb_mod_mask_t := 8;
   XCB_MOD_MASK_2 : constant xcb_mod_mask_t := 16;
   XCB_MOD_MASK_3 : constant xcb_mod_mask_t := 32;
   XCB_MOD_MASK_4 : constant xcb_mod_mask_t := 64;
   XCB_MOD_MASK_5 : constant xcb_mod_mask_t := 128;
   XCB_MOD_MASK_ANY : constant xcb_mod_mask_t := 32768;  -- /usr/include/xcb/xproto.h:485

   subtype xcb_key_but_mask_t is unsigned;
   XCB_KEY_BUT_MASK_SHIFT : constant xcb_key_but_mask_t := 1;
   XCB_KEY_BUT_MASK_LOCK : constant xcb_key_but_mask_t := 2;
   XCB_KEY_BUT_MASK_CONTROL : constant xcb_key_but_mask_t := 4;
   XCB_KEY_BUT_MASK_MOD_1 : constant xcb_key_but_mask_t := 8;
   XCB_KEY_BUT_MASK_MOD_2 : constant xcb_key_but_mask_t := 16;
   XCB_KEY_BUT_MASK_MOD_3 : constant xcb_key_but_mask_t := 32;
   XCB_KEY_BUT_MASK_MOD_4 : constant xcb_key_but_mask_t := 64;
   XCB_KEY_BUT_MASK_MOD_5 : constant xcb_key_but_mask_t := 128;
   XCB_KEY_BUT_MASK_BUTTON_1 : constant xcb_key_but_mask_t := 256;
   XCB_KEY_BUT_MASK_BUTTON_2 : constant xcb_key_but_mask_t := 512;
   XCB_KEY_BUT_MASK_BUTTON_3 : constant xcb_key_but_mask_t := 1024;
   XCB_KEY_BUT_MASK_BUTTON_4 : constant xcb_key_but_mask_t := 2048;
   XCB_KEY_BUT_MASK_BUTTON_5 : constant xcb_key_but_mask_t := 4096;  -- /usr/include/xcb/xproto.h:497

   type xcb_window_enum_t is
     (XCB_WINDOW_NONE);
   pragma Convention (C, xcb_window_enum_t);  -- /usr/include/xcb/xproto.h:513

   type xcb_key_press_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:524
      detail : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:525
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:526
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:527
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:528
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:529
      child : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:530
      root_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:531
      root_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:532
      event_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:533
      event_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:534
      state : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:535
      same_screen : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:536
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:537
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_key_press_event_t);  -- /usr/include/xcb/xproto.h:523

   subtype xcb_key_release_event_t is xcb_key_press_event_t;

   subtype xcb_button_mask_t is unsigned;
   XCB_BUTTON_MASK_1 : constant xcb_button_mask_t := 256;
   XCB_BUTTON_MASK_2 : constant xcb_button_mask_t := 512;
   XCB_BUTTON_MASK_3 : constant xcb_button_mask_t := 1024;
   XCB_BUTTON_MASK_4 : constant xcb_button_mask_t := 2048;
   XCB_BUTTON_MASK_5 : constant xcb_button_mask_t := 4096;
   XCB_BUTTON_MASK_ANY : constant xcb_button_mask_t := 32768;  -- /usr/include/xcb/xproto.h:545

   type xcb_button_press_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:561
      detail : aliased xcb_button_t;  -- /usr/include/xcb/xproto.h:562
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:563
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:564
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:565
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:566
      child : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:567
      root_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:568
      root_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:569
      event_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:570
      event_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:571
      state : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:572
      same_screen : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:573
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:574
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_button_press_event_t);  -- /usr/include/xcb/xproto.h:560

   subtype xcb_button_release_event_t is xcb_button_press_event_t;

   type xcb_motion_t is
     (XCB_MOTION_NORMAL,
      XCB_MOTION_HINT);
   pragma Convention (C, xcb_motion_t);  -- /usr/include/xcb/xproto.h:582

   type xcb_motion_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:594
      detail : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:595
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:596
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:597
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:598
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:599
      child : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:600
      root_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:601
      root_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:602
      event_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:603
      event_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:604
      state : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:605
      same_screen : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:606
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:607
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_motion_notify_event_t);  -- /usr/include/xcb/xproto.h:593

   type xcb_notify_detail_t is
     (XCB_NOTIFY_DETAIL_ANCESTOR,
      XCB_NOTIFY_DETAIL_VIRTUAL,
      XCB_NOTIFY_DETAIL_INFERIOR,
      XCB_NOTIFY_DETAIL_NONLINEAR,
      XCB_NOTIFY_DETAIL_NONLINEAR_VIRTUAL,
      XCB_NOTIFY_DETAIL_POINTER,
      XCB_NOTIFY_DETAIL_POINTER_ROOT,
      XCB_NOTIFY_DETAIL_NONE);
   pragma Convention (C, xcb_notify_detail_t);  -- /usr/include/xcb/xproto.h:610

   type xcb_notify_mode_t is
     (XCB_NOTIFY_MODE_NORMAL,
      XCB_NOTIFY_MODE_GRAB,
      XCB_NOTIFY_MODE_UNGRAB,
      XCB_NOTIFY_MODE_WHILE_GRABBED);
   pragma Convention (C, xcb_notify_mode_t);  -- /usr/include/xcb/xproto.h:621

   type xcb_enter_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:635
      detail : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:636
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:637
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:638
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:639
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:640
      child : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:641
      root_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:642
      root_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:643
      event_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:644
      event_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:645
      state : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:646
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:647
      same_screen_focus : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:648
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_enter_notify_event_t);  -- /usr/include/xcb/xproto.h:634

   subtype xcb_leave_notify_event_t is xcb_enter_notify_event_t;

   type xcb_focus_in_event_t_pad0_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_focus_in_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:663
      detail : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:664
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:665
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:666
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:667
      pad0 : aliased xcb_focus_in_event_t_pad0_array;  -- /usr/include/xcb/xproto.h:668
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_focus_in_event_t);  -- /usr/include/xcb/xproto.h:662

   subtype xcb_focus_out_event_t is xcb_focus_in_event_t;

   type xcb_keymap_notify_event_t_keys_array is array (0 .. 30) of aliased C99.Stdint.uint8_t;
   type xcb_keymap_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:683
      keys : aliased xcb_keymap_notify_event_t_keys_array;  -- /usr/include/xcb/xproto.h:684
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_keymap_notify_event_t);  -- /usr/include/xcb/xproto.h:682

   type xcb_expose_event_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_expose_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:694
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:695
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:696
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:697
      x : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:698
      y : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:699
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:700
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:701
      count : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:702
      pad1 : aliased xcb_expose_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:703
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_expose_event_t);  -- /usr/include/xcb/xproto.h:693

   type xcb_graphics_exposure_event_t_pad1_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_graphics_exposure_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:713
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:714
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:715
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:716
      x : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:717
      y : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:718
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:719
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:720
      minor_opcode : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:721
      count : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:722
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:723
      pad1 : aliased xcb_graphics_exposure_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:724
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_graphics_exposure_event_t);  -- /usr/include/xcb/xproto.h:712

   type xcb_no_exposure_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:734
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:735
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:736
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:737
      minor_opcode : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:738
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:739
      pad1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:740
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_no_exposure_event_t);  -- /usr/include/xcb/xproto.h:733

   type xcb_visibility_t is
     (XCB_VISIBILITY_UNOBSCURED,
      XCB_VISIBILITY_PARTIALLY_OBSCURED,
      XCB_VISIBILITY_FULLY_OBSCURED);
   pragma Convention (C, xcb_visibility_t);  -- /usr/include/xcb/xproto.h:743

   type xcb_visibility_notify_event_t_pad1_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_visibility_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:756
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:757
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:758
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:759
      state : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:760
      pad1 : aliased xcb_visibility_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:761
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_visibility_notify_event_t);  -- /usr/include/xcb/xproto.h:755

   type xcb_create_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:771
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:772
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:773
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:774
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:775
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:776
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:777
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:778
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:779
      border_width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:780
      override_redirect : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:781
      pad1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:782
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_notify_event_t);  -- /usr/include/xcb/xproto.h:770

   type xcb_destroy_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:792
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:793
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:794
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:795
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:796
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_destroy_notify_event_t);  -- /usr/include/xcb/xproto.h:791

   type xcb_unmap_notify_event_t_pad1_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_unmap_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:806
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:807
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:808
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:809
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:810
      from_configure : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:811
      pad1 : aliased xcb_unmap_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:812
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_unmap_notify_event_t);  -- /usr/include/xcb/xproto.h:805

   type xcb_map_notify_event_t_pad1_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_map_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:822
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:823
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:824
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:825
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:826
      override_redirect : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:827
      pad1 : aliased xcb_map_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:828
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_map_notify_event_t);  -- /usr/include/xcb/xproto.h:821

   type xcb_map_request_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:838
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:839
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:840
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:841
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:842
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_map_request_event_t);  -- /usr/include/xcb/xproto.h:837

   type xcb_reparent_notify_event_t_pad1_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_reparent_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:852
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:853
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:854
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:855
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:856
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:857
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:858
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:859
      override_redirect : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:860
      pad1 : aliased xcb_reparent_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:861
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_reparent_notify_event_t);  -- /usr/include/xcb/xproto.h:851

   type xcb_configure_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:871
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:872
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:873
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:874
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:875
      above_sibling : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:876
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:877
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:878
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:879
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:880
      border_width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:881
      override_redirect : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:882
      pad1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:883
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_configure_notify_event_t);  -- /usr/include/xcb/xproto.h:870

   type xcb_configure_request_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:893
      stack_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:894
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:895
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:896
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:897
      sibling : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:898
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:899
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:900
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:901
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:902
      border_width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:903
      value_mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:904
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_configure_request_event_t);  -- /usr/include/xcb/xproto.h:892

   type xcb_gravity_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:914
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:915
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:916
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:917
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:918
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:919
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:920
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_gravity_notify_event_t);  -- /usr/include/xcb/xproto.h:913

   type xcb_resize_request_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:930
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:931
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:932
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:933
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:934
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:935
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_resize_request_event_t);  -- /usr/include/xcb/xproto.h:929

   type xcb_place_t is
     (XCB_PLACE_ON_TOP,
      XCB_PLACE_ON_BOTTOM);
   pragma Convention (C, xcb_place_t);  -- /usr/include/xcb/xproto.h:938

   type xcb_circulate_notify_event_t_pad1_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_circulate_notify_event_t_pad2_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_circulate_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:954
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:955
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:956
      event : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:957
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:958
      pad1 : aliased xcb_circulate_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:959
      place : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:960
      pad2 : aliased xcb_circulate_notify_event_t_pad2_array;  -- /usr/include/xcb/xproto.h:961
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_circulate_notify_event_t);  -- /usr/include/xcb/xproto.h:953

   subtype xcb_circulate_request_event_t is xcb_circulate_notify_event_t;

   type xcb_property_t is
     (XCB_PROPERTY_NEW_VALUE,
      XCB_PROPERTY_DELETE);
   pragma Convention (C, xcb_property_t);  -- /usr/include/xcb/xproto.h:969

   type xcb_property_notify_event_t_pad1_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_property_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:981
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:982
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:983
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:984
      atom : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:985
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:986
      state : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:987
      pad1 : aliased xcb_property_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:988
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_property_notify_event_t);  -- /usr/include/xcb/xproto.h:980

   type xcb_selection_clear_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:998
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:999
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1000
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:1001
      owner : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1002
      selection : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1003
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_selection_clear_event_t);  -- /usr/include/xcb/xproto.h:997

   type xcb_time_t is
     (XCB_TIME_CURRENT_TIME);
   pragma Convention (C, xcb_time_t);  -- /usr/include/xcb/xproto.h:1006

   subtype xcb_atom_enum_t is unsigned;
   XCB_ATOM_NONE : constant xcb_atom_enum_t := 0;
   XCB_ATOM_ANY : constant xcb_atom_enum_t := 0;
   XCB_ATOM_PRIMARY : constant xcb_atom_enum_t := 1;
   XCB_ATOM_SECONDARY : constant xcb_atom_enum_t := 2;
   XCB_ATOM_ARC : constant xcb_atom_enum_t := 3;
   XCB_ATOM_ATOM : constant xcb_atom_enum_t := 4;
   XCB_ATOM_BITMAP : constant xcb_atom_enum_t := 5;
   XCB_ATOM_CARDINAL : constant xcb_atom_enum_t := 6;
   XCB_ATOM_COLORMAP : constant xcb_atom_enum_t := 7;
   XCB_ATOM_CURSOR : constant xcb_atom_enum_t := 8;
   XCB_ATOM_CUT_BUFFER0 : constant xcb_atom_enum_t := 9;
   XCB_ATOM_CUT_BUFFER1 : constant xcb_atom_enum_t := 10;
   XCB_ATOM_CUT_BUFFER2 : constant xcb_atom_enum_t := 11;
   XCB_ATOM_CUT_BUFFER3 : constant xcb_atom_enum_t := 12;
   XCB_ATOM_CUT_BUFFER4 : constant xcb_atom_enum_t := 13;
   XCB_ATOM_CUT_BUFFER5 : constant xcb_atom_enum_t := 14;
   XCB_ATOM_CUT_BUFFER6 : constant xcb_atom_enum_t := 15;
   XCB_ATOM_CUT_BUFFER7 : constant xcb_atom_enum_t := 16;
   XCB_ATOM_DRAWABLE : constant xcb_atom_enum_t := 17;
   XCB_ATOM_FONT : constant xcb_atom_enum_t := 18;
   XCB_ATOM_INTEGER : constant xcb_atom_enum_t := 19;
   XCB_ATOM_PIXMAP : constant xcb_atom_enum_t := 20;
   XCB_ATOM_POINT : constant xcb_atom_enum_t := 21;
   XCB_ATOM_RECTANGLE : constant xcb_atom_enum_t := 22;
   XCB_ATOM_RESOURCE_MANAGER : constant xcb_atom_enum_t := 23;
   XCB_ATOM_RGB_COLOR_MAP : constant xcb_atom_enum_t := 24;
   XCB_ATOM_RGB_BEST_MAP : constant xcb_atom_enum_t := 25;
   XCB_ATOM_RGB_BLUE_MAP : constant xcb_atom_enum_t := 26;
   XCB_ATOM_RGB_DEFAULT_MAP : constant xcb_atom_enum_t := 27;
   XCB_ATOM_RGB_GRAY_MAP : constant xcb_atom_enum_t := 28;
   XCB_ATOM_RGB_GREEN_MAP : constant xcb_atom_enum_t := 29;
   XCB_ATOM_RGB_RED_MAP : constant xcb_atom_enum_t := 30;
   XCB_ATOM_STRING : constant xcb_atom_enum_t := 31;
   XCB_ATOM_VISUALID : constant xcb_atom_enum_t := 32;
   XCB_ATOM_WINDOW : constant xcb_atom_enum_t := 33;
   XCB_ATOM_WM_COMMAND : constant xcb_atom_enum_t := 34;
   XCB_ATOM_WM_HINTS : constant xcb_atom_enum_t := 35;
   XCB_ATOM_WM_CLIENT_MACHINE : constant xcb_atom_enum_t := 36;
   XCB_ATOM_WM_ICON_NAME : constant xcb_atom_enum_t := 37;
   XCB_ATOM_WM_ICON_SIZE : constant xcb_atom_enum_t := 38;
   XCB_ATOM_WM_NAME : constant xcb_atom_enum_t := 39;
   XCB_ATOM_WM_NORMAL_HINTS : constant xcb_atom_enum_t := 40;
   XCB_ATOM_WM_SIZE_HINTS : constant xcb_atom_enum_t := 41;
   XCB_ATOM_WM_ZOOM_HINTS : constant xcb_atom_enum_t := 42;
   XCB_ATOM_MIN_SPACE : constant xcb_atom_enum_t := 43;
   XCB_ATOM_NORM_SPACE : constant xcb_atom_enum_t := 44;
   XCB_ATOM_MAX_SPACE : constant xcb_atom_enum_t := 45;
   XCB_ATOM_END_SPACE : constant xcb_atom_enum_t := 46;
   XCB_ATOM_SUPERSCRIPT_X : constant xcb_atom_enum_t := 47;
   XCB_ATOM_SUPERSCRIPT_Y : constant xcb_atom_enum_t := 48;
   XCB_ATOM_SUBSCRIPT_X : constant xcb_atom_enum_t := 49;
   XCB_ATOM_SUBSCRIPT_Y : constant xcb_atom_enum_t := 50;
   XCB_ATOM_UNDERLINE_POSITION : constant xcb_atom_enum_t := 51;
   XCB_ATOM_UNDERLINE_THICKNESS : constant xcb_atom_enum_t := 52;
   XCB_ATOM_STRIKEOUT_ASCENT : constant xcb_atom_enum_t := 53;
   XCB_ATOM_STRIKEOUT_DESCENT : constant xcb_atom_enum_t := 54;
   XCB_ATOM_ITALIC_ANGLE : constant xcb_atom_enum_t := 55;
   XCB_ATOM_X_HEIGHT : constant xcb_atom_enum_t := 56;
   XCB_ATOM_QUAD_WIDTH : constant xcb_atom_enum_t := 57;
   XCB_ATOM_WEIGHT : constant xcb_atom_enum_t := 58;
   XCB_ATOM_POINT_SIZE : constant xcb_atom_enum_t := 59;
   XCB_ATOM_RESOLUTION : constant xcb_atom_enum_t := 60;
   XCB_ATOM_COPYRIGHT : constant xcb_atom_enum_t := 61;
   XCB_ATOM_NOTICE : constant xcb_atom_enum_t := 62;
   XCB_ATOM_FONT_NAME : constant xcb_atom_enum_t := 63;
   XCB_ATOM_FAMILY_NAME : constant xcb_atom_enum_t := 64;
   XCB_ATOM_FULL_NAME : constant xcb_atom_enum_t := 65;
   XCB_ATOM_CAP_HEIGHT : constant xcb_atom_enum_t := 66;
   XCB_ATOM_WM_CLASS : constant xcb_atom_enum_t := 67;
   XCB_ATOM_WM_TRANSIENT_FOR : constant xcb_atom_enum_t := 68;  -- /usr/include/xcb/xproto.h:1010

   type xcb_selection_request_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1090
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1091
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1092
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:1093
      owner : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1094
      requestor : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1095
      selection : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1096
      target : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1097
      property : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1098
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_selection_request_event_t);  -- /usr/include/xcb/xproto.h:1089

   type xcb_selection_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1108
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1109
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1110
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:1111
      requestor : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1112
      selection : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1113
      target : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1114
      property : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1115
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_selection_notify_event_t);  -- /usr/include/xcb/xproto.h:1107

   type xcb_colormap_state_t is
     (XCB_COLORMAP_STATE_UNINSTALLED,
      XCB_COLORMAP_STATE_INSTALLED);
   pragma Convention (C, xcb_colormap_state_t);  -- /usr/include/xcb/xproto.h:1118

   type xcb_colormap_enum_t is
     (XCB_COLORMAP_NONE);
   pragma Convention (C, xcb_colormap_enum_t);  -- /usr/include/xcb/xproto.h:1127

   type xcb_colormap_notify_event_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_colormap_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1138
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1139
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1140
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1141
      colormap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:1142
      u_new : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1143
      state : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1144
      pad1 : aliased xcb_colormap_notify_event_t_pad1_array;  -- /usr/include/xcb/xproto.h:1145
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_colormap_notify_event_t);  -- /usr/include/xcb/xproto.h:1137

   type xcb_client_message_data_t_data8_array is array (0 .. 19) of aliased C99.Stdint.uint8_t;
   type xcb_client_message_data_t_data16_array is array (0 .. 9) of aliased C99.Stdint.uint16_t;
   type xcb_client_message_data_t_data32_array is array (0 .. 4) of aliased C99.Stdint.uint32_t;
   type xcb_client_message_data_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            data8 : aliased xcb_client_message_data_t_data8_array;  -- /usr/include/xcb/xproto.h:1152
         when 1 =>
            data16 : aliased xcb_client_message_data_t_data16_array;  -- /usr/include/xcb/xproto.h:1153
         when others =>
            data32 : aliased xcb_client_message_data_t_data32_array;  -- /usr/include/xcb/xproto.h:1154
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_client_message_data_t);
   pragma Unchecked_Union (xcb_client_message_data_t);  -- /usr/include/xcb/xproto.h:1151

   type xcb_client_message_data_iterator_t is record
      data : access xcb_client_message_data_t;  -- /usr/include/xcb/xproto.h:1161
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:1162
      index : aliased int;  -- /usr/include/xcb/xproto.h:1163
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_client_message_data_iterator_t);  -- /usr/include/xcb/xproto.h:1160

   type xcb_client_message_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1173
      format : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1174
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1175
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1176
      c_type : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1177
      data : xcb_client_message_data_t;  -- /usr/include/xcb/xproto.h:1178
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_client_message_event_t);  -- /usr/include/xcb/xproto.h:1172

   type xcb_mapping_t is
     (XCB_MAPPING_MODIFIER,
      XCB_MAPPING_KEYBOARD,
      XCB_MAPPING_POINTER);
   pragma Convention (C, xcb_mapping_t);  -- /usr/include/xcb/xproto.h:1181

   type xcb_mapping_notify_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1194
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1195
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1196
      request : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1197
      first_keycode : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:1198
      count : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1199
      pad1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1200
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_mapping_notify_event_t);  -- /usr/include/xcb/xproto.h:1193

   type xcb_ge_generic_event_t_pad0_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_ge_generic_event_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1210
      extension : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1211
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1212
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1213
      event_type : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1214
      pad0 : aliased xcb_ge_generic_event_t_pad0_array;  -- /usr/include/xcb/xproto.h:1215
      full_sequence : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1216
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ge_generic_event_t);  -- /usr/include/xcb/xproto.h:1209

   type xcb_request_error_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1226
      error_code : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1227
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1228
      bad_value : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1229
      minor_opcode : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1230
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1231
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1232
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_request_error_t);  -- /usr/include/xcb/xproto.h:1225

   type xcb_value_error_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1242
      error_code : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1243
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1244
      bad_value : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1245
      minor_opcode : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1246
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1247
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1248
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_value_error_t);  -- /usr/include/xcb/xproto.h:1241

   subtype xcb_window_error_t is xcb_value_error_t;

   subtype xcb_pixmap_error_t is xcb_value_error_t;

   subtype xcb_atom_error_t is xcb_value_error_t;

   subtype xcb_cursor_error_t is xcb_value_error_t;

   subtype xcb_font_error_t is xcb_value_error_t;

   subtype xcb_match_error_t is xcb_request_error_t;

   subtype xcb_drawable_error_t is xcb_value_error_t;

   subtype xcb_access_error_t is xcb_request_error_t;

   subtype xcb_alloc_error_t is xcb_request_error_t;

   subtype xcb_colormap_error_t is xcb_value_error_t;

   subtype xcb_g_context_error_t is xcb_value_error_t;

   subtype xcb_id_choice_error_t is xcb_value_error_t;

   subtype xcb_name_error_t is xcb_request_error_t;

   subtype xcb_length_error_t is xcb_request_error_t;

   subtype xcb_implementation_error_t is xcb_request_error_t;

   type xcb_window_class_t is
     (XCB_WINDOW_CLASS_COPY_FROM_PARENT,
      XCB_WINDOW_CLASS_INPUT_OUTPUT,
      XCB_WINDOW_CLASS_INPUT_ONLY);
   pragma Convention (C, xcb_window_class_t);  -- /usr/include/xcb/xproto.h:1326

   subtype xcb_cw_t is unsigned;
   XCB_CW_BACK_PIXMAP : constant xcb_cw_t := 1;
   XCB_CW_BACK_PIXEL : constant xcb_cw_t := 2;
   XCB_CW_BORDER_PIXMAP : constant xcb_cw_t := 4;
   XCB_CW_BORDER_PIXEL : constant xcb_cw_t := 8;
   XCB_CW_BIT_GRAVITY : constant xcb_cw_t := 16;
   XCB_CW_WIN_GRAVITY : constant xcb_cw_t := 32;
   XCB_CW_BACKING_STORE : constant xcb_cw_t := 64;
   XCB_CW_BACKING_PLANES : constant xcb_cw_t := 128;
   XCB_CW_BACKING_PIXEL : constant xcb_cw_t := 256;
   XCB_CW_OVERRIDE_REDIRECT : constant xcb_cw_t := 512;
   XCB_CW_SAVE_UNDER : constant xcb_cw_t := 1024;
   XCB_CW_EVENT_MASK : constant xcb_cw_t := 2048;
   XCB_CW_DONT_PROPAGATE : constant xcb_cw_t := 4096;
   XCB_CW_COLORMAP : constant xcb_cw_t := 8192;
   XCB_CW_CURSOR : constant xcb_cw_t := 16384;  -- /usr/include/xcb/xproto.h:1332

   type xcb_back_pixmap_t is
     (XCB_BACK_PIXMAP_NONE,
      XCB_BACK_PIXMAP_PARENT_RELATIVE);
   pragma Convention (C, xcb_back_pixmap_t);  -- /usr/include/xcb/xproto.h:1433

   subtype xcb_gravity_t is unsigned;
   XCB_GRAVITY_BIT_FORGET : constant xcb_gravity_t := 0;
   XCB_GRAVITY_WIN_UNMAP : constant xcb_gravity_t := 0;
   XCB_GRAVITY_NORTH_WEST : constant xcb_gravity_t := 1;
   XCB_GRAVITY_NORTH : constant xcb_gravity_t := 2;
   XCB_GRAVITY_NORTH_EAST : constant xcb_gravity_t := 3;
   XCB_GRAVITY_WEST : constant xcb_gravity_t := 4;
   XCB_GRAVITY_CENTER : constant xcb_gravity_t := 5;
   XCB_GRAVITY_EAST : constant xcb_gravity_t := 6;
   XCB_GRAVITY_SOUTH_WEST : constant xcb_gravity_t := 7;
   XCB_GRAVITY_SOUTH : constant xcb_gravity_t := 8;
   XCB_GRAVITY_SOUTH_EAST : constant xcb_gravity_t := 9;
   XCB_GRAVITY_STATIC : constant xcb_gravity_t := 10;  -- /usr/include/xcb/xproto.h:1438

   type xcb_create_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1460
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1461
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1462
      wid : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1463
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1464
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:1465
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:1466
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1467
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1468
      border_width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1469
      u_class : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1470
      visual : aliased xcb_visualid_t;  -- /usr/include/xcb/xproto.h:1471
      value_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1472
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_window_request_t);  -- /usr/include/xcb/xproto.h:1459

   type xcb_change_window_attributes_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1482
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1483
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1484
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1485
      value_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1486
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_window_attributes_request_t);  -- /usr/include/xcb/xproto.h:1481

   type xcb_map_state_t is
     (XCB_MAP_STATE_UNMAPPED,
      XCB_MAP_STATE_UNVIEWABLE,
      XCB_MAP_STATE_VIEWABLE);
   pragma Convention (C, xcb_map_state_t);  -- /usr/include/xcb/xproto.h:1489

   type xcb_get_window_attributes_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1499
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_window_attributes_cookie_t);  -- /usr/include/xcb/xproto.h:1498

   type xcb_get_window_attributes_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1509
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1510
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1511
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1512
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_window_attributes_request_t);  -- /usr/include/xcb/xproto.h:1508

   type xcb_get_window_attributes_reply_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_get_window_attributes_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1519
      backing_store : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1520
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1521
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1522
      visual : aliased xcb_visualid_t;  -- /usr/include/xcb/xproto.h:1523
      u_class : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1524
      bit_gravity : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1525
      win_gravity : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1526
      backing_planes : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1527
      backing_pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1528
      save_under : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1529
      map_is_installed : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1530
      map_state : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1531
      override_redirect : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1532
      colormap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:1533
      all_event_masks : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1534
      your_event_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1535
      do_not_propagate_mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1536
      pad0 : aliased xcb_get_window_attributes_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:1537
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_window_attributes_reply_t);  -- /usr/include/xcb/xproto.h:1518

   type xcb_destroy_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1547
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1548
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1549
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1550
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_destroy_window_request_t);  -- /usr/include/xcb/xproto.h:1546

   type xcb_destroy_subwindows_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1560
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1561
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1562
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1563
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_destroy_subwindows_request_t);  -- /usr/include/xcb/xproto.h:1559

   type xcb_set_mode_t is
     (XCB_SET_MODE_INSERT,
      XCB_SET_MODE_DELETE);
   pragma Convention (C, xcb_set_mode_t);  -- /usr/include/xcb/xproto.h:1566

   type xcb_change_save_set_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1578
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1579
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1580
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1581
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_save_set_request_t);  -- /usr/include/xcb/xproto.h:1577

   type xcb_reparent_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1591
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1592
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1593
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1594
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1595
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:1596
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:1597
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_reparent_window_request_t);  -- /usr/include/xcb/xproto.h:1590

   type xcb_map_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1607
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1608
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1609
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1610
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_map_window_request_t);  -- /usr/include/xcb/xproto.h:1606

   type xcb_map_subwindows_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1620
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1621
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1622
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1623
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_map_subwindows_request_t);  -- /usr/include/xcb/xproto.h:1619

   type xcb_unmap_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1633
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1634
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1635
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1636
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_unmap_window_request_t);  -- /usr/include/xcb/xproto.h:1632

   type xcb_unmap_subwindows_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1646
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1647
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1648
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1649
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_unmap_subwindows_request_t);  -- /usr/include/xcb/xproto.h:1645

   subtype xcb_config_window_t is unsigned;
   XCB_CONFIG_WINDOW_X : constant xcb_config_window_t := 1;
   XCB_CONFIG_WINDOW_Y : constant xcb_config_window_t := 2;
   XCB_CONFIG_WINDOW_WIDTH : constant xcb_config_window_t := 4;
   XCB_CONFIG_WINDOW_HEIGHT : constant xcb_config_window_t := 8;
   XCB_CONFIG_WINDOW_BORDER_WIDTH : constant xcb_config_window_t := 16;
   XCB_CONFIG_WINDOW_SIBLING : constant xcb_config_window_t := 32;
   XCB_CONFIG_WINDOW_STACK_MODE : constant xcb_config_window_t := 64;  -- /usr/include/xcb/xproto.h:1652

   type xcb_stack_mode_t is
     (XCB_STACK_MODE_ABOVE,
      XCB_STACK_MODE_BELOW,
      XCB_STACK_MODE_TOP_IF,
      XCB_STACK_MODE_BOTTOM_IF,
      XCB_STACK_MODE_OPPOSITE);
   pragma Convention (C, xcb_stack_mode_t);  -- /usr/include/xcb/xproto.h:1662

   type xcb_configure_window_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_configure_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1677
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1678
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1679
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1680
      value_mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1681
      pad1 : aliased xcb_configure_window_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:1682
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_configure_window_request_t);  -- /usr/include/xcb/xproto.h:1676

   type xcb_circulate_t is
     (XCB_CIRCULATE_RAISE_LOWEST,
      XCB_CIRCULATE_LOWER_HIGHEST);
   pragma Convention (C, xcb_circulate_t);  -- /usr/include/xcb/xproto.h:1685

   type xcb_circulate_window_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1697
      direction : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1698
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1699
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1700
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_circulate_window_request_t);  -- /usr/include/xcb/xproto.h:1696

   type xcb_get_geometry_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1707
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_geometry_cookie_t);  -- /usr/include/xcb/xproto.h:1706

   type xcb_get_geometry_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1717
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1718
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1719
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:1720
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_geometry_request_t);  -- /usr/include/xcb/xproto.h:1716

   type xcb_get_geometry_reply_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_get_geometry_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1727
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1728
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1729
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1730
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1731
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:1732
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:1733
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1734
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1735
      border_width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1736
      pad0 : aliased xcb_get_geometry_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:1737
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_geometry_reply_t);  -- /usr/include/xcb/xproto.h:1726

   type xcb_query_tree_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1744
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_tree_cookie_t);  -- /usr/include/xcb/xproto.h:1743

   type xcb_query_tree_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1754
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1755
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1756
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1757
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_tree_request_t);  -- /usr/include/xcb/xproto.h:1753

   type xcb_query_tree_reply_t_pad1_array is array (0 .. 13) of aliased C99.Stdint.uint8_t;
   type xcb_query_tree_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1764
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1765
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1766
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1767
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1768
      parent : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1769
      children_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1770
      pad1 : aliased xcb_query_tree_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:1771
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_tree_reply_t);  -- /usr/include/xcb/xproto.h:1763

   type xcb_intern_atom_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1778
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_intern_atom_cookie_t);  -- /usr/include/xcb/xproto.h:1777

   type xcb_intern_atom_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_intern_atom_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1788
      only_if_exists : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1789
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1790
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1791
      pad0 : aliased xcb_intern_atom_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:1792
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_intern_atom_request_t);  -- /usr/include/xcb/xproto.h:1787

   type xcb_intern_atom_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1799
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1800
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1801
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1802
      atom : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1803
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_intern_atom_reply_t);  -- /usr/include/xcb/xproto.h:1798

   type xcb_get_atom_name_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1810
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_atom_name_cookie_t);  -- /usr/include/xcb/xproto.h:1809

   type xcb_get_atom_name_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1820
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1821
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1822
      atom : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1823
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_atom_name_request_t);  -- /usr/include/xcb/xproto.h:1819

   type xcb_get_atom_name_reply_t_pad1_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_get_atom_name_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1830
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1831
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1832
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1833
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1834
      pad1 : aliased xcb_get_atom_name_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:1835
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_atom_name_reply_t);  -- /usr/include/xcb/xproto.h:1829

   type xcb_prop_mode_t is
     (XCB_PROP_MODE_REPLACE,
      XCB_PROP_MODE_PREPEND,
      XCB_PROP_MODE_APPEND);
   pragma Convention (C, xcb_prop_mode_t);  -- /usr/include/xcb/xproto.h:1838

   type xcb_change_property_request_t_pad0_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_change_property_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1861
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1862
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1863
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1864
      property : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1865
      c_type : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1866
      format : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1867
      pad0 : aliased xcb_change_property_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:1868
      data_len : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1869
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_property_request_t);  -- /usr/include/xcb/xproto.h:1860

   type xcb_delete_property_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1879
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1880
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1881
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1882
      property : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1883
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_delete_property_request_t);  -- /usr/include/xcb/xproto.h:1878

   type xcb_get_property_type_t is
     (XCB_GET_PROPERTY_TYPE_ANY);
   pragma Convention (C, xcb_get_property_type_t);  -- /usr/include/xcb/xproto.h:1886

   type xcb_get_property_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1894
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_property_cookie_t);  -- /usr/include/xcb/xproto.h:1893

   type xcb_get_property_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1904
      u_delete : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1905
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1906
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1907
      property : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1908
      c_type : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1909
      long_offset : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1910
      long_length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1911
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_property_request_t);  -- /usr/include/xcb/xproto.h:1903

   type xcb_get_property_reply_t_pad0_array is array (0 .. 11) of aliased C99.Stdint.uint8_t;
   type xcb_get_property_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1918
      format : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1919
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1920
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1921
      c_type : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1922
      bytes_after : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1923
      value_len : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1924
      pad0 : aliased xcb_get_property_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:1925
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_property_reply_t);  -- /usr/include/xcb/xproto.h:1917

   type xcb_list_properties_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1932
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_properties_cookie_t);  -- /usr/include/xcb/xproto.h:1931

   type xcb_list_properties_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1942
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1943
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1944
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1945
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_properties_request_t);  -- /usr/include/xcb/xproto.h:1941

   type xcb_list_properties_reply_t_pad1_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_list_properties_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1952
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1953
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1954
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:1955
      atoms_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1956
      pad1 : aliased xcb_list_properties_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:1957
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_properties_reply_t);  -- /usr/include/xcb/xproto.h:1951

   type xcb_set_selection_owner_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1967
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1968
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1969
      owner : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:1970
      selection : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1971
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:1972
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_selection_owner_request_t);  -- /usr/include/xcb/xproto.h:1966

   type xcb_get_selection_owner_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:1979
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_selection_owner_cookie_t);  -- /usr/include/xcb/xproto.h:1978

   type xcb_get_selection_owner_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1989
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1990
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:1991
      selection : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:1992
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_selection_owner_request_t);  -- /usr/include/xcb/xproto.h:1988

   type xcb_get_selection_owner_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:1999
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2000
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2001
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2002
      owner : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2003
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_selection_owner_reply_t);  -- /usr/include/xcb/xproto.h:1998

   type xcb_convert_selection_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2013
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2014
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2015
      requestor : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2016
      selection : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:2017
      target : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:2018
      property : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:2019
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2020
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_convert_selection_request_t);  -- /usr/include/xcb/xproto.h:2012

   type xcb_send_event_dest_t is
     (XCB_SEND_EVENT_DEST_POINTER_WINDOW,
      XCB_SEND_EVENT_DEST_ITEM_FOCUS);
   pragma Convention (C, xcb_send_event_dest_t);  -- /usr/include/xcb/xproto.h:2023

   subtype xcb_send_event_request_t_event_array is Interfaces.C.char_array (0 .. 31);
   type xcb_send_event_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2035
      propagate : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2036
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2037
      destination : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2038
      event_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2039
      event : aliased xcb_send_event_request_t_event_array;  -- /usr/include/xcb/xproto.h:2040
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_send_event_request_t);  -- /usr/include/xcb/xproto.h:2034

   type xcb_grab_mode_t is
     (XCB_GRAB_MODE_SYNC,
      XCB_GRAB_MODE_ASYNC);
   pragma Convention (C, xcb_grab_mode_t);  -- /usr/include/xcb/xproto.h:2043

   type xcb_grab_status_t is
     (XCB_GRAB_STATUS_SUCCESS,
      XCB_GRAB_STATUS_ALREADY_GRABBED,
      XCB_GRAB_STATUS_INVALID_TIME,
      XCB_GRAB_STATUS_NOT_VIEWABLE,
      XCB_GRAB_STATUS_FROZEN);
   pragma Convention (C, xcb_grab_status_t);  -- /usr/include/xcb/xproto.h:2054

   type xcb_cursor_enum_t is
     (XCB_CURSOR_NONE);
   pragma Convention (C, xcb_cursor_enum_t);  -- /usr/include/xcb/xproto.h:2062

   type xcb_grab_pointer_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2070
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_pointer_cookie_t);  -- /usr/include/xcb/xproto.h:2069

   type xcb_grab_pointer_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2080
      owner_events : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2081
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2082
      grab_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2083
      event_mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2084
      pointer_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2085
      keyboard_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2086
      confine_to : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2087
      cursor : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:2088
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2089
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_pointer_request_t);  -- /usr/include/xcb/xproto.h:2079

   type xcb_grab_pointer_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2096
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2097
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2098
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2099
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_pointer_reply_t);  -- /usr/include/xcb/xproto.h:2095

   type xcb_ungrab_pointer_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2109
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2110
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2111
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2112
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ungrab_pointer_request_t);  -- /usr/include/xcb/xproto.h:2108

   type xcb_button_index_t is
     (XCB_BUTTON_INDEX_ANY,
      XCB_BUTTON_INDEX_1,
      XCB_BUTTON_INDEX_2,
      XCB_BUTTON_INDEX_3,
      XCB_BUTTON_INDEX_4,
      XCB_BUTTON_INDEX_5);
   pragma Convention (C, xcb_button_index_t);  -- /usr/include/xcb/xproto.h:2115

   type xcb_grab_button_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2143
      owner_events : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2144
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2145
      grab_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2146
      event_mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2147
      pointer_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2148
      keyboard_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2149
      confine_to : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2150
      cursor : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:2151
      button : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2152
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2153
      modifiers : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2154
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_button_request_t);  -- /usr/include/xcb/xproto.h:2142

   type xcb_ungrab_button_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_ungrab_button_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2164
      button : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2165
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2166
      grab_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2167
      modifiers : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2168
      pad0 : aliased xcb_ungrab_button_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:2169
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ungrab_button_request_t);  -- /usr/include/xcb/xproto.h:2163

   type xcb_change_active_pointer_grab_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_change_active_pointer_grab_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2179
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2180
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2181
      cursor : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:2182
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2183
      event_mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2184
      pad1 : aliased xcb_change_active_pointer_grab_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:2185
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_active_pointer_grab_request_t);  -- /usr/include/xcb/xproto.h:2178

   type xcb_grab_keyboard_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2192
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_keyboard_cookie_t);  -- /usr/include/xcb/xproto.h:2191

   type xcb_grab_keyboard_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_grab_keyboard_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2202
      owner_events : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2203
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2204
      grab_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2205
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2206
      pointer_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2207
      keyboard_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2208
      pad0 : aliased xcb_grab_keyboard_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:2209
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_keyboard_request_t);  -- /usr/include/xcb/xproto.h:2201

   type xcb_grab_keyboard_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2216
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2217
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2218
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2219
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_keyboard_reply_t);  -- /usr/include/xcb/xproto.h:2215

   type xcb_ungrab_keyboard_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2229
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2230
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2231
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2232
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ungrab_keyboard_request_t);  -- /usr/include/xcb/xproto.h:2228

   type xcb_grab_t is
     (XCB_GRAB_ANY);
   pragma Convention (C, xcb_grab_t);  -- /usr/include/xcb/xproto.h:2235

   type xcb_grab_key_request_t_pad0_array is array (0 .. 2) of aliased C99.Stdint.uint8_t;
   type xcb_grab_key_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2246
      owner_events : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2247
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2248
      grab_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2249
      modifiers : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2250
      key : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:2251
      pointer_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2252
      keyboard_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2253
      pad0 : aliased xcb_grab_key_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:2254
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_key_request_t);  -- /usr/include/xcb/xproto.h:2245

   type xcb_ungrab_key_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_ungrab_key_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2264
      key : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:2265
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2266
      grab_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2267
      modifiers : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2268
      pad0 : aliased xcb_ungrab_key_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:2269
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ungrab_key_request_t);  -- /usr/include/xcb/xproto.h:2263

   type xcb_allow_t is
     (XCB_ALLOW_ASYNC_POINTER,
      XCB_ALLOW_SYNC_POINTER,
      XCB_ALLOW_REPLAY_POINTER,
      XCB_ALLOW_ASYNC_KEYBOARD,
      XCB_ALLOW_SYNC_KEYBOARD,
      XCB_ALLOW_REPLAY_KEYBOARD,
      XCB_ALLOW_ASYNC_BOTH,
      XCB_ALLOW_SYNC_BOTH);
   pragma Convention (C, xcb_allow_t);  -- /usr/include/xcb/xproto.h:2272

   type xcb_allow_events_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2355
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2356
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2357
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2358
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_allow_events_request_t);  -- /usr/include/xcb/xproto.h:2354

   type xcb_grab_server_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2368
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2369
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2370
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_grab_server_request_t);  -- /usr/include/xcb/xproto.h:2367

   type xcb_ungrab_server_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2380
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2381
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2382
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_ungrab_server_request_t);  -- /usr/include/xcb/xproto.h:2379

   type xcb_query_pointer_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2389
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_pointer_cookie_t);  -- /usr/include/xcb/xproto.h:2388

   type xcb_query_pointer_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2399
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2400
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2401
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2402
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_pointer_request_t);  -- /usr/include/xcb/xproto.h:2398

   type xcb_query_pointer_reply_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_query_pointer_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2409
      same_screen : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2410
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2411
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2412
      root : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2413
      child : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2414
      root_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2415
      root_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2416
      win_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2417
      win_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2418
      mask : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2419
      pad0 : aliased xcb_query_pointer_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:2420
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_pointer_reply_t);  -- /usr/include/xcb/xproto.h:2408

   type xcb_timecoord_t is record
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2427
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2428
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2429
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_timecoord_t);  -- /usr/include/xcb/xproto.h:2426

   type xcb_timecoord_iterator_t is record
      data : access xcb_timecoord_t;  -- /usr/include/xcb/xproto.h:2436
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:2437
      index : aliased int;  -- /usr/include/xcb/xproto.h:2438
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_timecoord_iterator_t);  -- /usr/include/xcb/xproto.h:2435

   type xcb_get_motion_events_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2445
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_motion_events_cookie_t);  -- /usr/include/xcb/xproto.h:2444

   type xcb_get_motion_events_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2455
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2456
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2457
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2458
      start : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2459
      stop : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2460
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_motion_events_request_t);  -- /usr/include/xcb/xproto.h:2454

   type xcb_get_motion_events_reply_t_pad1_array is array (0 .. 19) of aliased C99.Stdint.uint8_t;
   type xcb_get_motion_events_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2467
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2468
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2469
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2470
      events_len : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2471
      pad1 : aliased xcb_get_motion_events_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:2472
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_motion_events_reply_t);  -- /usr/include/xcb/xproto.h:2466

   type xcb_translate_coordinates_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2479
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_translate_coordinates_cookie_t);  -- /usr/include/xcb/xproto.h:2478

   type xcb_translate_coordinates_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2489
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2490
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2491
      src_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2492
      dst_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2493
      src_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2494
      src_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2495
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_translate_coordinates_request_t);  -- /usr/include/xcb/xproto.h:2488

   type xcb_translate_coordinates_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2502
      same_screen : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2503
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2504
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2505
      child : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2506
      dst_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2507
      dst_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2508
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_translate_coordinates_reply_t);  -- /usr/include/xcb/xproto.h:2501

   type xcb_warp_pointer_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2518
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2519
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2520
      src_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2521
      dst_window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2522
      src_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2523
      src_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2524
      src_width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2525
      src_height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2526
      dst_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2527
      dst_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2528
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_warp_pointer_request_t);  -- /usr/include/xcb/xproto.h:2517

   type xcb_input_focus_t is
     (XCB_INPUT_FOCUS_NONE,
      XCB_INPUT_FOCUS_POINTER_ROOT,
      XCB_INPUT_FOCUS_PARENT,
      XCB_INPUT_FOCUS_FOLLOW_KEYBOARD);
   pragma Convention (C, xcb_input_focus_t);  -- /usr/include/xcb/xproto.h:2531

   type xcb_set_input_focus_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2556
      revert_to : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2557
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2558
      focus : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2559
      time : aliased xcb_timestamp_t;  -- /usr/include/xcb/xproto.h:2560
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_input_focus_request_t);  -- /usr/include/xcb/xproto.h:2555

   type xcb_get_input_focus_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2567
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_input_focus_cookie_t);  -- /usr/include/xcb/xproto.h:2566

   type xcb_get_input_focus_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2577
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2578
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2579
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_input_focus_request_t);  -- /usr/include/xcb/xproto.h:2576

   type xcb_get_input_focus_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2586
      revert_to : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2587
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2588
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2589
      focus : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:2590
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_input_focus_reply_t);  -- /usr/include/xcb/xproto.h:2585

   type xcb_query_keymap_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2597
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_keymap_cookie_t);  -- /usr/include/xcb/xproto.h:2596

   type xcb_query_keymap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2607
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2608
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2609
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_keymap_request_t);  -- /usr/include/xcb/xproto.h:2606

   type xcb_query_keymap_reply_t_keys_array is array (0 .. 31) of aliased C99.Stdint.uint8_t;
   type xcb_query_keymap_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2616
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2617
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2618
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2619
      keys : aliased xcb_query_keymap_reply_t_keys_array;  -- /usr/include/xcb/xproto.h:2620
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_keymap_reply_t);  -- /usr/include/xcb/xproto.h:2615

   type xcb_open_font_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_open_font_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2630
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2631
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2632
      fid : aliased xcb_font_t;  -- /usr/include/xcb/xproto.h:2633
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2634
      pad1 : aliased xcb_open_font_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:2635
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_open_font_request_t);  -- /usr/include/xcb/xproto.h:2629

   type xcb_close_font_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2645
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2646
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2647
      font : aliased xcb_font_t;  -- /usr/include/xcb/xproto.h:2648
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_close_font_request_t);  -- /usr/include/xcb/xproto.h:2644

   type xcb_font_draw_t is
     (XCB_FONT_DRAW_LEFT_TO_RIGHT,
      XCB_FONT_DRAW_RIGHT_TO_LEFT);
   pragma Convention (C, xcb_font_draw_t);  -- /usr/include/xcb/xproto.h:2651

   type xcb_fontprop_t is record
      name : aliased xcb_atom_t;  -- /usr/include/xcb/xproto.h:2660
      value : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2661
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_fontprop_t);  -- /usr/include/xcb/xproto.h:2659

   type xcb_fontprop_iterator_t is record
      data : access xcb_fontprop_t;  -- /usr/include/xcb/xproto.h:2668
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:2669
      index : aliased int;  -- /usr/include/xcb/xproto.h:2670
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_fontprop_iterator_t);  -- /usr/include/xcb/xproto.h:2667

   type xcb_charinfo_t is record
      left_side_bearing : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2677
      right_side_bearing : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2678
      character_width : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2679
      ascent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2680
      descent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2681
      attributes : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2682
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_charinfo_t);  -- /usr/include/xcb/xproto.h:2676

   type xcb_charinfo_iterator_t is record
      data : access xcb_charinfo_t;  -- /usr/include/xcb/xproto.h:2689
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:2690
      index : aliased int;  -- /usr/include/xcb/xproto.h:2691
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_charinfo_iterator_t);  -- /usr/include/xcb/xproto.h:2688

   type xcb_query_font_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2698
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_font_cookie_t);  -- /usr/include/xcb/xproto.h:2697

   type xcb_query_font_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2708
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2709
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2710
      font : aliased xcb_fontable_t;  -- /usr/include/xcb/xproto.h:2711
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_font_request_t);  -- /usr/include/xcb/xproto.h:2707

   type xcb_query_font_reply_t_pad1_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_query_font_reply_t_pad2_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_query_font_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2718
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2719
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2720
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2721
      min_bounds : aliased xcb_charinfo_t;  -- /usr/include/xcb/xproto.h:2722
      pad1 : aliased xcb_query_font_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:2723
      max_bounds : aliased xcb_charinfo_t;  -- /usr/include/xcb/xproto.h:2724
      pad2 : aliased xcb_query_font_reply_t_pad2_array;  -- /usr/include/xcb/xproto.h:2725
      min_char_or_byte2 : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2726
      max_char_or_byte2 : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2727
      default_char : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2728
      properties_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2729
      draw_direction : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2730
      min_byte1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2731
      max_byte1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2732
      all_chars_exist : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2733
      font_ascent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2734
      font_descent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2735
      char_infos_len : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2736
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_font_reply_t);  -- /usr/include/xcb/xproto.h:2717

   type xcb_query_text_extents_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2743
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_text_extents_cookie_t);  -- /usr/include/xcb/xproto.h:2742

   type xcb_query_text_extents_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2753
      odd_length : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2754
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2755
      font : aliased xcb_fontable_t;  -- /usr/include/xcb/xproto.h:2756
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_text_extents_request_t);  -- /usr/include/xcb/xproto.h:2752

   type xcb_query_text_extents_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2763
      draw_direction : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2764
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2765
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2766
      font_ascent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2767
      font_descent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2768
      overall_ascent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2769
      overall_descent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2770
      overall_width : aliased C99.Stdint.int32_t;  -- /usr/include/xcb/xproto.h:2771
      overall_left : aliased C99.Stdint.int32_t;  -- /usr/include/xcb/xproto.h:2772
      overall_right : aliased C99.Stdint.int32_t;  -- /usr/include/xcb/xproto.h:2773
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_text_extents_reply_t);  -- /usr/include/xcb/xproto.h:2762

   type xcb_str_t is record
      name_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2780
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_str_t);  -- /usr/include/xcb/xproto.h:2779

   type xcb_str_iterator_t is record
      data : access xcb_str_t;  -- /usr/include/xcb/xproto.h:2787
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:2788
      index : aliased int;  -- /usr/include/xcb/xproto.h:2789
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_str_iterator_t);  -- /usr/include/xcb/xproto.h:2786

   type xcb_list_fonts_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2796
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_fonts_cookie_t);  -- /usr/include/xcb/xproto.h:2795

   type xcb_list_fonts_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2806
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2807
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2808
      max_names : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2809
      pattern_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2810
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_fonts_request_t);  -- /usr/include/xcb/xproto.h:2805

   type xcb_list_fonts_reply_t_pad1_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_list_fonts_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2817
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2818
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2819
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2820
      names_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2821
      pad1 : aliased xcb_list_fonts_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:2822
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_fonts_reply_t);  -- /usr/include/xcb/xproto.h:2816

   type xcb_list_fonts_with_info_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2829
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_fonts_with_info_cookie_t);  -- /usr/include/xcb/xproto.h:2828

   type xcb_list_fonts_with_info_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2839
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2840
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2841
      max_names : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2842
      pattern_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2843
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_fonts_with_info_request_t);  -- /usr/include/xcb/xproto.h:2838

   type xcb_list_fonts_with_info_reply_t_pad0_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_list_fonts_with_info_reply_t_pad1_array is array (0 .. 3) of aliased C99.Stdint.uint8_t;
   type xcb_list_fonts_with_info_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2850
      name_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2851
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2852
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2853
      min_bounds : aliased xcb_charinfo_t;  -- /usr/include/xcb/xproto.h:2854
      pad0 : aliased xcb_list_fonts_with_info_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:2855
      max_bounds : aliased xcb_charinfo_t;  -- /usr/include/xcb/xproto.h:2856
      pad1 : aliased xcb_list_fonts_with_info_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:2857
      min_char_or_byte2 : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2858
      max_char_or_byte2 : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2859
      default_char : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2860
      properties_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2861
      draw_direction : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2862
      min_byte1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2863
      max_byte1 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2864
      all_chars_exist : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2865
      font_ascent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2866
      font_descent : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:2867
      replies_hint : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2868
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_fonts_with_info_reply_t);  -- /usr/include/xcb/xproto.h:2849

   type xcb_set_font_path_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_set_font_path_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2878
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2879
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2880
      font_qty : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2881
      pad1 : aliased xcb_set_font_path_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:2882
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_font_path_request_t);  -- /usr/include/xcb/xproto.h:2877

   type xcb_get_font_path_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:2889
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_font_path_cookie_t);  -- /usr/include/xcb/xproto.h:2888

   type xcb_get_font_path_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2899
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2900
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2901
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_font_path_request_t);  -- /usr/include/xcb/xproto.h:2898

   type xcb_get_font_path_reply_t_pad1_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_get_font_path_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2908
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2909
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2910
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:2911
      path_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2912
      pad1 : aliased xcb_get_font_path_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:2913
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_font_path_reply_t);  -- /usr/include/xcb/xproto.h:2907

   type xcb_create_pixmap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2923
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2924
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2925
      pid : aliased xcb_pixmap_t;  -- /usr/include/xcb/xproto.h:2926
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:2927
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2928
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2929
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_pixmap_request_t);  -- /usr/include/xcb/xproto.h:2922

   type xcb_free_pixmap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2939
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:2940
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:2941
      pixmap : aliased xcb_pixmap_t;  -- /usr/include/xcb/xproto.h:2942
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_free_pixmap_request_t);  -- /usr/include/xcb/xproto.h:2938

   subtype xcb_gc_t is unsigned;
   XCB_GC_FUNCTION : constant xcb_gc_t := 1;
   XCB_GC_PLANE_MASK : constant xcb_gc_t := 2;
   XCB_GC_FOREGROUND : constant xcb_gc_t := 4;
   XCB_GC_BACKGROUND : constant xcb_gc_t := 8;
   XCB_GC_LINE_WIDTH : constant xcb_gc_t := 16;
   XCB_GC_LINE_STYLE : constant xcb_gc_t := 32;
   XCB_GC_CAP_STYLE : constant xcb_gc_t := 64;
   XCB_GC_JOIN_STYLE : constant xcb_gc_t := 128;
   XCB_GC_FILL_STYLE : constant xcb_gc_t := 256;
   XCB_GC_FILL_RULE : constant xcb_gc_t := 512;
   XCB_GC_TILE : constant xcb_gc_t := 1024;
   XCB_GC_STIPPLE : constant xcb_gc_t := 2048;
   XCB_GC_TILE_STIPPLE_ORIGIN_X : constant xcb_gc_t := 4096;
   XCB_GC_TILE_STIPPLE_ORIGIN_Y : constant xcb_gc_t := 8192;
   XCB_GC_FONT : constant xcb_gc_t := 16384;
   XCB_GC_SUBWINDOW_MODE : constant xcb_gc_t := 32768;
   XCB_GC_GRAPHICS_EXPOSURES : constant xcb_gc_t := 65536;
   XCB_GC_CLIP_ORIGIN_X : constant xcb_gc_t := 131072;
   XCB_GC_CLIP_ORIGIN_Y : constant xcb_gc_t := 262144;
   XCB_GC_CLIP_MASK : constant xcb_gc_t := 524288;
   XCB_GC_DASH_OFFSET : constant xcb_gc_t := 1048576;
   XCB_GC_DASH_LIST : constant xcb_gc_t := 2097152;
   XCB_GC_ARC_MODE : constant xcb_gc_t := 4194304;  -- /usr/include/xcb/xproto.h:2945

   type xcb_gx_t is
     (XCB_GX_CLEAR,
      XCB_GX_AND,
      XCB_GX_AND_REVERSE,
      XCB_GX_COPY,
      XCB_GX_AND_INVERTED,
      XCB_GX_NOOP,
      XCB_GX_XOR,
      XCB_GX_OR,
      XCB_GX_NOR,
      XCB_GX_EQUIV,
      XCB_GX_INVERT,
      XCB_GX_OR_REVERSE,
      XCB_GX_COPY_INVERTED,
      XCB_GX_OR_INVERTED,
      XCB_GX_NAND,
      XCB_GX_SET);
   pragma Convention (C, xcb_gx_t);  -- /usr/include/xcb/xproto.h:3094

   type xcb_line_style_t is
     (XCB_LINE_STYLE_SOLID,
      XCB_LINE_STYLE_ON_OFF_DASH,
      XCB_LINE_STYLE_DOUBLE_DASH);
   pragma Convention (C, xcb_line_style_t);  -- /usr/include/xcb/xproto.h:3113

   type xcb_cap_style_t is
     (XCB_CAP_STYLE_NOT_LAST,
      XCB_CAP_STYLE_BUTT,
      XCB_CAP_STYLE_ROUND,
      XCB_CAP_STYLE_PROJECTING);
   pragma Convention (C, xcb_cap_style_t);  -- /usr/include/xcb/xproto.h:3119

   type xcb_join_style_t is
     (XCB_JOIN_STYLE_MITER,
      XCB_JOIN_STYLE_ROUND,
      XCB_JOIN_STYLE_BEVEL);
   pragma Convention (C, xcb_join_style_t);  -- /usr/include/xcb/xproto.h:3126

   type xcb_fill_style_t is
     (XCB_FILL_STYLE_SOLID,
      XCB_FILL_STYLE_TILED,
      XCB_FILL_STYLE_STIPPLED,
      XCB_FILL_STYLE_OPAQUE_STIPPLED);
   pragma Convention (C, xcb_fill_style_t);  -- /usr/include/xcb/xproto.h:3132

   type xcb_fill_rule_t is
     (XCB_FILL_RULE_EVEN_ODD,
      XCB_FILL_RULE_WINDING);
   pragma Convention (C, xcb_fill_rule_t);  -- /usr/include/xcb/xproto.h:3139

   type xcb_subwindow_mode_t is
     (XCB_SUBWINDOW_MODE_CLIP_BY_CHILDREN,
      XCB_SUBWINDOW_MODE_INCLUDE_INFERIORS);
   pragma Convention (C, xcb_subwindow_mode_t);  -- /usr/include/xcb/xproto.h:3144

   type xcb_arc_mode_t is
     (XCB_ARC_MODE_CHORD,
      XCB_ARC_MODE_PIE_SLICE);
   pragma Convention (C, xcb_arc_mode_t);  -- /usr/include/xcb/xproto.h:3149

   type xcb_create_gc_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3161
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3162
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3163
      cid : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3164
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3165
      value_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3166
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_gc_request_t);  -- /usr/include/xcb/xproto.h:3160

   type xcb_change_gc_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3176
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3177
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3178
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3179
      value_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3180
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_gc_request_t);  -- /usr/include/xcb/xproto.h:3175

   type xcb_copy_gc_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3190
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3191
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3192
      src_gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3193
      dst_gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3194
      value_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3195
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_copy_gc_request_t);  -- /usr/include/xcb/xproto.h:3189

   type xcb_set_dashes_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3205
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3206
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3207
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3208
      dash_offset : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3209
      dashes_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3210
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_dashes_request_t);  -- /usr/include/xcb/xproto.h:3204

   type xcb_clip_ordering_t is
     (XCB_CLIP_ORDERING_UNSORTED,
      XCB_CLIP_ORDERING_Y_SORTED,
      XCB_CLIP_ORDERING_YX_SORTED,
      XCB_CLIP_ORDERING_YX_BANDED);
   pragma Convention (C, xcb_clip_ordering_t);  -- /usr/include/xcb/xproto.h:3213

   type xcb_set_clip_rectangles_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3227
      ordering : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3228
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3229
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3230
      clip_x_origin : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3231
      clip_y_origin : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3232
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_clip_rectangles_request_t);  -- /usr/include/xcb/xproto.h:3226

   type xcb_free_gc_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3242
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3243
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3244
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3245
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_free_gc_request_t);  -- /usr/include/xcb/xproto.h:3241

   type xcb_clear_area_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3255
      exposures : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3256
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3257
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:3258
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3259
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3260
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3261
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3262
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_clear_area_request_t);  -- /usr/include/xcb/xproto.h:3254

   type xcb_copy_area_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3272
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3273
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3274
      src_drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3275
      dst_drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3276
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3277
      src_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3278
      src_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3279
      dst_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3280
      dst_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3281
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3282
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3283
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_copy_area_request_t);  -- /usr/include/xcb/xproto.h:3271

   type xcb_copy_plane_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3293
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3294
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3295
      src_drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3296
      dst_drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3297
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3298
      src_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3299
      src_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3300
      dst_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3301
      dst_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3302
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3303
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3304
      bit_plane : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3305
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_copy_plane_request_t);  -- /usr/include/xcb/xproto.h:3292

   type xcb_coord_mode_t is
     (XCB_COORD_MODE_ORIGIN,
      XCB_COORD_MODE_PREVIOUS);
   pragma Convention (C, xcb_coord_mode_t);  -- /usr/include/xcb/xproto.h:3308

   type xcb_poly_point_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3324
      coordinate_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3325
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3326
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3327
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3328
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_point_request_t);  -- /usr/include/xcb/xproto.h:3323

   type xcb_poly_line_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3338
      coordinate_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3339
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3340
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3341
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3342
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_line_request_t);  -- /usr/include/xcb/xproto.h:3337

   type xcb_segment_t is record
      x1 : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3349
      y1 : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3350
      x2 : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3351
      y2 : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3352
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_segment_t);  -- /usr/include/xcb/xproto.h:3348

   type xcb_segment_iterator_t is record
      data : access xcb_segment_t;  -- /usr/include/xcb/xproto.h:3359
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:3360
      index : aliased int;  -- /usr/include/xcb/xproto.h:3361
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_segment_iterator_t);  -- /usr/include/xcb/xproto.h:3358

   type xcb_poly_segment_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3371
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3372
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3373
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3374
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3375
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_segment_request_t);  -- /usr/include/xcb/xproto.h:3370

   type xcb_poly_rectangle_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3385
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3386
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3387
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3388
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3389
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_rectangle_request_t);  -- /usr/include/xcb/xproto.h:3384

   type xcb_poly_arc_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3399
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3400
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3401
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3402
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3403
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_arc_request_t);  -- /usr/include/xcb/xproto.h:3398

   type xcb_poly_shape_t is
     (XCB_POLY_SHAPE_COMPLEX,
      XCB_POLY_SHAPE_NONCONVEX,
      XCB_POLY_SHAPE_CONVEX);
   pragma Convention (C, xcb_poly_shape_t);  -- /usr/include/xcb/xproto.h:3406

   type xcb_fill_poly_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_fill_poly_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3419
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3420
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3421
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3422
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3423
      shape : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3424
      coordinate_mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3425
      pad1 : aliased xcb_fill_poly_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:3426
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_fill_poly_request_t);  -- /usr/include/xcb/xproto.h:3418

   type xcb_poly_fill_rectangle_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3436
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3437
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3438
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3439
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3440
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_fill_rectangle_request_t);  -- /usr/include/xcb/xproto.h:3435

   type xcb_poly_fill_arc_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3450
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3451
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3452
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3453
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3454
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_fill_arc_request_t);  -- /usr/include/xcb/xproto.h:3449

   type xcb_image_format_t is
     (XCB_IMAGE_FORMAT_XY_BITMAP,
      XCB_IMAGE_FORMAT_XY_PIXMAP,
      XCB_IMAGE_FORMAT_Z_PIXMAP);
   pragma Convention (C, xcb_image_format_t);  -- /usr/include/xcb/xproto.h:3457

   type xcb_put_image_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_put_image_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3470
      format : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3471
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3472
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3473
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3474
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3475
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3476
      dst_x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3477
      dst_y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3478
      left_pad : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3479
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3480
      pad0 : aliased xcb_put_image_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:3481
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_put_image_request_t);  -- /usr/include/xcb/xproto.h:3469

   type xcb_get_image_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3488
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_image_cookie_t);  -- /usr/include/xcb/xproto.h:3487

   type xcb_get_image_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3498
      format : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3499
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3500
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3501
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3502
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3503
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3504
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3505
      plane_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3506
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_image_request_t);  -- /usr/include/xcb/xproto.h:3497

   type xcb_get_image_reply_t_pad0_array is array (0 .. 19) of aliased C99.Stdint.uint8_t;
   type xcb_get_image_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3513
      depth : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3514
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3515
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3516
      visual : aliased xcb_visualid_t;  -- /usr/include/xcb/xproto.h:3517
      pad0 : aliased xcb_get_image_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:3518
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_image_reply_t);  -- /usr/include/xcb/xproto.h:3512

   type xcb_poly_text_8_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3528
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3529
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3530
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3531
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3532
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3533
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3534
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_text_8_request_t);  -- /usr/include/xcb/xproto.h:3527

   type xcb_poly_text_16_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3544
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3545
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3546
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3547
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3548
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3549
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3550
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_poly_text_16_request_t);  -- /usr/include/xcb/xproto.h:3543

   type xcb_image_text_8_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3560
      string_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3561
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3562
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3563
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3564
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3565
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3566
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_image_text_8_request_t);  -- /usr/include/xcb/xproto.h:3559

   type xcb_image_text_16_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3576
      string_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3577
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3578
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:3579
      gc : aliased xcb_gcontext_t;  -- /usr/include/xcb/xproto.h:3580
      x : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3581
      y : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:3582
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_image_text_16_request_t);  -- /usr/include/xcb/xproto.h:3575

   type xcb_colormap_alloc_t is
     (XCB_COLORMAP_ALLOC_NONE,
      XCB_COLORMAP_ALLOC_ALL);
   pragma Convention (C, xcb_colormap_alloc_t);  -- /usr/include/xcb/xproto.h:3585

   type xcb_create_colormap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3597
      alloc : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3598
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3599
      mid : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3600
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:3601
      visual : aliased xcb_visualid_t;  -- /usr/include/xcb/xproto.h:3602
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_colormap_request_t);  -- /usr/include/xcb/xproto.h:3596

   type xcb_free_colormap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3612
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3613
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3614
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3615
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_free_colormap_request_t);  -- /usr/include/xcb/xproto.h:3611

   type xcb_copy_colormap_and_free_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3625
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3626
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3627
      mid : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3628
      src_cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3629
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_copy_colormap_and_free_request_t);  -- /usr/include/xcb/xproto.h:3624

   type xcb_install_colormap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3639
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3640
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3641
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3642
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_install_colormap_request_t);  -- /usr/include/xcb/xproto.h:3638

   type xcb_uninstall_colormap_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3652
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3653
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3654
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3655
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_uninstall_colormap_request_t);  -- /usr/include/xcb/xproto.h:3651

   type xcb_list_installed_colormaps_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3662
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_installed_colormaps_cookie_t);  -- /usr/include/xcb/xproto.h:3661

   type xcb_list_installed_colormaps_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3672
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3673
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3674
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:3675
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_installed_colormaps_request_t);  -- /usr/include/xcb/xproto.h:3671

   type xcb_list_installed_colormaps_reply_t_pad1_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_list_installed_colormaps_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3682
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3683
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3684
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3685
      cmaps_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3686
      pad1 : aliased xcb_list_installed_colormaps_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:3687
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_installed_colormaps_reply_t);  -- /usr/include/xcb/xproto.h:3681

   type xcb_alloc_color_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3694
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_cookie_t);  -- /usr/include/xcb/xproto.h:3693

   type xcb_alloc_color_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_alloc_color_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3704
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3705
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3706
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3707
      red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3708
      green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3709
      blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3710
      pad1 : aliased xcb_alloc_color_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:3711
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_request_t);  -- /usr/include/xcb/xproto.h:3703

   type xcb_alloc_color_reply_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_alloc_color_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3718
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3719
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3720
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3721
      red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3722
      green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3723
      blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3724
      pad1 : aliased xcb_alloc_color_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:3725
      pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3726
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_reply_t);  -- /usr/include/xcb/xproto.h:3717

   type xcb_alloc_named_color_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3733
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_named_color_cookie_t);  -- /usr/include/xcb/xproto.h:3732

   type xcb_alloc_named_color_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_alloc_named_color_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3743
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3744
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3745
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3746
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3747
      pad1 : aliased xcb_alloc_named_color_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:3748
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_named_color_request_t);  -- /usr/include/xcb/xproto.h:3742

   type xcb_alloc_named_color_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3755
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3756
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3757
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3758
      pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3759
      exact_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3760
      exact_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3761
      exact_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3762
      visual_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3763
      visual_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3764
      visual_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3765
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_named_color_reply_t);  -- /usr/include/xcb/xproto.h:3754

   type xcb_alloc_color_cells_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3772
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_cells_cookie_t);  -- /usr/include/xcb/xproto.h:3771

   type xcb_alloc_color_cells_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3782
      contiguous : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3783
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3784
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3785
      colors : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3786
      planes : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3787
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_cells_request_t);  -- /usr/include/xcb/xproto.h:3781

   type xcb_alloc_color_cells_reply_t_pad1_array is array (0 .. 19) of aliased C99.Stdint.uint8_t;
   type xcb_alloc_color_cells_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3794
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3795
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3796
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3797
      pixels_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3798
      masks_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3799
      pad1 : aliased xcb_alloc_color_cells_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:3800
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_cells_reply_t);  -- /usr/include/xcb/xproto.h:3793

   type xcb_alloc_color_planes_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3807
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_planes_cookie_t);  -- /usr/include/xcb/xproto.h:3806

   type xcb_alloc_color_planes_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3817
      contiguous : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3818
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3819
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3820
      colors : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3821
      reds : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3822
      greens : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3823
      blues : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3824
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_planes_request_t);  -- /usr/include/xcb/xproto.h:3816

   type xcb_alloc_color_planes_reply_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_alloc_color_planes_reply_t_pad2_array is array (0 .. 7) of aliased C99.Stdint.uint8_t;
   type xcb_alloc_color_planes_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3831
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3832
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3833
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3834
      pixels_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3835
      pad1 : aliased xcb_alloc_color_planes_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:3836
      red_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3837
      green_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3838
      blue_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3839
      pad2 : aliased xcb_alloc_color_planes_reply_t_pad2_array;  -- /usr/include/xcb/xproto.h:3840
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_alloc_color_planes_reply_t);  -- /usr/include/xcb/xproto.h:3830

   type xcb_free_colors_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3850
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3851
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3852
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3853
      plane_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3854
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_free_colors_request_t);  -- /usr/include/xcb/xproto.h:3849

   subtype xcb_color_flag_t is unsigned;
   XCB_COLOR_FLAG_RED : constant xcb_color_flag_t := 1;
   XCB_COLOR_FLAG_GREEN : constant xcb_color_flag_t := 2;
   XCB_COLOR_FLAG_BLUE : constant xcb_color_flag_t := 4;  -- /usr/include/xcb/xproto.h:3857

   type xcb_coloritem_t is record
      pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3867
      red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3868
      green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3869
      blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3870
      flags : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3871
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3872
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_coloritem_t);  -- /usr/include/xcb/xproto.h:3866

   type xcb_coloritem_iterator_t is record
      data : access xcb_coloritem_t;  -- /usr/include/xcb/xproto.h:3879
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:3880
      index : aliased int;  -- /usr/include/xcb/xproto.h:3881
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_coloritem_iterator_t);  -- /usr/include/xcb/xproto.h:3878

   type xcb_store_colors_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3891
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3892
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3893
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3894
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_store_colors_request_t);  -- /usr/include/xcb/xproto.h:3890

   type xcb_store_named_color_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_store_named_color_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3904
      flags : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3905
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3906
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3907
      pixel : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3908
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3909
      pad0 : aliased xcb_store_named_color_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:3910
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_store_named_color_request_t);  -- /usr/include/xcb/xproto.h:3903

   type xcb_rgb_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_rgb_t is record
      red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3917
      green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3918
      blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3919
      pad0 : aliased xcb_rgb_t_pad0_array;  -- /usr/include/xcb/xproto.h:3920
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_rgb_t);  -- /usr/include/xcb/xproto.h:3916

   type xcb_rgb_iterator_t is record
      data : access xcb_rgb_t;  -- /usr/include/xcb/xproto.h:3927
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:3928
      index : aliased int;  -- /usr/include/xcb/xproto.h:3929
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_rgb_iterator_t);  -- /usr/include/xcb/xproto.h:3926

   type xcb_query_colors_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3936
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_colors_cookie_t);  -- /usr/include/xcb/xproto.h:3935

   type xcb_query_colors_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3946
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3947
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3948
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3949
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_colors_request_t);  -- /usr/include/xcb/xproto.h:3945

   type xcb_query_colors_reply_t_pad1_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_query_colors_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3956
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3957
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3958
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3959
      colors_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3960
      pad1 : aliased xcb_query_colors_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:3961
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_colors_reply_t);  -- /usr/include/xcb/xproto.h:3955

   type xcb_lookup_color_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:3968
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_lookup_color_cookie_t);  -- /usr/include/xcb/xproto.h:3967

   type xcb_lookup_color_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_lookup_color_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3978
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3979
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3980
      cmap : aliased xcb_colormap_t;  -- /usr/include/xcb/xproto.h:3981
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3982
      pad1 : aliased xcb_lookup_color_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:3983
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_lookup_color_request_t);  -- /usr/include/xcb/xproto.h:3977

   type xcb_lookup_color_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3990
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:3991
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3992
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:3993
      exact_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3994
      exact_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3995
      exact_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3996
      visual_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3997
      visual_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3998
      visual_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:3999
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_lookup_color_reply_t);  -- /usr/include/xcb/xproto.h:3989

   type xcb_pixmap_enum_t is
     (XCB_PIXMAP_NONE);
   pragma Convention (C, xcb_pixmap_enum_t);  -- /usr/include/xcb/xproto.h:4002

   type xcb_create_cursor_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4013
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4014
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4015
      cid : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:4016
      source : aliased xcb_pixmap_t;  -- /usr/include/xcb/xproto.h:4017
      mask : aliased xcb_pixmap_t;  -- /usr/include/xcb/xproto.h:4018
      fore_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4019
      fore_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4020
      fore_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4021
      back_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4022
      back_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4023
      back_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4024
      x : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4025
      y : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4026
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_cursor_request_t);  -- /usr/include/xcb/xproto.h:4012

   type xcb_font_enum_t is
     (XCB_FONT_NONE);
   pragma Convention (C, xcb_font_enum_t);  -- /usr/include/xcb/xproto.h:4029

   type xcb_create_glyph_cursor_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4040
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4041
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4042
      cid : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:4043
      source_font : aliased xcb_font_t;  -- /usr/include/xcb/xproto.h:4044
      mask_font : aliased xcb_font_t;  -- /usr/include/xcb/xproto.h:4045
      source_char : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4046
      mask_char : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4047
      fore_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4048
      fore_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4049
      fore_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4050
      back_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4051
      back_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4052
      back_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4053
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_create_glyph_cursor_request_t);  -- /usr/include/xcb/xproto.h:4039

   type xcb_free_cursor_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4063
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4064
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4065
      cursor : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:4066
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_free_cursor_request_t);  -- /usr/include/xcb/xproto.h:4062

   type xcb_recolor_cursor_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4076
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4077
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4078
      cursor : aliased xcb_cursor_t;  -- /usr/include/xcb/xproto.h:4079
      fore_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4080
      fore_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4081
      fore_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4082
      back_red : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4083
      back_green : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4084
      back_blue : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4085
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_recolor_cursor_request_t);  -- /usr/include/xcb/xproto.h:4075

   type xcb_query_shape_of_t is
     (XCB_QUERY_SHAPE_OF_LARGEST_CURSOR,
      XCB_QUERY_SHAPE_OF_FASTEST_TILE,
      XCB_QUERY_SHAPE_OF_FASTEST_STIPPLE);
   pragma Convention (C, xcb_query_shape_of_t);  -- /usr/include/xcb/xproto.h:4088

   type xcb_query_best_size_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4098
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_best_size_cookie_t);  -- /usr/include/xcb/xproto.h:4097

   type xcb_query_best_size_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4108
      u_class : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4109
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4110
      drawable : aliased xcb_drawable_t;  -- /usr/include/xcb/xproto.h:4111
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4112
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4113
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_best_size_request_t);  -- /usr/include/xcb/xproto.h:4107

   type xcb_query_best_size_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4120
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4121
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4122
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4123
      width : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4124
      height : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4125
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_best_size_reply_t);  -- /usr/include/xcb/xproto.h:4119

   type xcb_query_extension_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4132
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_extension_cookie_t);  -- /usr/include/xcb/xproto.h:4131

   type xcb_query_extension_request_t_pad1_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_query_extension_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4142
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4143
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4144
      name_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4145
      pad1 : aliased xcb_query_extension_request_t_pad1_array;  -- /usr/include/xcb/xproto.h:4146
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_extension_request_t);  -- /usr/include/xcb/xproto.h:4141

   type xcb_query_extension_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4153
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4154
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4155
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4156
      present : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4157
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4158
      first_event : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4159
      first_error : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4160
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_query_extension_reply_t);  -- /usr/include/xcb/xproto.h:4152

   type xcb_list_extensions_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4167
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_extensions_cookie_t);  -- /usr/include/xcb/xproto.h:4166

   type xcb_list_extensions_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4177
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4178
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4179
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_extensions_request_t);  -- /usr/include/xcb/xproto.h:4176

   type xcb_list_extensions_reply_t_pad0_array is array (0 .. 23) of aliased C99.Stdint.uint8_t;
   type xcb_list_extensions_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4186
      names_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4187
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4188
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4189
      pad0 : aliased xcb_list_extensions_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:4190
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_extensions_reply_t);  -- /usr/include/xcb/xproto.h:4185

   type xcb_change_keyboard_mapping_request_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_change_keyboard_mapping_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4200
      keycode_count : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4201
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4202
      first_keycode : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:4203
      keysyms_per_keycode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4204
      pad0 : aliased xcb_change_keyboard_mapping_request_t_pad0_array;  -- /usr/include/xcb/xproto.h:4205
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_keyboard_mapping_request_t);  -- /usr/include/xcb/xproto.h:4199

   type xcb_get_keyboard_mapping_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4212
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_keyboard_mapping_cookie_t);  -- /usr/include/xcb/xproto.h:4211

   type xcb_get_keyboard_mapping_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4222
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4223
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4224
      first_keycode : aliased xcb_keycode_t;  -- /usr/include/xcb/xproto.h:4225
      count : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4226
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_keyboard_mapping_request_t);  -- /usr/include/xcb/xproto.h:4221

   type xcb_get_keyboard_mapping_reply_t_pad0_array is array (0 .. 23) of aliased C99.Stdint.uint8_t;
   type xcb_get_keyboard_mapping_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4233
      keysyms_per_keycode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4234
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4235
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4236
      pad0 : aliased xcb_get_keyboard_mapping_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:4237
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_keyboard_mapping_reply_t);  -- /usr/include/xcb/xproto.h:4232

   subtype xcb_kb_t is unsigned;
   XCB_KB_KEY_CLICK_PERCENT : constant xcb_kb_t := 1;
   XCB_KB_BELL_PERCENT : constant xcb_kb_t := 2;
   XCB_KB_BELL_PITCH : constant xcb_kb_t := 4;
   XCB_KB_BELL_DURATION : constant xcb_kb_t := 8;
   XCB_KB_LED : constant xcb_kb_t := 16;
   XCB_KB_LED_MODE : constant xcb_kb_t := 32;
   XCB_KB_KEY : constant xcb_kb_t := 64;
   XCB_KB_AUTO_REPEAT_MODE : constant xcb_kb_t := 128;  -- /usr/include/xcb/xproto.h:4240

   type xcb_led_mode_t is
     (XCB_LED_MODE_OFF,
      XCB_LED_MODE_ON);
   pragma Convention (C, xcb_led_mode_t);  -- /usr/include/xcb/xproto.h:4251

   type xcb_auto_repeat_mode_t is
     (XCB_AUTO_REPEAT_MODE_OFF,
      XCB_AUTO_REPEAT_MODE_ON,
      XCB_AUTO_REPEAT_MODE_DEFAULT);
   pragma Convention (C, xcb_auto_repeat_mode_t);  -- /usr/include/xcb/xproto.h:4256

   type xcb_change_keyboard_control_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4269
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4270
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4271
      value_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4272
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_keyboard_control_request_t);  -- /usr/include/xcb/xproto.h:4268

   type xcb_get_keyboard_control_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4279
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_keyboard_control_cookie_t);  -- /usr/include/xcb/xproto.h:4278

   type xcb_get_keyboard_control_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4289
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4290
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4291
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_keyboard_control_request_t);  -- /usr/include/xcb/xproto.h:4288

   type xcb_get_keyboard_control_reply_t_pad0_array is array (0 .. 1) of aliased C99.Stdint.uint8_t;
   type xcb_get_keyboard_control_reply_t_auto_repeats_array is array (0 .. 31) of aliased C99.Stdint.uint8_t;
   type xcb_get_keyboard_control_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4298
      global_auto_repeat : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4299
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4300
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4301
      led_mask : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4302
      key_click_percent : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4303
      bell_percent : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4304
      bell_pitch : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4305
      bell_duration : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4306
      pad0 : aliased xcb_get_keyboard_control_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:4307
      auto_repeats : aliased xcb_get_keyboard_control_reply_t_auto_repeats_array;  -- /usr/include/xcb/xproto.h:4308
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_keyboard_control_reply_t);  -- /usr/include/xcb/xproto.h:4297

   type xcb_bell_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4318
      percent : aliased C99.Stdint.int8_t;  -- /usr/include/xcb/xproto.h:4319
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4320
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_bell_request_t);  -- /usr/include/xcb/xproto.h:4317

   type xcb_change_pointer_control_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4330
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4331
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4332
      acceleration_numerator : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:4333
      acceleration_denominator : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:4334
      threshold : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:4335
      do_acceleration : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4336
      do_threshold : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4337
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_pointer_control_request_t);  -- /usr/include/xcb/xproto.h:4329

   type xcb_get_pointer_control_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4344
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_pointer_control_cookie_t);  -- /usr/include/xcb/xproto.h:4343

   type xcb_get_pointer_control_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4354
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4355
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4356
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_pointer_control_request_t);  -- /usr/include/xcb/xproto.h:4353

   type xcb_get_pointer_control_reply_t_pad1_array is array (0 .. 17) of aliased C99.Stdint.uint8_t;
   type xcb_get_pointer_control_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4363
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4364
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4365
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4366
      acceleration_numerator : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4367
      acceleration_denominator : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4368
      threshold : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4369
      pad1 : aliased xcb_get_pointer_control_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:4370
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_pointer_control_reply_t);  -- /usr/include/xcb/xproto.h:4362

   type xcb_blanking_t is
     (XCB_BLANKING_NOT_PREFERRED,
      XCB_BLANKING_PREFERRED,
      XCB_BLANKING_DEFAULT);
   pragma Convention (C, xcb_blanking_t);  -- /usr/include/xcb/xproto.h:4373

   type xcb_exposures_t is
     (XCB_EXPOSURES_NOT_ALLOWED,
      XCB_EXPOSURES_ALLOWED,
      XCB_EXPOSURES_DEFAULT);
   pragma Convention (C, xcb_exposures_t);  -- /usr/include/xcb/xproto.h:4379

   type xcb_set_screen_saver_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4392
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4393
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4394
      timeout : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:4395
      interval : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:4396
      prefer_blanking : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4397
      allow_exposures : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4398
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_screen_saver_request_t);  -- /usr/include/xcb/xproto.h:4391

   type xcb_get_screen_saver_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4405
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_screen_saver_cookie_t);  -- /usr/include/xcb/xproto.h:4404

   type xcb_get_screen_saver_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4415
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4416
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4417
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_screen_saver_request_t);  -- /usr/include/xcb/xproto.h:4414

   type xcb_get_screen_saver_reply_t_pad1_array is array (0 .. 17) of aliased C99.Stdint.uint8_t;
   type xcb_get_screen_saver_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4424
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4425
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4426
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4427
      timeout : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4428
      interval : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4429
      prefer_blanking : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4430
      allow_exposures : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4431
      pad1 : aliased xcb_get_screen_saver_reply_t_pad1_array;  -- /usr/include/xcb/xproto.h:4432
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_screen_saver_reply_t);  -- /usr/include/xcb/xproto.h:4423

   type xcb_host_mode_t is
     (XCB_HOST_MODE_INSERT,
      XCB_HOST_MODE_DELETE);
   pragma Convention (C, xcb_host_mode_t);  -- /usr/include/xcb/xproto.h:4435

   subtype xcb_family_t is unsigned;
   XCB_FAMILY_INTERNET : constant xcb_family_t := 0;
   XCB_FAMILY_DECNET : constant xcb_family_t := 1;
   XCB_FAMILY_CHAOS : constant xcb_family_t := 2;
   XCB_FAMILY_SERVER_INTERPRETED : constant xcb_family_t := 5;
   XCB_FAMILY_INTERNET_6 : constant xcb_family_t := 6;  -- /usr/include/xcb/xproto.h:4440

   type xcb_change_hosts_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4455
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4456
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4457
      family : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4458
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4459
      address_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4460
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_change_hosts_request_t);  -- /usr/include/xcb/xproto.h:4454

   type xcb_host_t is record
      family : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4467
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4468
      address_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4469
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_host_t);  -- /usr/include/xcb/xproto.h:4466

   type xcb_host_iterator_t is record
      data : access xcb_host_t;  -- /usr/include/xcb/xproto.h:4476
      c_rem : aliased int;  -- /usr/include/xcb/xproto.h:4477
      index : aliased int;  -- /usr/include/xcb/xproto.h:4478
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_host_iterator_t);  -- /usr/include/xcb/xproto.h:4475

   type xcb_list_hosts_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4485
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_hosts_cookie_t);  -- /usr/include/xcb/xproto.h:4484

   type xcb_list_hosts_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4495
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4496
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4497
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_hosts_request_t);  -- /usr/include/xcb/xproto.h:4494

   type xcb_list_hosts_reply_t_pad0_array is array (0 .. 21) of aliased C99.Stdint.uint8_t;
   type xcb_list_hosts_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4504
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4505
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4506
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4507
      hosts_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4508
      pad0 : aliased xcb_list_hosts_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:4509
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_list_hosts_reply_t);  -- /usr/include/xcb/xproto.h:4503

   type xcb_access_control_t is
     (XCB_ACCESS_CONTROL_DISABLE,
      XCB_ACCESS_CONTROL_ENABLE);
   pragma Convention (C, xcb_access_control_t);  -- /usr/include/xcb/xproto.h:4512

   type xcb_set_access_control_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4524
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4525
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4526
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_access_control_request_t);  -- /usr/include/xcb/xproto.h:4523

   type xcb_close_down_t is
     (XCB_CLOSE_DOWN_DESTROY_ALL,
      XCB_CLOSE_DOWN_RETAIN_PERMANENT,
      XCB_CLOSE_DOWN_RETAIN_TEMPORARY);
   pragma Convention (C, xcb_close_down_t);  -- /usr/include/xcb/xproto.h:4529

   type xcb_set_close_down_mode_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4542
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4543
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4544
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_close_down_mode_request_t);  -- /usr/include/xcb/xproto.h:4541

   type xcb_kill_t is
     (XCB_KILL_ALL_TEMPORARY);
   pragma Convention (C, xcb_kill_t);  -- /usr/include/xcb/xproto.h:4547

   type xcb_kill_client_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4558
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4559
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4560
      resource : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4561
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_kill_client_request_t);  -- /usr/include/xcb/xproto.h:4557

   type xcb_rotate_properties_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4571
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4572
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4573
      window : aliased xcb_window_t;  -- /usr/include/xcb/xproto.h:4574
      atoms_len : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4575
      c_delta : aliased C99.Stdint.int16_t;  -- /usr/include/xcb/xproto.h:4576
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_rotate_properties_request_t);  -- /usr/include/xcb/xproto.h:4570

   type xcb_screen_saver_t is
     (XCB_SCREEN_SAVER_RESET,
      XCB_SCREEN_SAVER_ACTIVE);
   pragma Convention (C, xcb_screen_saver_t);  -- /usr/include/xcb/xproto.h:4579

   type xcb_force_screen_saver_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4591
      mode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4592
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4593
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_force_screen_saver_request_t);  -- /usr/include/xcb/xproto.h:4590

   type xcb_mapping_status_t is
     (XCB_MAPPING_STATUS_SUCCESS,
      XCB_MAPPING_STATUS_BUSY,
      XCB_MAPPING_STATUS_FAILURE);
   pragma Convention (C, xcb_mapping_status_t);  -- /usr/include/xcb/xproto.h:4596

   type xcb_set_pointer_mapping_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4606
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_pointer_mapping_cookie_t);  -- /usr/include/xcb/xproto.h:4605

   type xcb_set_pointer_mapping_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4616
      map_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4617
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4618
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_pointer_mapping_request_t);  -- /usr/include/xcb/xproto.h:4615

   type xcb_set_pointer_mapping_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4625
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4626
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4627
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4628
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_pointer_mapping_reply_t);  -- /usr/include/xcb/xproto.h:4624

   type xcb_get_pointer_mapping_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4635
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_pointer_mapping_cookie_t);  -- /usr/include/xcb/xproto.h:4634

   type xcb_get_pointer_mapping_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4645
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4646
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4647
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_pointer_mapping_request_t);  -- /usr/include/xcb/xproto.h:4644

   type xcb_get_pointer_mapping_reply_t_pad0_array is array (0 .. 23) of aliased C99.Stdint.uint8_t;
   type xcb_get_pointer_mapping_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4654
      map_len : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4655
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4656
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4657
      pad0 : aliased xcb_get_pointer_mapping_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:4658
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_pointer_mapping_reply_t);  -- /usr/include/xcb/xproto.h:4653

   type xcb_map_index_t is
     (XCB_MAP_INDEX_SHIFT,
      XCB_MAP_INDEX_LOCK,
      XCB_MAP_INDEX_CONTROL,
      XCB_MAP_INDEX_1,
      XCB_MAP_INDEX_2,
      XCB_MAP_INDEX_3,
      XCB_MAP_INDEX_4,
      XCB_MAP_INDEX_5);
   pragma Convention (C, xcb_map_index_t);  -- /usr/include/xcb/xproto.h:4661

   type xcb_set_modifier_mapping_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4676
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_modifier_mapping_cookie_t);  -- /usr/include/xcb/xproto.h:4675

   type xcb_set_modifier_mapping_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4686
      keycodes_per_modifier : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4687
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4688
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_modifier_mapping_request_t);  -- /usr/include/xcb/xproto.h:4685

   type xcb_set_modifier_mapping_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4695
      status : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4696
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4697
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4698
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_set_modifier_mapping_reply_t);  -- /usr/include/xcb/xproto.h:4694

   type xcb_get_modifier_mapping_cookie_t is record
      sequence : aliased unsigned;  -- /usr/include/xcb/xproto.h:4705
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_modifier_mapping_cookie_t);  -- /usr/include/xcb/xproto.h:4704

   type xcb_get_modifier_mapping_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4715
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4716
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4717
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_modifier_mapping_request_t);  -- /usr/include/xcb/xproto.h:4714

   type xcb_get_modifier_mapping_reply_t_pad0_array is array (0 .. 23) of aliased C99.Stdint.uint8_t;
   type xcb_get_modifier_mapping_reply_t is record
      response_type : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4724
      keycodes_per_modifier : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4725
      sequence : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4726
      length : aliased C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:4727
      pad0 : aliased xcb_get_modifier_mapping_reply_t_pad0_array;  -- /usr/include/xcb/xproto.h:4728
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_get_modifier_mapping_reply_t);  -- /usr/include/xcb/xproto.h:4723

   type xcb_no_operation_request_t is record
      major_opcode : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4738
      pad0 : aliased C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:4739
      length : aliased C99.Stdint.uint16_t;  -- /usr/include/xcb/xproto.h:4740
   end record;
   pragma Convention (C_Pass_By_Copy, xcb_no_operation_request_t);  -- /usr/include/xcb/xproto.h:4737

   procedure xcb_char2b_next (i : access xcb_char2b_iterator_t);  -- /usr/include/xcb/xproto.h:4762
   pragma Import (C, xcb_char2b_next, "xcb_char2b_next");

   function xcb_char2b_end (i : xcb_char2b_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:4784
   pragma Import (C, xcb_char2b_end, "xcb_char2b_end");

   procedure xcb_window_next (i : access xcb_window_iterator_t);  -- /usr/include/xcb/xproto.h:4805
   pragma Import (C, xcb_window_next, "xcb_window_next");

   function xcb_window_end (i : xcb_window_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:4827
   pragma Import (C, xcb_window_end, "xcb_window_end");

   procedure xcb_pixmap_next (i : access xcb_pixmap_iterator_t);  -- /usr/include/xcb/xproto.h:4848
   pragma Import (C, xcb_pixmap_next, "xcb_pixmap_next");

   function xcb_pixmap_end (i : xcb_pixmap_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:4870
   pragma Import (C, xcb_pixmap_end, "xcb_pixmap_end");

   procedure xcb_cursor_next (i : access xcb_cursor_iterator_t);  -- /usr/include/xcb/xproto.h:4891
   pragma Import (C, xcb_cursor_next, "xcb_cursor_next");

   function xcb_cursor_end (i : xcb_cursor_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:4913
   pragma Import (C, xcb_cursor_end, "xcb_cursor_end");

   procedure xcb_font_next (i : access xcb_font_iterator_t);  -- /usr/include/xcb/xproto.h:4934
   pragma Import (C, xcb_font_next, "xcb_font_next");

   function xcb_font_end (i : xcb_font_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:4956
   pragma Import (C, xcb_font_end, "xcb_font_end");

   procedure xcb_gcontext_next (i : access xcb_gcontext_iterator_t);  -- /usr/include/xcb/xproto.h:4977
   pragma Import (C, xcb_gcontext_next, "xcb_gcontext_next");

   function xcb_gcontext_end (i : xcb_gcontext_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:4999
   pragma Import (C, xcb_gcontext_end, "xcb_gcontext_end");

   procedure xcb_colormap_next (i : access xcb_colormap_iterator_t);  -- /usr/include/xcb/xproto.h:5020
   pragma Import (C, xcb_colormap_next, "xcb_colormap_next");

   function xcb_colormap_end (i : xcb_colormap_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5042
   pragma Import (C, xcb_colormap_end, "xcb_colormap_end");

   procedure xcb_atom_next (i : access xcb_atom_iterator_t);  -- /usr/include/xcb/xproto.h:5063
   pragma Import (C, xcb_atom_next, "xcb_atom_next");

   function xcb_atom_end (i : xcb_atom_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5085
   pragma Import (C, xcb_atom_end, "xcb_atom_end");

   procedure xcb_drawable_next (i : access xcb_drawable_iterator_t);  -- /usr/include/xcb/xproto.h:5106
   pragma Import (C, xcb_drawable_next, "xcb_drawable_next");

   function xcb_drawable_end (i : xcb_drawable_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5128
   pragma Import (C, xcb_drawable_end, "xcb_drawable_end");

   procedure xcb_fontable_next (i : access xcb_fontable_iterator_t);  -- /usr/include/xcb/xproto.h:5149
   pragma Import (C, xcb_fontable_next, "xcb_fontable_next");

   function xcb_fontable_end (i : xcb_fontable_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5171
   pragma Import (C, xcb_fontable_end, "xcb_fontable_end");

   procedure xcb_visualid_next (i : access xcb_visualid_iterator_t);  -- /usr/include/xcb/xproto.h:5192
   pragma Import (C, xcb_visualid_next, "xcb_visualid_next");

   function xcb_visualid_end (i : xcb_visualid_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5214
   pragma Import (C, xcb_visualid_end, "xcb_visualid_end");

   procedure xcb_timestamp_next (i : access xcb_timestamp_iterator_t);  -- /usr/include/xcb/xproto.h:5235
   pragma Import (C, xcb_timestamp_next, "xcb_timestamp_next");

   function xcb_timestamp_end (i : xcb_timestamp_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5257
   pragma Import (C, xcb_timestamp_end, "xcb_timestamp_end");

   procedure xcb_keysym_next (i : access xcb_keysym_iterator_t);  -- /usr/include/xcb/xproto.h:5278
   pragma Import (C, xcb_keysym_next, "xcb_keysym_next");

   function xcb_keysym_end (i : xcb_keysym_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5300
   pragma Import (C, xcb_keysym_end, "xcb_keysym_end");

   procedure xcb_keycode_next (i : access xcb_keycode_iterator_t);  -- /usr/include/xcb/xproto.h:5321
   pragma Import (C, xcb_keycode_next, "xcb_keycode_next");

   function xcb_keycode_end (i : xcb_keycode_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5343
   pragma Import (C, xcb_keycode_end, "xcb_keycode_end");

   procedure xcb_button_next (i : access xcb_button_iterator_t);  -- /usr/include/xcb/xproto.h:5364
   pragma Import (C, xcb_button_next, "xcb_button_next");

   function xcb_button_end (i : xcb_button_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5386
   pragma Import (C, xcb_button_end, "xcb_button_end");

   procedure xcb_point_next (i : access xcb_point_iterator_t);  -- /usr/include/xcb/xproto.h:5407
   pragma Import (C, xcb_point_next, "xcb_point_next");

   function xcb_point_end (i : xcb_point_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5429
   pragma Import (C, xcb_point_end, "xcb_point_end");

   procedure xcb_rectangle_next (i : access xcb_rectangle_iterator_t);  -- /usr/include/xcb/xproto.h:5450
   pragma Import (C, xcb_rectangle_next, "xcb_rectangle_next");

   function xcb_rectangle_end (i : xcb_rectangle_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5472
   pragma Import (C, xcb_rectangle_end, "xcb_rectangle_end");

   procedure xcb_arc_next (i : access xcb_arc_iterator_t);  -- /usr/include/xcb/xproto.h:5493
   pragma Import (C, xcb_arc_next, "xcb_arc_next");

   function xcb_arc_end (i : xcb_arc_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5515
   pragma Import (C, xcb_arc_end, "xcb_arc_end");

   procedure xcb_format_next (i : access xcb_format_iterator_t);  -- /usr/include/xcb/xproto.h:5536
   pragma Import (C, xcb_format_next, "xcb_format_next");

   function xcb_format_end (i : xcb_format_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5558
   pragma Import (C, xcb_format_end, "xcb_format_end");

   procedure xcb_visualtype_next (i : access xcb_visualtype_iterator_t);  -- /usr/include/xcb/xproto.h:5579
   pragma Import (C, xcb_visualtype_next, "xcb_visualtype_next");

   function xcb_visualtype_end (i : xcb_visualtype_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5601
   pragma Import (C, xcb_visualtype_end, "xcb_visualtype_end");

   function xcb_depth_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:5604
   pragma Import (C, xcb_depth_sizeof, "xcb_depth_sizeof");

   function xcb_depth_visuals (R : access xcb_depth_t) return access xcb_visualtype_t;  -- /usr/include/xcb/xproto.h:5617
   pragma Import (C, xcb_depth_visuals, "xcb_depth_visuals");

   function xcb_depth_visuals_length (R : access xcb_depth_t) return int;  -- /usr/include/xcb/xproto.h:5630
   pragma Import (C, xcb_depth_visuals_length, "xcb_depth_visuals_length");

   function xcb_depth_visuals_iterator (R : access xcb_depth_t) return xcb_visualtype_iterator_t;  -- /usr/include/xcb/xproto.h:5643
   pragma Import (C, xcb_depth_visuals_iterator, "xcb_depth_visuals_iterator");

   procedure xcb_depth_next (i : access xcb_depth_iterator_t);  -- /usr/include/xcb/xproto.h:5664
   pragma Import (C, xcb_depth_next, "xcb_depth_next");

   function xcb_depth_end (i : xcb_depth_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5686
   pragma Import (C, xcb_depth_end, "xcb_depth_end");

   function xcb_screen_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:5689
   pragma Import (C, xcb_screen_sizeof, "xcb_screen_sizeof");

   function xcb_screen_allowed_depths_length (R : access xcb_screen_t) return int;  -- /usr/include/xcb/xproto.h:5702
   pragma Import (C, xcb_screen_allowed_depths_length, "xcb_screen_allowed_depths_length");

   function xcb_screen_allowed_depths_iterator (R : access xcb_screen_t) return xcb_depth_iterator_t;  -- /usr/include/xcb/xproto.h:5715
   pragma Import (C, xcb_screen_allowed_depths_iterator, "xcb_screen_allowed_depths_iterator");

   procedure xcb_screen_next (i : access xcb_screen_iterator_t);  -- /usr/include/xcb/xproto.h:5736
   pragma Import (C, xcb_screen_next, "xcb_screen_next");

   function xcb_screen_end (i : xcb_screen_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5758
   pragma Import (C, xcb_screen_end, "xcb_screen_end");

   function xcb_setup_request_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:5761
   pragma Import (C, xcb_setup_request_sizeof, "xcb_setup_request_sizeof");

   function xcb_setup_request_authorization_protocol_name (R : access xcb_setup_request_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:5774
   pragma Import (C, xcb_setup_request_authorization_protocol_name, "xcb_setup_request_authorization_protocol_name");

   function xcb_setup_request_authorization_protocol_name_length (R : access xcb_setup_request_t) return int;  -- /usr/include/xcb/xproto.h:5787
   pragma Import (C, xcb_setup_request_authorization_protocol_name_length, "xcb_setup_request_authorization_protocol_name_length");

   function xcb_setup_request_authorization_protocol_name_end (R : access xcb_setup_request_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5800
   pragma Import (C, xcb_setup_request_authorization_protocol_name_end, "xcb_setup_request_authorization_protocol_name_end");

   function xcb_setup_request_authorization_protocol_data (R : access xcb_setup_request_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:5813
   pragma Import (C, xcb_setup_request_authorization_protocol_data, "xcb_setup_request_authorization_protocol_data");

   function xcb_setup_request_authorization_protocol_data_length (R : access xcb_setup_request_t) return int;  -- /usr/include/xcb/xproto.h:5826
   pragma Import (C, xcb_setup_request_authorization_protocol_data_length, "xcb_setup_request_authorization_protocol_data_length");

   function xcb_setup_request_authorization_protocol_data_end (R : access xcb_setup_request_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5839
   pragma Import (C, xcb_setup_request_authorization_protocol_data_end, "xcb_setup_request_authorization_protocol_data_end");

   procedure xcb_setup_request_next (i : access xcb_setup_request_iterator_t);  -- /usr/include/xcb/xproto.h:5860
   pragma Import (C, xcb_setup_request_next, "xcb_setup_request_next");

   function xcb_setup_request_end (i : xcb_setup_request_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5882
   pragma Import (C, xcb_setup_request_end, "xcb_setup_request_end");

   function xcb_setup_failed_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:5885
   pragma Import (C, xcb_setup_failed_sizeof, "xcb_setup_failed_sizeof");

   function xcb_setup_failed_reason (R : access xcb_setup_failed_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:5898
   pragma Import (C, xcb_setup_failed_reason, "xcb_setup_failed_reason");

   function xcb_setup_failed_reason_length (R : access xcb_setup_failed_t) return int;  -- /usr/include/xcb/xproto.h:5911
   pragma Import (C, xcb_setup_failed_reason_length, "xcb_setup_failed_reason_length");

   function xcb_setup_failed_reason_end (R : access xcb_setup_failed_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5924
   pragma Import (C, xcb_setup_failed_reason_end, "xcb_setup_failed_reason_end");

   procedure xcb_setup_failed_next (i : access xcb_setup_failed_iterator_t);  -- /usr/include/xcb/xproto.h:5945
   pragma Import (C, xcb_setup_failed_next, "xcb_setup_failed_next");

   function xcb_setup_failed_end (i : xcb_setup_failed_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:5967
   pragma Import (C, xcb_setup_failed_end, "xcb_setup_failed_end");

   function xcb_setup_authenticate_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:5970
   pragma Import (C, xcb_setup_authenticate_sizeof, "xcb_setup_authenticate_sizeof");

   function xcb_setup_authenticate_reason (R : access xcb_setup_authenticate_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:5983
   pragma Import (C, xcb_setup_authenticate_reason, "xcb_setup_authenticate_reason");

   function xcb_setup_authenticate_reason_length (R : access xcb_setup_authenticate_t) return int;  -- /usr/include/xcb/xproto.h:5996
   pragma Import (C, xcb_setup_authenticate_reason_length, "xcb_setup_authenticate_reason_length");

   function xcb_setup_authenticate_reason_end (R : access xcb_setup_authenticate_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:6009
   pragma Import (C, xcb_setup_authenticate_reason_end, "xcb_setup_authenticate_reason_end");

   procedure xcb_setup_authenticate_next (i : access xcb_setup_authenticate_iterator_t);  -- /usr/include/xcb/xproto.h:6030
   pragma Import (C, xcb_setup_authenticate_next, "xcb_setup_authenticate_next");

   function xcb_setup_authenticate_end (i : xcb_setup_authenticate_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:6052
   pragma Import (C, xcb_setup_authenticate_end, "xcb_setup_authenticate_end");

   function xcb_setup_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:6055
   pragma Import (C, xcb_setup_sizeof, "xcb_setup_sizeof");

   function xcb_setup_vendor (R : access xcb_setup_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:6068
   pragma Import (C, xcb_setup_vendor, "xcb_setup_vendor");

   function xcb_setup_vendor_length (R : access xcb_setup_t) return int;  -- /usr/include/xcb/xproto.h:6081
   pragma Import (C, xcb_setup_vendor_length, "xcb_setup_vendor_length");

   function xcb_setup_vendor_end (R : access xcb_setup_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:6094
   pragma Import (C, xcb_setup_vendor_end, "xcb_setup_vendor_end");

   function xcb_setup_pixmap_formats (R : access xcb_setup_t) return access xcb_format_t;  -- /usr/include/xcb/xproto.h:6107
   pragma Import (C, xcb_setup_pixmap_formats, "xcb_setup_pixmap_formats");

   function xcb_setup_pixmap_formats_length (R : access xcb_setup_t) return int;  -- /usr/include/xcb/xproto.h:6120
   pragma Import (C, xcb_setup_pixmap_formats_length, "xcb_setup_pixmap_formats_length");

   function xcb_setup_pixmap_formats_iterator (R : access xcb_setup_t) return xcb_format_iterator_t;  -- /usr/include/xcb/xproto.h:6133
   pragma Import (C, xcb_setup_pixmap_formats_iterator, "xcb_setup_pixmap_formats_iterator");

   function xcb_setup_roots_length (R : access xcb_setup_t) return int;  -- /usr/include/xcb/xproto.h:6146
   pragma Import (C, xcb_setup_roots_length, "xcb_setup_roots_length");

   function xcb_setup_roots_iterator (R : access xcb_setup_t) return xcb_screen_iterator_t;  -- /usr/include/xcb/xproto.h:6159
   pragma Import (C, xcb_setup_roots_iterator, "xcb_setup_roots_iterator");

   procedure xcb_setup_next (i : access xcb_setup_iterator_t);  -- /usr/include/xcb/xproto.h:6180
   pragma Import (C, xcb_setup_next, "xcb_setup_next");

   function xcb_setup_end (i : xcb_setup_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:6202
   pragma Import (C, xcb_setup_end, "xcb_setup_end");

   procedure xcb_client_message_data_next (i : access xcb_client_message_data_iterator_t);  -- /usr/include/xcb/xproto.h:6223
   pragma Import (C, xcb_client_message_data_next, "xcb_client_message_data_next");

   function xcb_client_message_data_end (i : xcb_client_message_data_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:6245
   pragma Import (C, xcb_client_message_data_end, "xcb_client_message_data_end");

   function xcb_create_window_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:6248
   pragma Import (C, xcb_create_window_sizeof, "xcb_create_window_sizeof");

   function xcb_create_window_checked
     (c : xcb_connection_t_access;
      depth : C99.Stdint.uint8_t;
      wid : xcb_window_t;
      parent : xcb_window_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      border_width : C99.Stdint.uint16_t;
      u_class : C99.Stdint.uint16_t;
      visual : xcb_visualid_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6319
   pragma Import (C, xcb_create_window_checked, "xcb_create_window_checked");

   function xcb_create_window
     (c : xcb_connection_t_access;
      depth : C99.Stdint.uint8_t;
      wid : xcb_window_t;
      parent : xcb_window_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      border_width : C99.Stdint.uint16_t;
      u_class : C99.Stdint.uint16_t;
      visual : xcb_visualid_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6399
   pragma Import (C, xcb_create_window, "xcb_create_window");

   function xcb_change_window_attributes_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:6414
   pragma Import (C, xcb_change_window_attributes_sizeof, "xcb_change_window_attributes_sizeof");

   function xcb_change_window_attributes_checked
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6448
   pragma Import (C, xcb_change_window_attributes_checked, "xcb_change_window_attributes_checked");

   function xcb_change_window_attributes
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6482
   pragma Import (C, xcb_change_window_attributes, "xcb_change_window_attributes");

   function xcb_get_window_attributes (c : xcb_connection_t_access; window : xcb_window_t) return xcb_get_window_attributes_cookie_t;  -- /usr/include/xcb/xproto.h:6509
   pragma Import (C, xcb_get_window_attributes, "xcb_get_window_attributes");

   function xcb_get_window_attributes_unchecked (c : xcb_connection_t_access; window : xcb_window_t) return xcb_get_window_attributes_cookie_t;  -- /usr/include/xcb/xproto.h:6537
   pragma Import (C, xcb_get_window_attributes_unchecked, "xcb_get_window_attributes_unchecked");

   function xcb_get_window_attributes_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_window_attributes_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_window_attributes_reply_t;  -- /usr/include/xcb/xproto.h:6567
   pragma Import (C, xcb_get_window_attributes_reply, "xcb_get_window_attributes_reply");

   function xcb_destroy_window_checked (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6601
   pragma Import (C, xcb_destroy_window_checked, "xcb_destroy_window_checked");

   function xcb_destroy_window (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6631
   pragma Import (C, xcb_destroy_window, "xcb_destroy_window");

   function xcb_destroy_subwindows_checked (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6657
   pragma Import (C, xcb_destroy_subwindows_checked, "xcb_destroy_subwindows_checked");

   function xcb_destroy_subwindows (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6680
   pragma Import (C, xcb_destroy_subwindows, "xcb_destroy_subwindows");

   function xcb_change_save_set_checked
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6714
   pragma Import (C, xcb_change_save_set_checked, "xcb_change_save_set_checked");

   function xcb_change_save_set
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6746
   pragma Import (C, xcb_change_save_set, "xcb_change_save_set");

   function xcb_reparent_window_checked
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      parent : xcb_window_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6786
   pragma Import (C, xcb_reparent_window_checked, "xcb_reparent_window_checked");

   function xcb_reparent_window
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      parent : xcb_window_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6825
   pragma Import (C, xcb_reparent_window, "xcb_reparent_window");

   function xcb_map_window_checked (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6874
   pragma Import (C, xcb_map_window_checked, "xcb_map_window_checked");

   function xcb_map_window (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6917
   pragma Import (C, xcb_map_window, "xcb_map_window");

   function xcb_map_subwindows_checked (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6943
   pragma Import (C, xcb_map_subwindows_checked, "xcb_map_subwindows_checked");

   function xcb_map_subwindows (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6966
   pragma Import (C, xcb_map_subwindows, "xcb_map_subwindows");

   function xcb_unmap_window_checked (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:6998
   pragma Import (C, xcb_unmap_window_checked, "xcb_unmap_window_checked");

   function xcb_unmap_window (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7027
   pragma Import (C, xcb_unmap_window, "xcb_unmap_window");

   function xcb_unmap_subwindows_checked (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7053
   pragma Import (C, xcb_unmap_subwindows_checked, "xcb_unmap_subwindows_checked");

   function xcb_unmap_subwindows (c : xcb_connection_t_access; window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7076
   pragma Import (C, xcb_unmap_subwindows, "xcb_unmap_subwindows");

   function xcb_configure_window_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:7080
   pragma Import (C, xcb_configure_window_sizeof, "xcb_configure_window_sizeof");

   function xcb_configure_window_checked
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      value_mask : C99.Stdint.uint16_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7112
   pragma Import (C, xcb_configure_window_checked, "xcb_configure_window_checked");

   function xcb_configure_window
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      value_mask : C99.Stdint.uint16_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7144
   pragma Import (C, xcb_configure_window, "xcb_configure_window");

   function xcb_circulate_window_checked
     (c : xcb_connection_t_access;
      direction : C99.Stdint.uint8_t;
      window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7181
   pragma Import (C, xcb_circulate_window_checked, "xcb_circulate_window_checked");

   function xcb_circulate_window
     (c : xcb_connection_t_access;
      direction : C99.Stdint.uint8_t;
      window : xcb_window_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7214
   pragma Import (C, xcb_circulate_window, "xcb_circulate_window");

   function xcb_get_geometry (c : xcb_connection_t_access; drawable : xcb_drawable_t) return xcb_get_geometry_cookie_t;  -- /usr/include/xcb/xproto.h:7240
   pragma Import (C, xcb_get_geometry, "xcb_get_geometry");

   function xcb_get_geometry_unchecked (c : xcb_connection_t_access; drawable : xcb_drawable_t) return xcb_get_geometry_cookie_t;  -- /usr/include/xcb/xproto.h:7268
   pragma Import (C, xcb_get_geometry_unchecked, "xcb_get_geometry_unchecked");

   function xcb_get_geometry_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_geometry_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_geometry_reply_t;  -- /usr/include/xcb/xproto.h:7298
   pragma Import (C, xcb_get_geometry_reply, "xcb_get_geometry_reply");

   function xcb_query_tree_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:7303
   pragma Import (C, xcb_query_tree_sizeof, "xcb_query_tree_sizeof");

   function xcb_query_tree (c : xcb_connection_t_access; window : xcb_window_t) return xcb_query_tree_cookie_t;  -- /usr/include/xcb/xproto.h:7328
   pragma Import (C, xcb_query_tree, "xcb_query_tree");

   function xcb_query_tree_unchecked (c : xcb_connection_t_access; window : xcb_window_t) return xcb_query_tree_cookie_t;  -- /usr/include/xcb/xproto.h:7357
   pragma Import (C, xcb_query_tree_unchecked, "xcb_query_tree_unchecked");

   function xcb_query_tree_children (R : access xcb_query_tree_reply_t) return access xcb_window_t;  -- /usr/include/xcb/xproto.h:7371
   pragma Import (C, xcb_query_tree_children, "xcb_query_tree_children");

   function xcb_query_tree_children_length (R : access xcb_query_tree_reply_t) return int;  -- /usr/include/xcb/xproto.h:7384
   pragma Import (C, xcb_query_tree_children_length, "xcb_query_tree_children_length");

   function xcb_query_tree_children_end (R : access xcb_query_tree_reply_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:7397
   pragma Import (C, xcb_query_tree_children_end, "xcb_query_tree_children_end");

   function xcb_query_tree_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_tree_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_tree_reply_t;  -- /usr/include/xcb/xproto.h:7426
   pragma Import (C, xcb_query_tree_reply, "xcb_query_tree_reply");

   function xcb_intern_atom_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:7431
   pragma Import (C, xcb_intern_atom_sizeof, "xcb_intern_atom_sizeof");

   function xcb_intern_atom
     (c : xcb_connection_t_access;
      only_if_exists : C99.Stdint.uint8_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_intern_atom_cookie_t;  -- /usr/include/xcb/xproto.h:7465
   pragma Import (C, xcb_intern_atom, "xcb_intern_atom");

   function xcb_intern_atom_unchecked
     (c : xcb_connection_t_access;
      only_if_exists : C99.Stdint.uint8_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_intern_atom_cookie_t;  -- /usr/include/xcb/xproto.h:7505
   pragma Import (C, xcb_intern_atom_unchecked, "xcb_intern_atom_unchecked");

   function xcb_intern_atom_reply
     (c : xcb_connection_t_access;
      cookie : xcb_intern_atom_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_intern_atom_reply_t;  -- /usr/include/xcb/xproto.h:7537
   pragma Import (C, xcb_intern_atom_reply, "xcb_intern_atom_reply");

   function xcb_get_atom_name_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:7542
   pragma Import (C, xcb_get_atom_name_sizeof, "xcb_get_atom_name_sizeof");

   function xcb_get_atom_name (c : xcb_connection_t_access; atom : xcb_atom_t) return xcb_get_atom_name_cookie_t;  -- /usr/include/xcb/xproto.h:7564
   pragma Import (C, xcb_get_atom_name, "xcb_get_atom_name");

   function xcb_get_atom_name_unchecked (c : xcb_connection_t_access; atom : xcb_atom_t) return xcb_get_atom_name_cookie_t;  -- /usr/include/xcb/xproto.h:7590
   pragma Import (C, xcb_get_atom_name_unchecked, "xcb_get_atom_name_unchecked");

   function xcb_get_atom_name_name (R : access xcb_get_atom_name_reply_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:7604
   pragma Import (C, xcb_get_atom_name_name, "xcb_get_atom_name_name");

   function xcb_get_atom_name_name_length (R : access xcb_get_atom_name_reply_t) return int;  -- /usr/include/xcb/xproto.h:7617
   pragma Import (C, xcb_get_atom_name_name_length, "xcb_get_atom_name_name_length");

   function xcb_get_atom_name_name_end (R : access xcb_get_atom_name_reply_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:7630
   pragma Import (C, xcb_get_atom_name_name_end, "xcb_get_atom_name_name_end");

   function xcb_get_atom_name_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_atom_name_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_atom_name_reply_t;  -- /usr/include/xcb/xproto.h:7659
   pragma Import (C, xcb_get_atom_name_reply, "xcb_get_atom_name_reply");

   function xcb_change_property_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:7664
   pragma Import (C, xcb_change_property_sizeof, "xcb_change_property_sizeof");

   function xcb_change_property_checked
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      window : xcb_window_t;
      property : xcb_atom_t;
      c_type : xcb_atom_t;
      format : C99.Stdint.uint8_t;
      data_len : C99.Stdint.uint32_t;
      data : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7709
   pragma Import (C, xcb_change_property_checked, "xcb_change_property_checked");

   function xcb_change_property
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      window : xcb_window_t;
      property : xcb_atom_t;
      c_type : xcb_atom_t;
      format : C99.Stdint.uint8_t;
      data_len : C99.Stdint.uint32_t;
      data : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7758
   pragma Import (C, xcb_change_property, "xcb_change_property");

   function xcb_delete_property_checked
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      property : xcb_atom_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7791
   pragma Import (C, xcb_delete_property_checked, "xcb_delete_property_checked");

   function xcb_delete_property
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      property : xcb_atom_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:7816
   pragma Import (C, xcb_delete_property, "xcb_delete_property");

   function xcb_get_property_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:7821
   pragma Import (C, xcb_get_property_sizeof, "xcb_get_property_sizeof");

   function xcb_get_property
     (c : xcb_connection_t_access;
      u_delete : C99.Stdint.uint8_t;
      window : xcb_window_t;
      property : xcb_atom_t;
      c_type : xcb_atom_t;
      long_offset : C99.Stdint.uint32_t;
      long_length : C99.Stdint.uint32_t) return xcb_get_property_cookie_t;  -- /usr/include/xcb/xproto.h:7867
   pragma Import (C, xcb_get_property, "xcb_get_property");

   function xcb_get_property_unchecked
     (c : xcb_connection_t_access;
      u_delete : C99.Stdint.uint8_t;
      window : xcb_window_t;
      property : xcb_atom_t;
      c_type : xcb_atom_t;
      long_offset : C99.Stdint.uint32_t;
      long_length : C99.Stdint.uint32_t) return xcb_get_property_cookie_t;  -- /usr/include/xcb/xproto.h:7922
   pragma Import (C, xcb_get_property_unchecked, "xcb_get_property_unchecked");

   function xcb_get_property_value (R : access xcb_get_property_reply_t) return System.Address;  -- /usr/include/xcb/xproto.h:7941
   pragma Import (C, xcb_get_property_value, "xcb_get_property_value");

   function xcb_get_property_value_length (R : access xcb_get_property_reply_t) return int;  -- /usr/include/xcb/xproto.h:7954
   pragma Import (C, xcb_get_property_value_length, "xcb_get_property_value_length");

   function xcb_get_property_value_end (R : access xcb_get_property_reply_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:7967
   pragma Import (C, xcb_get_property_value_end, "xcb_get_property_value_end");

   function xcb_get_property_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_property_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_property_reply_t;  -- /usr/include/xcb/xproto.h:7996
   pragma Import (C, xcb_get_property_reply, "xcb_get_property_reply");

   function xcb_list_properties_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:8001
   pragma Import (C, xcb_list_properties_sizeof, "xcb_list_properties_sizeof");

   function xcb_list_properties (c : xcb_connection_t_access; window : xcb_window_t) return xcb_list_properties_cookie_t;  -- /usr/include/xcb/xproto.h:8023
   pragma Import (C, xcb_list_properties, "xcb_list_properties");

   function xcb_list_properties_unchecked (c : xcb_connection_t_access; window : xcb_window_t) return xcb_list_properties_cookie_t;  -- /usr/include/xcb/xproto.h:8049
   pragma Import (C, xcb_list_properties_unchecked, "xcb_list_properties_unchecked");

   function xcb_list_properties_atoms (R : System.Address) return access xcb_atom_t;  -- /usr/include/xcb/xproto.h:8063
   pragma Import (C, xcb_list_properties_atoms, "xcb_list_properties_atoms");

   function xcb_list_properties_atoms_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:8076
   pragma Import (C, xcb_list_properties_atoms_length, "xcb_list_properties_atoms_length");

   function xcb_list_properties_atoms_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:8089
   pragma Import (C, xcb_list_properties_atoms_end, "xcb_list_properties_atoms_end");

   function xcb_list_properties_reply
     (c : xcb_connection_t_access;
      cookie : xcb_list_properties_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_list_properties_reply_t;  -- /usr/include/xcb/xproto.h:8118
   pragma Import (C, xcb_list_properties_reply, "xcb_list_properties_reply");

   function xcb_set_selection_owner_checked
     (c : xcb_connection_t_access;
      owner : xcb_window_t;
      selection : xcb_atom_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8163
   pragma Import (C, xcb_set_selection_owner_checked, "xcb_set_selection_owner_checked");

   function xcb_set_selection_owner
     (c : xcb_connection_t_access;
      owner : xcb_window_t;
      selection : xcb_atom_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8206
   pragma Import (C, xcb_set_selection_owner, "xcb_set_selection_owner");

   function xcb_get_selection_owner (c : xcb_connection_t_access; selection : xcb_atom_t) return xcb_get_selection_owner_cookie_t;  -- /usr/include/xcb/xproto.h:8235
   pragma Import (C, xcb_get_selection_owner, "xcb_get_selection_owner");

   function xcb_get_selection_owner_unchecked (c : xcb_connection_t_access; selection : xcb_atom_t) return xcb_get_selection_owner_cookie_t;  -- /usr/include/xcb/xproto.h:8265
   pragma Import (C, xcb_get_selection_owner_unchecked, "xcb_get_selection_owner_unchecked");

   function xcb_get_selection_owner_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_selection_owner_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_selection_owner_reply_t;  -- /usr/include/xcb/xproto.h:8295
   pragma Import (C, xcb_get_selection_owner_reply, "xcb_get_selection_owner_reply");

   function xcb_convert_selection_checked
     (c : xcb_connection_t_access;
      requestor : xcb_window_t;
      selection : xcb_atom_t;
      target : xcb_atom_t;
      property : xcb_atom_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8326
   pragma Import (C, xcb_convert_selection_checked, "xcb_convert_selection_checked");

   function xcb_convert_selection
     (c : xcb_connection_t_access;
      requestor : xcb_window_t;
      selection : xcb_atom_t;
      target : xcb_atom_t;
      property : xcb_atom_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8357
   pragma Import (C, xcb_convert_selection, "xcb_convert_selection");

   function xcb_send_event_checked
     (c : xcb_connection_t_access;
      propagate : C99.Stdint.uint8_t;
      destination : xcb_window_t;
      event_mask : C99.Stdint.uint32_t;
      event : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8416
   pragma Import (C, xcb_send_event_checked, "xcb_send_event_checked");

   function xcb_send_event
     (c : xcb_connection_t_access;
      propagate : C99.Stdint.uint8_t;
      destination : xcb_window_t;
      event_mask : C99.Stdint.uint32_t;
      event : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8471
   pragma Import (C, xcb_send_event, "xcb_send_event");

   function xcb_grab_pointer
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      event_mask : C99.Stdint.uint16_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t;
      confine_to : xcb_window_t;
      cursor : xcb_cursor_t;
      time : xcb_timestamp_t) return xcb_grab_pointer_cookie_t;  -- /usr/include/xcb/xproto.h:8532
   pragma Import (C, xcb_grab_pointer, "xcb_grab_pointer");

   function xcb_grab_pointer_unchecked
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      event_mask : C99.Stdint.uint16_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t;
      confine_to : xcb_window_t;
      cursor : xcb_cursor_t;
      time : xcb_timestamp_t) return xcb_grab_pointer_cookie_t;  -- /usr/include/xcb/xproto.h:8600
   pragma Import (C, xcb_grab_pointer_unchecked, "xcb_grab_pointer_unchecked");

   function xcb_grab_pointer_reply
     (c : xcb_connection_t_access;
      cookie : xcb_grab_pointer_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_grab_pointer_reply_t;  -- /usr/include/xcb/xproto.h:8637
   pragma Import (C, xcb_grab_pointer_reply, "xcb_grab_pointer_reply");

   function xcb_ungrab_pointer_checked (c : xcb_connection_t_access; time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8673
   pragma Import (C, xcb_ungrab_pointer_checked, "xcb_ungrab_pointer_checked");

   function xcb_ungrab_pointer (c : xcb_connection_t_access; time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8705
   pragma Import (C, xcb_ungrab_pointer, "xcb_ungrab_pointer");

   function xcb_grab_button_checked
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      event_mask : C99.Stdint.uint16_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t;
      confine_to : xcb_window_t;
      cursor : xcb_cursor_t;
      button : C99.Stdint.uint8_t;
      modifiers : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8796
   pragma Import (C, xcb_grab_button_checked, "xcb_grab_button_checked");

   function xcb_grab_button
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      event_mask : C99.Stdint.uint16_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t;
      confine_to : xcb_window_t;
      cursor : xcb_cursor_t;
      button : C99.Stdint.uint8_t;
      modifiers : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8892
   pragma Import (C, xcb_grab_button, "xcb_grab_button");

   function xcb_ungrab_button_checked
     (c : xcb_connection_t_access;
      button : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      modifiers : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8928
   pragma Import (C, xcb_ungrab_button_checked, "xcb_ungrab_button_checked");

   function xcb_ungrab_button
     (c : xcb_connection_t_access;
      button : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      modifiers : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8955
   pragma Import (C, xcb_ungrab_button, "xcb_ungrab_button");

   function xcb_change_active_pointer_grab_checked
     (c : xcb_connection_t_access;
      cursor : xcb_cursor_t;
      time : xcb_timestamp_t;
      event_mask : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:8985
   pragma Import (C, xcb_change_active_pointer_grab_checked, "xcb_change_active_pointer_grab_checked");

   function xcb_change_active_pointer_grab
     (c : xcb_connection_t_access;
      cursor : xcb_cursor_t;
      time : xcb_timestamp_t;
      event_mask : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9012
   pragma Import (C, xcb_change_active_pointer_grab, "xcb_change_active_pointer_grab");

   function xcb_grab_keyboard
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      time : xcb_timestamp_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t) return xcb_grab_keyboard_cookie_t;  -- /usr/include/xcb/xproto.h:9062
   pragma Import (C, xcb_grab_keyboard, "xcb_grab_keyboard");

   function xcb_grab_keyboard_unchecked
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      time : xcb_timestamp_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t) return xcb_grab_keyboard_cookie_t;  -- /usr/include/xcb/xproto.h:9117
   pragma Import (C, xcb_grab_keyboard_unchecked, "xcb_grab_keyboard_unchecked");

   function xcb_grab_keyboard_reply
     (c : xcb_connection_t_access;
      cookie : xcb_grab_keyboard_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_grab_keyboard_reply_t;  -- /usr/include/xcb/xproto.h:9151
   pragma Import (C, xcb_grab_keyboard_reply, "xcb_grab_keyboard_reply");

   function xcb_ungrab_keyboard_checked (c : xcb_connection_t_access; time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9178
   pragma Import (C, xcb_ungrab_keyboard_checked, "xcb_ungrab_keyboard_checked");

   function xcb_ungrab_keyboard (c : xcb_connection_t_access; time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9201
   pragma Import (C, xcb_ungrab_keyboard, "xcb_ungrab_keyboard");

   function xcb_grab_key_checked
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      modifiers : C99.Stdint.uint16_t;
      key : xcb_keycode_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9276
   pragma Import (C, xcb_grab_key_checked, "xcb_grab_key_checked");

   function xcb_grab_key
     (c : xcb_connection_t_access;
      owner_events : C99.Stdint.uint8_t;
      grab_window : xcb_window_t;
      modifiers : C99.Stdint.uint16_t;
      key : xcb_keycode_t;
      pointer_mode : C99.Stdint.uint8_t;
      keyboard_mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9353
   pragma Import (C, xcb_grab_key, "xcb_grab_key");

   function xcb_ungrab_key_checked
     (c : xcb_connection_t_access;
      key : xcb_keycode_t;
      grab_window : xcb_window_t;
      modifiers : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9396
   pragma Import (C, xcb_ungrab_key_checked, "xcb_ungrab_key_checked");

   function xcb_ungrab_key
     (c : xcb_connection_t_access;
      key : xcb_keycode_t;
      grab_window : xcb_window_t;
      modifiers : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9433
   pragma Import (C, xcb_ungrab_key, "xcb_ungrab_key");

   function xcb_allow_events_checked
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9472
   pragma Import (C, xcb_allow_events_checked, "xcb_allow_events_checked");

   function xcb_allow_events
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9507
   pragma Import (C, xcb_allow_events, "xcb_allow_events");

   function xcb_grab_server_checked (c : xcb_connection_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9533
   pragma Import (C, xcb_grab_server_checked, "xcb_grab_server_checked");

   function xcb_grab_server (c : xcb_connection_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9554
   pragma Import (C, xcb_grab_server, "xcb_grab_server");

   function xcb_ungrab_server_checked (c : xcb_connection_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9578
   pragma Import (C, xcb_ungrab_server_checked, "xcb_ungrab_server_checked");

   function xcb_ungrab_server (c : xcb_connection_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:9599
   pragma Import (C, xcb_ungrab_server, "xcb_ungrab_server");

   function xcb_query_pointer (c : xcb_connection_t_access; window : xcb_window_t) return xcb_query_pointer_cookie_t;  -- /usr/include/xcb/xproto.h:9625
   pragma Import (C, xcb_query_pointer, "xcb_query_pointer");

   function xcb_query_pointer_unchecked (c : xcb_connection_t_access; window : xcb_window_t) return xcb_query_pointer_cookie_t;  -- /usr/include/xcb/xproto.h:9655
   pragma Import (C, xcb_query_pointer_unchecked, "xcb_query_pointer_unchecked");

   function xcb_query_pointer_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_pointer_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_pointer_reply_t;  -- /usr/include/xcb/xproto.h:9685
   pragma Import (C, xcb_query_pointer_reply, "xcb_query_pointer_reply");

   procedure xcb_timecoord_next (i : access xcb_timecoord_iterator_t);  -- /usr/include/xcb/xproto.h:9708
   pragma Import (C, xcb_timecoord_next, "xcb_timecoord_next");

   function xcb_timecoord_end (i : xcb_timecoord_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:9730
   pragma Import (C, xcb_timecoord_end, "xcb_timecoord_end");

   function xcb_get_motion_events_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:9733
   pragma Import (C, xcb_get_motion_events_sizeof, "xcb_get_motion_events_sizeof");

   function xcb_get_motion_events
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      start : xcb_timestamp_t;
      stop : xcb_timestamp_t) return xcb_get_motion_events_cookie_t;  -- /usr/include/xcb/xproto.h:9757
   pragma Import (C, xcb_get_motion_events, "xcb_get_motion_events");

   function xcb_get_motion_events_unchecked
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      start : xcb_timestamp_t;
      stop : xcb_timestamp_t) return xcb_get_motion_events_cookie_t;  -- /usr/include/xcb/xproto.h:9787
   pragma Import (C, xcb_get_motion_events_unchecked, "xcb_get_motion_events_unchecked");

   function xcb_get_motion_events_events (R : access xcb_get_motion_events_reply_t) return access xcb_timecoord_t;  -- /usr/include/xcb/xproto.h:9803
   pragma Import (C, xcb_get_motion_events_events, "xcb_get_motion_events_events");

   function xcb_get_motion_events_events_length (R : access xcb_get_motion_events_reply_t) return int;  -- /usr/include/xcb/xproto.h:9816
   pragma Import (C, xcb_get_motion_events_events_length, "xcb_get_motion_events_events_length");

   function xcb_get_motion_events_events_iterator (R : access xcb_get_motion_events_reply_t) return xcb_timecoord_iterator_t;  -- /usr/include/xcb/xproto.h:9829
   pragma Import (C, xcb_get_motion_events_events_iterator, "xcb_get_motion_events_events_iterator");

   function xcb_get_motion_events_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_motion_events_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_motion_events_reply_t;  -- /usr/include/xcb/xproto.h:9858
   pragma Import (C, xcb_get_motion_events_reply, "xcb_get_motion_events_reply");

   function xcb_translate_coordinates
     (c : xcb_connection_t_access;
      src_window : xcb_window_t;
      dst_window : xcb_window_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t) return xcb_translate_coordinates_cookie_t;  -- /usr/include/xcb/xproto.h:9885
   pragma Import (C, xcb_translate_coordinates, "xcb_translate_coordinates");

   function xcb_translate_coordinates_unchecked
     (c : xcb_connection_t_access;
      src_window : xcb_window_t;
      dst_window : xcb_window_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t) return xcb_translate_coordinates_cookie_t;  -- /usr/include/xcb/xproto.h:9917
   pragma Import (C, xcb_translate_coordinates_unchecked, "xcb_translate_coordinates_unchecked");

   function xcb_translate_coordinates_reply
     (c : xcb_connection_t_access;
      cookie : xcb_translate_coordinates_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_translate_coordinates_reply_t;  -- /usr/include/xcb/xproto.h:9950
   pragma Import (C, xcb_translate_coordinates_reply, "xcb_translate_coordinates_reply");

   function xcb_warp_pointer_checked
     (c : xcb_connection_t_access;
      src_window : xcb_window_t;
      dst_window : xcb_window_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t;
      src_width : C99.Stdint.uint16_t;
      src_height : C99.Stdint.uint16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10003
   pragma Import (C, xcb_warp_pointer_checked, "xcb_warp_pointer_checked");

   function xcb_warp_pointer
     (c : xcb_connection_t_access;
      src_window : xcb_window_t;
      dst_window : xcb_window_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t;
      src_width : C99.Stdint.uint16_t;
      src_height : C99.Stdint.uint16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10059
   pragma Import (C, xcb_warp_pointer, "xcb_warp_pointer");

   function xcb_set_input_focus_checked
     (c : xcb_connection_t_access;
      revert_to : C99.Stdint.uint8_t;
      focus : xcb_window_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10114
   pragma Import (C, xcb_set_input_focus_checked, "xcb_set_input_focus_checked");

   function xcb_set_input_focus
     (c : xcb_connection_t_access;
      revert_to : C99.Stdint.uint8_t;
      focus : xcb_window_t;
      time : xcb_timestamp_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10161
   pragma Import (C, xcb_set_input_focus, "xcb_set_input_focus");

   function xcb_get_input_focus (c : xcb_connection_t) return xcb_get_input_focus_cookie_t;  -- /usr/include/xcb/xproto.h:10185
   pragma Import (C, xcb_get_input_focus, "xcb_get_input_focus");

   function xcb_get_input_focus_unchecked (c : xcb_connection_t) return xcb_get_input_focus_cookie_t;  -- /usr/include/xcb/xproto.h:10209
   pragma Import (C, xcb_get_input_focus_unchecked, "xcb_get_input_focus_unchecked");

   function xcb_get_input_focus_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_input_focus_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_input_focus_reply_t;  -- /usr/include/xcb/xproto.h:10238
   pragma Import (C, xcb_get_input_focus_reply, "xcb_get_input_focus_reply");

   function xcb_query_keymap (c : xcb_connection_t) return xcb_query_keymap_cookie_t;  -- /usr/include/xcb/xproto.h:10261
   pragma Import (C, xcb_query_keymap, "xcb_query_keymap");

   function xcb_query_keymap_unchecked (c : xcb_connection_t) return xcb_query_keymap_cookie_t;  -- /usr/include/xcb/xproto.h:10285
   pragma Import (C, xcb_query_keymap_unchecked, "xcb_query_keymap_unchecked");

   function xcb_query_keymap_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_keymap_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_keymap_reply_t;  -- /usr/include/xcb/xproto.h:10314
   pragma Import (C, xcb_query_keymap_reply, "xcb_query_keymap_reply");

   function xcb_open_font_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:10319
   pragma Import (C, xcb_open_font_sizeof, "xcb_open_font_sizeof");

   function xcb_open_font_checked
     (c : xcb_connection_t_access;
      fid : xcb_font_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10353
   pragma Import (C, xcb_open_font_checked, "xcb_open_font_checked");

   function xcb_open_font
     (c : xcb_connection_t_access;
      fid : xcb_font_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10387
   pragma Import (C, xcb_open_font, "xcb_open_font");

   function xcb_close_font_checked (c : xcb_connection_t_access; font : xcb_font_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10415
   pragma Import (C, xcb_close_font_checked, "xcb_close_font_checked");

   function xcb_close_font (c : xcb_connection_t_access; font : xcb_font_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:10438
   pragma Import (C, xcb_close_font, "xcb_close_font");

   procedure xcb_fontprop_next (i : access xcb_fontprop_iterator_t);  -- /usr/include/xcb/xproto.h:10460
   pragma Import (C, xcb_fontprop_next, "xcb_fontprop_next");

   function xcb_fontprop_end (i : xcb_fontprop_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:10482
   pragma Import (C, xcb_fontprop_end, "xcb_fontprop_end");

   procedure xcb_charinfo_next (i : access xcb_charinfo_iterator_t);  -- /usr/include/xcb/xproto.h:10503
   pragma Import (C, xcb_charinfo_next, "xcb_charinfo_next");

   function xcb_charinfo_end (i : xcb_charinfo_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:10525
   pragma Import (C, xcb_charinfo_end, "xcb_charinfo_end");

   function xcb_query_font_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:10528
   pragma Import (C, xcb_query_font_sizeof, "xcb_query_font_sizeof");

   function xcb_query_font (c : xcb_connection_t_access; font : xcb_fontable_t) return xcb_query_font_cookie_t;  -- /usr/include/xcb/xproto.h:10552
   pragma Import (C, xcb_query_font, "xcb_query_font");

   function xcb_query_font_unchecked (c : xcb_connection_t_access; font : xcb_fontable_t) return xcb_query_font_cookie_t;  -- /usr/include/xcb/xproto.h:10580
   pragma Import (C, xcb_query_font_unchecked, "xcb_query_font_unchecked");

   function xcb_query_font_properties (R : access xcb_query_font_reply_t) return access xcb_fontprop_t;  -- /usr/include/xcb/xproto.h:10594
   pragma Import (C, xcb_query_font_properties, "xcb_query_font_properties");

   function xcb_query_font_properties_length (R : access xcb_query_font_reply_t) return int;  -- /usr/include/xcb/xproto.h:10607
   pragma Import (C, xcb_query_font_properties_length, "xcb_query_font_properties_length");

   function xcb_query_font_properties_iterator (R : access xcb_query_font_reply_t) return xcb_fontprop_iterator_t;  -- /usr/include/xcb/xproto.h:10620
   pragma Import (C, xcb_query_font_properties_iterator, "xcb_query_font_properties_iterator");

   function xcb_query_font_char_infos (R : access xcb_query_font_reply_t) return access xcb_charinfo_t;  -- /usr/include/xcb/xproto.h:10633
   pragma Import (C, xcb_query_font_char_infos, "xcb_query_font_char_infos");

   function xcb_query_font_char_infos_length (R : access xcb_query_font_reply_t) return int;  -- /usr/include/xcb/xproto.h:10646
   pragma Import (C, xcb_query_font_char_infos_length, "xcb_query_font_char_infos_length");

   function xcb_query_font_char_infos_iterator (R : access xcb_query_font_reply_t) return xcb_charinfo_iterator_t;  -- /usr/include/xcb/xproto.h:10659
   pragma Import (C, xcb_query_font_char_infos_iterator, "xcb_query_font_char_infos_iterator");

   function xcb_query_font_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_font_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_font_reply_t;  -- /usr/include/xcb/xproto.h:10688
   pragma Import (C, xcb_query_font_reply, "xcb_query_font_reply");

   function xcb_query_text_extents_sizeof (u_buffer : System.Address; string_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:10693
   pragma Import (C, xcb_query_text_extents_sizeof, "xcb_query_text_extents_sizeof");

   function xcb_query_text_extents
     (c : xcb_connection_t_access;
      font : xcb_fontable_t;
      string_len : C99.Stdint.uint32_t;
      string : System.Address) return xcb_query_text_extents_cookie_t;  -- /usr/include/xcb/xproto.h:10742
   pragma Import (C, xcb_query_text_extents, "xcb_query_text_extents");

   function xcb_query_text_extents_unchecked
     (c : xcb_connection_t_access;
      font : xcb_fontable_t;
      string_len : C99.Stdint.uint32_t;
      string : System.Address) return xcb_query_text_extents_cookie_t;  -- /usr/include/xcb/xproto.h:10796
   pragma Import (C, xcb_query_text_extents_unchecked, "xcb_query_text_extents_unchecked");

   function xcb_query_text_extents_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_text_extents_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_text_extents_reply_t;  -- /usr/include/xcb/xproto.h:10828
   pragma Import (C, xcb_query_text_extents_reply, "xcb_query_text_extents_reply");

   function xcb_str_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:10833
   pragma Import (C, xcb_str_sizeof, "xcb_str_sizeof");

   function xcb_str_name (R : access xcb_str_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:10846
   pragma Import (C, xcb_str_name, "xcb_str_name");

   function xcb_str_name_length (R : access xcb_str_t) return int;  -- /usr/include/xcb/xproto.h:10859
   pragma Import (C, xcb_str_name_length, "xcb_str_name_length");

   function xcb_str_name_end (R : access xcb_str_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:10872
   pragma Import (C, xcb_str_name_end, "xcb_str_name_end");

   procedure xcb_str_next (i : access xcb_str_iterator_t);  -- /usr/include/xcb/xproto.h:10893
   pragma Import (C, xcb_str_next, "xcb_str_next");

   function xcb_str_end (i : xcb_str_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:10915
   pragma Import (C, xcb_str_end, "xcb_str_end");

   function xcb_list_fonts_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:10918
   pragma Import (C, xcb_list_fonts_sizeof, "xcb_list_fonts_sizeof");

   function xcb_list_fonts
     (c : xcb_connection_t_access;
      max_names : C99.Stdint.uint16_t;
      pattern_len : C99.Stdint.uint16_t;
      pattern : Interfaces.C.Strings.chars_ptr) return xcb_list_fonts_cookie_t;  -- /usr/include/xcb/xproto.h:10950
   pragma Import (C, xcb_list_fonts, "xcb_list_fonts");

   function xcb_list_fonts_unchecked
     (c : xcb_connection_t_access;
      max_names : C99.Stdint.uint16_t;
      pattern_len : C99.Stdint.uint16_t;
      pattern : Interfaces.C.Strings.chars_ptr) return xcb_list_fonts_cookie_t;  -- /usr/include/xcb/xproto.h:10988
   pragma Import (C, xcb_list_fonts_unchecked, "xcb_list_fonts_unchecked");

   function xcb_list_fonts_names_length (R : access xcb_list_fonts_reply_t) return int;  -- /usr/include/xcb/xproto.h:11004
   pragma Import (C, xcb_list_fonts_names_length, "xcb_list_fonts_names_length");

   function xcb_list_fonts_names_iterator (R : access xcb_list_fonts_reply_t) return xcb_str_iterator_t;  -- /usr/include/xcb/xproto.h:11017
   pragma Import (C, xcb_list_fonts_names_iterator, "xcb_list_fonts_names_iterator");

   function xcb_list_fonts_reply
     (c : xcb_connection_t_access;
      cookie : xcb_list_fonts_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_list_fonts_reply_t;  -- /usr/include/xcb/xproto.h:11046
   pragma Import (C, xcb_list_fonts_reply, "xcb_list_fonts_reply");

   function xcb_list_fonts_with_info_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:11051
   pragma Import (C, xcb_list_fonts_with_info_sizeof, "xcb_list_fonts_with_info_sizeof");

   function xcb_list_fonts_with_info
     (c : xcb_connection_t_access;
      max_names : C99.Stdint.uint16_t;
      pattern_len : C99.Stdint.uint16_t;
      pattern : Interfaces.C.Strings.chars_ptr) return xcb_list_fonts_with_info_cookie_t;  -- /usr/include/xcb/xproto.h:11083
   pragma Import (C, xcb_list_fonts_with_info, "xcb_list_fonts_with_info");

   function xcb_list_fonts_with_info_unchecked
     (c : xcb_connection_t_access;
      max_names : C99.Stdint.uint16_t;
      pattern_len : C99.Stdint.uint16_t;
      pattern : Interfaces.C.Strings.chars_ptr) return xcb_list_fonts_with_info_cookie_t;  -- /usr/include/xcb/xproto.h:11121
   pragma Import (C, xcb_list_fonts_with_info_unchecked, "xcb_list_fonts_with_info_unchecked");

   function xcb_list_fonts_with_info_properties (R : access xcb_list_fonts_with_info_reply_t) return access xcb_fontprop_t;  -- /usr/include/xcb/xproto.h:11137
   pragma Import (C, xcb_list_fonts_with_info_properties, "xcb_list_fonts_with_info_properties");

   function xcb_list_fonts_with_info_properties_length (R : access xcb_list_fonts_with_info_reply_t) return int;  -- /usr/include/xcb/xproto.h:11150
   pragma Import (C, xcb_list_fonts_with_info_properties_length, "xcb_list_fonts_with_info_properties_length");

   function xcb_list_fonts_with_info_properties_iterator (R : access xcb_list_fonts_with_info_reply_t) return xcb_fontprop_iterator_t;  -- /usr/include/xcb/xproto.h:11163
   pragma Import (C, xcb_list_fonts_with_info_properties_iterator, "xcb_list_fonts_with_info_properties_iterator");

   function xcb_list_fonts_with_info_name (R : access xcb_list_fonts_with_info_reply_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/xcb/xproto.h:11176
   pragma Import (C, xcb_list_fonts_with_info_name, "xcb_list_fonts_with_info_name");

   function xcb_list_fonts_with_info_name_length (R : access xcb_list_fonts_with_info_reply_t) return int;  -- /usr/include/xcb/xproto.h:11189
   pragma Import (C, xcb_list_fonts_with_info_name_length, "xcb_list_fonts_with_info_name_length");

   function xcb_list_fonts_with_info_name_end (R : access xcb_list_fonts_with_info_reply_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:11202
   pragma Import (C, xcb_list_fonts_with_info_name_end, "xcb_list_fonts_with_info_name_end");

   function xcb_list_fonts_with_info_reply
     (c : xcb_connection_t_access;
      cookie : xcb_list_fonts_with_info_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_list_fonts_with_info_reply_t;  -- /usr/include/xcb/xproto.h:11231
   pragma Import (C, xcb_list_fonts_with_info_reply, "xcb_list_fonts_with_info_reply");

   function xcb_set_font_path_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:11236
   pragma Import (C, xcb_set_font_path_sizeof, "xcb_set_font_path_sizeof");

   function xcb_set_font_path_checked
     (c : xcb_connection_t_access;
      font_qty : C99.Stdint.uint16_t;
      font : access xcb_str_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11262
   pragma Import (C, xcb_set_font_path_checked, "xcb_set_font_path_checked");

   function xcb_set_font_path
     (c : xcb_connection_t_access;
      font_qty : C99.Stdint.uint16_t;
      font : access xcb_str_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11287
   pragma Import (C, xcb_set_font_path, "xcb_set_font_path");

   function xcb_get_font_path_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:11292
   pragma Import (C, xcb_get_font_path_sizeof, "xcb_get_font_path_sizeof");

   function xcb_get_font_path (c : xcb_connection_t) return xcb_get_font_path_cookie_t;  -- /usr/include/xcb/xproto.h:11313
   pragma Import (C, xcb_get_font_path, "xcb_get_font_path");

   function xcb_get_font_path_unchecked (c : xcb_connection_t) return xcb_get_font_path_cookie_t;  -- /usr/include/xcb/xproto.h:11337
   pragma Import (C, xcb_get_font_path_unchecked, "xcb_get_font_path_unchecked");

   function xcb_get_font_path_path_length (R : access xcb_get_font_path_reply_t) return int;  -- /usr/include/xcb/xproto.h:11350
   pragma Import (C, xcb_get_font_path_path_length, "xcb_get_font_path_path_length");

   function xcb_get_font_path_path_iterator (R : access xcb_get_font_path_reply_t) return xcb_str_iterator_t;  -- /usr/include/xcb/xproto.h:11363
   pragma Import (C, xcb_get_font_path_path_iterator, "xcb_get_font_path_path_iterator");

   function xcb_get_font_path_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_font_path_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_font_path_reply_t;  -- /usr/include/xcb/xproto.h:11392
   pragma Import (C, xcb_get_font_path_reply, "xcb_get_font_path_reply");

   function xcb_create_pixmap_checked
     (c : xcb_connection_t_access;
      depth : C99.Stdint.uint8_t;
      pid : xcb_pixmap_t;
      drawable : xcb_drawable_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11431
   pragma Import (C, xcb_create_pixmap_checked, "xcb_create_pixmap_checked");

   function xcb_create_pixmap
     (c : xcb_connection_t_access;
      depth : C99.Stdint.uint8_t;
      pid : xcb_pixmap_t;
      drawable : xcb_drawable_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11470
   pragma Import (C, xcb_create_pixmap, "xcb_create_pixmap");

   function xcb_free_pixmap_checked (c : xcb_connection_t_access; pixmap : xcb_pixmap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11503
   pragma Import (C, xcb_free_pixmap_checked, "xcb_free_pixmap_checked");

   function xcb_free_pixmap (c : xcb_connection_t_access; pixmap : xcb_pixmap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11529
   pragma Import (C, xcb_free_pixmap, "xcb_free_pixmap");

   function xcb_create_gc_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:11533
   pragma Import (C, xcb_create_gc_sizeof, "xcb_create_gc_sizeof");

   function xcb_create_gc_checked
     (c : xcb_connection_t_access;
      cid : xcb_gcontext_t;
      drawable : xcb_drawable_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11566
   pragma Import (C, xcb_create_gc_checked, "xcb_create_gc_checked");

   function xcb_create_gc
     (c : xcb_connection_t_access;
      cid : xcb_gcontext_t;
      drawable : xcb_drawable_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11600
   pragma Import (C, xcb_create_gc, "xcb_create_gc");

   function xcb_change_gc_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:11607
   pragma Import (C, xcb_change_gc_sizeof, "xcb_change_gc_sizeof");

   function xcb_change_gc_checked
     (c : xcb_connection_t_access;
      gc : xcb_gcontext_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11641
   pragma Import (C, xcb_change_gc_checked, "xcb_change_gc_checked");

   function xcb_change_gc
     (c : xcb_connection_t_access;
      gc : xcb_gcontext_t;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11675
   pragma Import (C, xcb_change_gc, "xcb_change_gc");

   function xcb_copy_gc_checked
     (c : xcb_connection_t_access;
      src_gc : xcb_gcontext_t;
      dst_gc : xcb_gcontext_t;
      value_mask : C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11705
   pragma Import (C, xcb_copy_gc_checked, "xcb_copy_gc_checked");

   function xcb_copy_gc
     (c : xcb_connection_t_access;
      src_gc : xcb_gcontext_t;
      dst_gc : xcb_gcontext_t;
      value_mask : C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11732
   pragma Import (C, xcb_copy_gc, "xcb_copy_gc");

   function xcb_set_dashes_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:11738
   pragma Import (C, xcb_set_dashes_sizeof, "xcb_set_dashes_sizeof");

   function xcb_set_dashes_checked
     (c : xcb_connection_t_access;
      gc : xcb_gcontext_t;
      dash_offset : C99.Stdint.uint16_t;
      dashes_len : C99.Stdint.uint16_t;
      dashes : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11766
   pragma Import (C, xcb_set_dashes_checked, "xcb_set_dashes_checked");

   function xcb_set_dashes
     (c : xcb_connection_t_access;
      gc : xcb_gcontext_t;
      dash_offset : C99.Stdint.uint16_t;
      dashes_len : C99.Stdint.uint16_t;
      dashes : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11795
   pragma Import (C, xcb_set_dashes, "xcb_set_dashes");

   function xcb_set_clip_rectangles_sizeof (u_buffer : System.Address; rectangles_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:11802
   pragma Import (C, xcb_set_clip_rectangles_sizeof, "xcb_set_clip_rectangles_sizeof");

   function xcb_set_clip_rectangles_checked
     (c : xcb_connection_t_access;
      ordering : C99.Stdint.uint8_t;
      gc : xcb_gcontext_t;
      clip_x_origin : C99.Stdint.int16_t;
      clip_y_origin : C99.Stdint.int16_t;
      rectangles_len : C99.Stdint.uint32_t;
      rectangles : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11833
   pragma Import (C, xcb_set_clip_rectangles_checked, "xcb_set_clip_rectangles_checked");

   function xcb_set_clip_rectangles
     (c : xcb_connection_t_access;
      ordering : C99.Stdint.uint8_t;
      gc : xcb_gcontext_t;
      clip_x_origin : C99.Stdint.int16_t;
      clip_y_origin : C99.Stdint.int16_t;
      rectangles_len : C99.Stdint.uint32_t;
      rectangles : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11866
   pragma Import (C, xcb_set_clip_rectangles, "xcb_set_clip_rectangles");

   function xcb_free_gc_checked (c : xcb_connection_t_access; gc : xcb_gcontext_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11899
   pragma Import (C, xcb_free_gc_checked, "xcb_free_gc_checked");

   function xcb_free_gc (c : xcb_connection_t_access; gc : xcb_gcontext_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11924
   pragma Import (C, xcb_free_gc, "xcb_free_gc");

   function xcb_clear_area_checked
     (c : xcb_connection_t_access;
      exposures : C99.Stdint.uint8_t;
      window : xcb_window_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11955
   pragma Import (C, xcb_clear_area_checked, "xcb_clear_area_checked");

   function xcb_clear_area
     (c : xcb_connection_t_access;
      exposures : C99.Stdint.uint8_t;
      window : xcb_window_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:11988
   pragma Import (C, xcb_clear_area, "xcb_clear_area");

   function xcb_copy_area_checked
     (c : xcb_connection_t_access;
      src_drawable : xcb_drawable_t;
      dst_drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12037
   pragma Import (C, xcb_copy_area_checked, "xcb_copy_area_checked");

   function xcb_copy_area
     (c : xcb_connection_t_access;
      src_drawable : xcb_drawable_t;
      dst_drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12086
   pragma Import (C, xcb_copy_area, "xcb_copy_area");

   function xcb_copy_plane_checked
     (c : xcb_connection_t_access;
      src_drawable : xcb_drawable_t;
      dst_drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      bit_plane : C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12129
   pragma Import (C, xcb_copy_plane_checked, "xcb_copy_plane_checked");

   function xcb_copy_plane
     (c : xcb_connection_t_access;
      src_drawable : xcb_drawable_t;
      dst_drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      src_x : C99.Stdint.int16_t;
      src_y : C99.Stdint.int16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      bit_plane : C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12170
   pragma Import (C, xcb_copy_plane, "xcb_copy_plane");

   function xcb_poly_point_sizeof (u_buffer : System.Address; points_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12183
   pragma Import (C, xcb_poly_point_sizeof, "xcb_poly_point_sizeof");

   function xcb_poly_point_checked
     (c : xcb_connection_t_access;
      coordinate_mode : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      points_len : C99.Stdint.uint32_t;
      points : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12213
   pragma Import (C, xcb_poly_point_checked, "xcb_poly_point_checked");

   function xcb_poly_point
     (c : xcb_connection_t_access;
      coordinate_mode : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      points_len : C99.Stdint.uint32_t;
      points : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12244
   pragma Import (C, xcb_poly_point, "xcb_poly_point");

   function xcb_poly_line_sizeof (u_buffer : System.Address; points_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12252
   pragma Import (C, xcb_poly_line_sizeof, "xcb_poly_line_sizeof");

   function xcb_poly_line_checked
     (c : xcb_connection_t_access;
      coordinate_mode : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      points_len : C99.Stdint.uint32_t;
      points : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12296
   pragma Import (C, xcb_poly_line_checked, "xcb_poly_line_checked");

   function xcb_poly_line
     (c : xcb_connection_t_access;
      coordinate_mode : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      points_len : C99.Stdint.uint32_t;
      points : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12341
   pragma Import (C, xcb_poly_line, "xcb_poly_line");

   procedure xcb_segment_next (i : access xcb_segment_iterator_t);  -- /usr/include/xcb/xproto.h:12367
   pragma Import (C, xcb_segment_next, "xcb_segment_next");

   function xcb_segment_end (i : xcb_segment_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:12389
   pragma Import (C, xcb_segment_end, "xcb_segment_end");

   function xcb_poly_segment_sizeof (u_buffer : System.Address; segments_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12392
   pragma Import (C, xcb_poly_segment_sizeof, "xcb_poly_segment_sizeof");

   function xcb_poly_segment_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      segments_len : C99.Stdint.uint32_t;
      segments : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12436
   pragma Import (C, xcb_poly_segment_checked, "xcb_poly_segment_checked");

   function xcb_poly_segment
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      segments_len : C99.Stdint.uint32_t;
      segments : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12480
   pragma Import (C, xcb_poly_segment, "xcb_poly_segment");

   function xcb_poly_rectangle_sizeof (u_buffer : System.Address; rectangles_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12487
   pragma Import (C, xcb_poly_rectangle_sizeof, "xcb_poly_rectangle_sizeof");

   function xcb_poly_rectangle_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      rectangles_len : C99.Stdint.uint32_t;
      rectangles : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12516
   pragma Import (C, xcb_poly_rectangle_checked, "xcb_poly_rectangle_checked");

   function xcb_poly_rectangle
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      rectangles_len : C99.Stdint.uint32_t;
      rectangles : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12545
   pragma Import (C, xcb_poly_rectangle, "xcb_poly_rectangle");

   function xcb_poly_arc_sizeof (u_buffer : System.Address; arcs_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12552
   pragma Import (C, xcb_poly_arc_sizeof, "xcb_poly_arc_sizeof");

   function xcb_poly_arc_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      arcs_len : C99.Stdint.uint32_t;
      arcs : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12581
   pragma Import (C, xcb_poly_arc_checked, "xcb_poly_arc_checked");

   function xcb_poly_arc
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      arcs_len : C99.Stdint.uint32_t;
      arcs : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12610
   pragma Import (C, xcb_poly_arc, "xcb_poly_arc");

   function xcb_fill_poly_sizeof (u_buffer : System.Address; points_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12617
   pragma Import (C, xcb_fill_poly_sizeof, "xcb_fill_poly_sizeof");

   function xcb_fill_poly_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      shape : C99.Stdint.uint8_t;
      coordinate_mode : C99.Stdint.uint8_t;
      points_len : C99.Stdint.uint32_t;
      points : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12648
   pragma Import (C, xcb_fill_poly_checked, "xcb_fill_poly_checked");

   function xcb_fill_poly
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      shape : C99.Stdint.uint8_t;
      coordinate_mode : C99.Stdint.uint8_t;
      points_len : C99.Stdint.uint32_t;
      points : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12681
   pragma Import (C, xcb_fill_poly, "xcb_fill_poly");

   function xcb_poly_fill_rectangle_sizeof (u_buffer : System.Address; rectangles_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12690
   pragma Import (C, xcb_poly_fill_rectangle_sizeof, "xcb_poly_fill_rectangle_sizeof");

   function xcb_poly_fill_rectangle_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      rectangles_len : C99.Stdint.uint32_t;
      rectangles : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12733
   pragma Import (C, xcb_poly_fill_rectangle_checked, "xcb_poly_fill_rectangle_checked");

   function xcb_poly_fill_rectangle
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      rectangles_len : C99.Stdint.uint32_t;
      rectangles : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12776
   pragma Import (C, xcb_poly_fill_rectangle, "xcb_poly_fill_rectangle");

   function xcb_poly_fill_arc_sizeof (u_buffer : System.Address; arcs_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12783
   pragma Import (C, xcb_poly_fill_arc_sizeof, "xcb_poly_fill_arc_sizeof");

   function xcb_poly_fill_arc_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      arcs_len : C99.Stdint.uint32_t;
      arcs : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12812
   pragma Import (C, xcb_poly_fill_arc_checked, "xcb_poly_fill_arc_checked");

   function xcb_poly_fill_arc
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      arcs_len : C99.Stdint.uint32_t;
      arcs : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12841
   pragma Import (C, xcb_poly_fill_arc, "xcb_poly_fill_arc");

   function xcb_put_image_sizeof (u_buffer : System.Address; data_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:12848
   pragma Import (C, xcb_put_image_sizeof, "xcb_put_image_sizeof");

   function xcb_put_image_checked
     (c : xcb_connection_t_access;
      format : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t;
      left_pad : C99.Stdint.uint8_t;
      depth : C99.Stdint.uint8_t;
      data_len : C99.Stdint.uint32_t;
      data : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12884
   pragma Import (C, xcb_put_image_checked, "xcb_put_image_checked");

   function xcb_put_image
     (c : xcb_connection_t_access;
      format : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      dst_x : C99.Stdint.int16_t;
      dst_y : C99.Stdint.int16_t;
      left_pad : C99.Stdint.uint8_t;
      depth : C99.Stdint.uint8_t;
      data_len : C99.Stdint.uint32_t;
      data : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:12927
   pragma Import (C, xcb_put_image, "xcb_put_image");

   function xcb_get_image_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:12941
   pragma Import (C, xcb_get_image_sizeof, "xcb_get_image_sizeof");

   function xcb_get_image
     (c : xcb_connection_t_access;
      format : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      plane_mask : C99.Stdint.uint32_t) return xcb_get_image_cookie_t;  -- /usr/include/xcb/xproto.h:12969
   pragma Import (C, xcb_get_image, "xcb_get_image");

   function xcb_get_image_unchecked
     (c : xcb_connection_t_access;
      format : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t;
      plane_mask : C99.Stdint.uint32_t) return xcb_get_image_cookie_t;  -- /usr/include/xcb/xproto.h:13007
   pragma Import (C, xcb_get_image_unchecked, "xcb_get_image_unchecked");

   function xcb_get_image_data (R : System.Address) return access C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:13027
   pragma Import (C, xcb_get_image_data, "xcb_get_image_data");

   function xcb_get_image_data_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:13040
   pragma Import (C, xcb_get_image_data_length, "xcb_get_image_data_length");

   function xcb_get_image_data_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:13053
   pragma Import (C, xcb_get_image_data_end, "xcb_get_image_data_end");

   function xcb_get_image_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_image_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_image_reply_t;  -- /usr/include/xcb/xproto.h:13082
   pragma Import (C, xcb_get_image_reply, "xcb_get_image_reply");

   function xcb_poly_text_8_sizeof (u_buffer : System.Address; items_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:13087
   pragma Import (C, xcb_poly_text_8_sizeof, "xcb_poly_text_8_sizeof");

   function xcb_poly_text_8_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      items_len : C99.Stdint.uint32_t;
      items : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13118
   pragma Import (C, xcb_poly_text_8_checked, "xcb_poly_text_8_checked");

   function xcb_poly_text_8
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      items_len : C99.Stdint.uint32_t;
      items : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13151
   pragma Import (C, xcb_poly_text_8, "xcb_poly_text_8");

   function xcb_poly_text_16_sizeof (u_buffer : System.Address; items_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:13160
   pragma Import (C, xcb_poly_text_16_sizeof, "xcb_poly_text_16_sizeof");

   function xcb_poly_text_16_checked
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      items_len : C99.Stdint.uint32_t;
      items : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13191
   pragma Import (C, xcb_poly_text_16_checked, "xcb_poly_text_16_checked");

   function xcb_poly_text_16
     (c : xcb_connection_t_access;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      items_len : C99.Stdint.uint32_t;
      items : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13224
   pragma Import (C, xcb_poly_text_16, "xcb_poly_text_16");

   function xcb_image_text_8_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:13233
   pragma Import (C, xcb_image_text_8_sizeof, "xcb_image_text_8_sizeof");

   function xcb_image_text_8_checked
     (c : xcb_connection_t_access;
      string_len : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      string : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13282
   pragma Import (C, xcb_image_text_8_checked, "xcb_image_text_8_checked");

   function xcb_image_text_8
     (c : xcb_connection_t_access;
      string_len : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      string : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13334
   pragma Import (C, xcb_image_text_8, "xcb_image_text_8");

   function xcb_image_text_16_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:13343
   pragma Import (C, xcb_image_text_16_sizeof, "xcb_image_text_16_sizeof");

   function xcb_image_text_16_checked
     (c : xcb_connection_t_access;
      string_len : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      string : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13393
   pragma Import (C, xcb_image_text_16_checked, "xcb_image_text_16_checked");

   function xcb_image_text_16
     (c : xcb_connection_t_access;
      string_len : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      gc : xcb_gcontext_t;
      x : C99.Stdint.int16_t;
      y : C99.Stdint.int16_t;
      string : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13446
   pragma Import (C, xcb_image_text_16, "xcb_image_text_16");

   function xcb_create_colormap_checked
     (c : xcb_connection_t_access;
      alloc : C99.Stdint.uint8_t;
      mid : xcb_colormap_t;
      window : xcb_window_t;
      visual : xcb_visualid_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13480
   pragma Import (C, xcb_create_colormap_checked, "xcb_create_colormap_checked");

   function xcb_create_colormap
     (c : xcb_connection_t_access;
      alloc : C99.Stdint.uint8_t;
      mid : xcb_colormap_t;
      window : xcb_window_t;
      visual : xcb_visualid_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13509
   pragma Import (C, xcb_create_colormap, "xcb_create_colormap");

   function xcb_free_colormap_checked (c : xcb_connection_t_access; cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13538
   pragma Import (C, xcb_free_colormap_checked, "xcb_free_colormap_checked");

   function xcb_free_colormap (c : xcb_connection_t_access; cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13561
   pragma Import (C, xcb_free_colormap, "xcb_free_colormap");

   function xcb_copy_colormap_and_free_checked
     (c : xcb_connection_t_access;
      mid : xcb_colormap_t;
      src_cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13588
   pragma Import (C, xcb_copy_colormap_and_free_checked, "xcb_copy_colormap_and_free_checked");

   function xcb_copy_colormap_and_free
     (c : xcb_connection_t_access;
      mid : xcb_colormap_t;
      src_cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13613
   pragma Import (C, xcb_copy_colormap_and_free, "xcb_copy_colormap_and_free");

   function xcb_install_colormap_checked (c : xcb_connection_t_access; cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13640
   pragma Import (C, xcb_install_colormap_checked, "xcb_install_colormap_checked");

   function xcb_install_colormap (c : xcb_connection_t_access; cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13663
   pragma Import (C, xcb_install_colormap, "xcb_install_colormap");

   function xcb_uninstall_colormap_checked (c : xcb_connection_t_access; cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13689
   pragma Import (C, xcb_uninstall_colormap_checked, "xcb_uninstall_colormap_checked");

   function xcb_uninstall_colormap (c : xcb_connection_t_access; cmap : xcb_colormap_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:13712
   pragma Import (C, xcb_uninstall_colormap, "xcb_uninstall_colormap");

   function xcb_list_installed_colormaps_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:13716
   pragma Import (C, xcb_list_installed_colormaps_sizeof, "xcb_list_installed_colormaps_sizeof");

   function xcb_list_installed_colormaps (c : xcb_connection_t_access; window : xcb_window_t) return xcb_list_installed_colormaps_cookie_t;  -- /usr/include/xcb/xproto.h:13738
   pragma Import (C, xcb_list_installed_colormaps, "xcb_list_installed_colormaps");

   function xcb_list_installed_colormaps_unchecked (c : xcb_connection_t_access; window : xcb_window_t) return xcb_list_installed_colormaps_cookie_t;  -- /usr/include/xcb/xproto.h:13764
   pragma Import (C, xcb_list_installed_colormaps_unchecked, "xcb_list_installed_colormaps_unchecked");

   function xcb_list_installed_colormaps_cmaps (R : System.Address) return access xcb_colormap_t;  -- /usr/include/xcb/xproto.h:13778
   pragma Import (C, xcb_list_installed_colormaps_cmaps, "xcb_list_installed_colormaps_cmaps");

   function xcb_list_installed_colormaps_cmaps_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:13791
   pragma Import (C, xcb_list_installed_colormaps_cmaps_length, "xcb_list_installed_colormaps_cmaps_length");

   function xcb_list_installed_colormaps_cmaps_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:13804
   pragma Import (C, xcb_list_installed_colormaps_cmaps_end, "xcb_list_installed_colormaps_cmaps_end");

   function xcb_list_installed_colormaps_reply
     (c : xcb_connection_t_access;
      cookie : xcb_list_installed_colormaps_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_list_installed_colormaps_reply_t;  -- /usr/include/xcb/xproto.h:13833
   pragma Import (C, xcb_list_installed_colormaps_reply, "xcb_list_installed_colormaps_reply");

   function xcb_alloc_color
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      red : C99.Stdint.uint16_t;
      green : C99.Stdint.uint16_t;
      blue : C99.Stdint.uint16_t) return xcb_alloc_color_cookie_t;  -- /usr/include/xcb/xproto.h:13869
   pragma Import (C, xcb_alloc_color, "xcb_alloc_color");

   function xcb_alloc_color_unchecked
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      red : C99.Stdint.uint16_t;
      green : C99.Stdint.uint16_t;
      blue : C99.Stdint.uint16_t) return xcb_alloc_color_cookie_t;  -- /usr/include/xcb/xproto.h:13910
   pragma Import (C, xcb_alloc_color_unchecked, "xcb_alloc_color_unchecked");

   function xcb_alloc_color_reply
     (c : xcb_connection_t_access;
      cookie : xcb_alloc_color_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_alloc_color_reply_t;  -- /usr/include/xcb/xproto.h:13943
   pragma Import (C, xcb_alloc_color_reply, "xcb_alloc_color_reply");

   function xcb_alloc_named_color_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:13948
   pragma Import (C, xcb_alloc_named_color_sizeof, "xcb_alloc_named_color_sizeof");

   function xcb_alloc_named_color
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_alloc_named_color_cookie_t;  -- /usr/include/xcb/xproto.h:13972
   pragma Import (C, xcb_alloc_named_color, "xcb_alloc_named_color");

   function xcb_alloc_named_color_unchecked
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_alloc_named_color_cookie_t;  -- /usr/include/xcb/xproto.h:14002
   pragma Import (C, xcb_alloc_named_color_unchecked, "xcb_alloc_named_color_unchecked");

   function xcb_alloc_named_color_reply
     (c : xcb_connection_t_access;
      cookie : xcb_alloc_named_color_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_alloc_named_color_reply_t;  -- /usr/include/xcb/xproto.h:14034
   pragma Import (C, xcb_alloc_named_color_reply, "xcb_alloc_named_color_reply");

   function xcb_alloc_color_cells_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:14039
   pragma Import (C, xcb_alloc_color_cells_sizeof, "xcb_alloc_color_cells_sizeof");

   function xcb_alloc_color_cells
     (c : xcb_connection_t_access;
      contiguous : C99.Stdint.uint8_t;
      cmap : xcb_colormap_t;
      colors : C99.Stdint.uint16_t;
      planes : C99.Stdint.uint16_t) return xcb_alloc_color_cells_cookie_t;  -- /usr/include/xcb/xproto.h:14064
   pragma Import (C, xcb_alloc_color_cells, "xcb_alloc_color_cells");

   function xcb_alloc_color_cells_unchecked
     (c : xcb_connection_t_access;
      contiguous : C99.Stdint.uint8_t;
      cmap : xcb_colormap_t;
      colors : C99.Stdint.uint16_t;
      planes : C99.Stdint.uint16_t) return xcb_alloc_color_cells_cookie_t;  -- /usr/include/xcb/xproto.h:14096
   pragma Import (C, xcb_alloc_color_cells_unchecked, "xcb_alloc_color_cells_unchecked");

   function xcb_alloc_color_cells_pixels (R : System.Address) return access C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:14113
   pragma Import (C, xcb_alloc_color_cells_pixels, "xcb_alloc_color_cells_pixels");

   function xcb_alloc_color_cells_pixels_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:14126
   pragma Import (C, xcb_alloc_color_cells_pixels_length, "xcb_alloc_color_cells_pixels_length");

   function xcb_alloc_color_cells_pixels_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:14139
   pragma Import (C, xcb_alloc_color_cells_pixels_end, "xcb_alloc_color_cells_pixels_end");

   function xcb_alloc_color_cells_masks (R : System.Address) return access C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:14152
   pragma Import (C, xcb_alloc_color_cells_masks, "xcb_alloc_color_cells_masks");

   function xcb_alloc_color_cells_masks_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:14165
   pragma Import (C, xcb_alloc_color_cells_masks_length, "xcb_alloc_color_cells_masks_length");

   function xcb_alloc_color_cells_masks_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:14178
   pragma Import (C, xcb_alloc_color_cells_masks_end, "xcb_alloc_color_cells_masks_end");

   function xcb_alloc_color_cells_reply
     (c : xcb_connection_t_access;
      cookie : xcb_alloc_color_cells_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_alloc_color_cells_reply_t;  -- /usr/include/xcb/xproto.h:14207
   pragma Import (C, xcb_alloc_color_cells_reply, "xcb_alloc_color_cells_reply");

   function xcb_alloc_color_planes_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:14212
   pragma Import (C, xcb_alloc_color_planes_sizeof, "xcb_alloc_color_planes_sizeof");

   function xcb_alloc_color_planes
     (c : xcb_connection_t_access;
      contiguous : C99.Stdint.uint8_t;
      cmap : xcb_colormap_t;
      colors : C99.Stdint.uint16_t;
      reds : C99.Stdint.uint16_t;
      greens : C99.Stdint.uint16_t;
      blues : C99.Stdint.uint16_t) return xcb_alloc_color_planes_cookie_t;  -- /usr/include/xcb/xproto.h:14239
   pragma Import (C, xcb_alloc_color_planes, "xcb_alloc_color_planes");

   function xcb_alloc_color_planes_unchecked
     (c : xcb_connection_t_access;
      contiguous : C99.Stdint.uint8_t;
      cmap : xcb_colormap_t;
      colors : C99.Stdint.uint16_t;
      reds : C99.Stdint.uint16_t;
      greens : C99.Stdint.uint16_t;
      blues : C99.Stdint.uint16_t) return xcb_alloc_color_planes_cookie_t;  -- /usr/include/xcb/xproto.h:14275
   pragma Import (C, xcb_alloc_color_planes_unchecked, "xcb_alloc_color_planes_unchecked");

   function xcb_alloc_color_planes_pixels (R : System.Address) return access C99.Stdint.uint32_t;  -- /usr/include/xcb/xproto.h:14294
   pragma Import (C, xcb_alloc_color_planes_pixels, "xcb_alloc_color_planes_pixels");

   function xcb_alloc_color_planes_pixels_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:14307
   pragma Import (C, xcb_alloc_color_planes_pixels_length, "xcb_alloc_color_planes_pixels_length");

   function xcb_alloc_color_planes_pixels_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:14320
   pragma Import (C, xcb_alloc_color_planes_pixels_end, "xcb_alloc_color_planes_pixels_end");

   function xcb_alloc_color_planes_reply
     (c : xcb_connection_t_access;
      cookie : xcb_alloc_color_planes_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_alloc_color_planes_reply_t;  -- /usr/include/xcb/xproto.h:14349
   pragma Import (C, xcb_alloc_color_planes_reply, "xcb_alloc_color_planes_reply");

   function xcb_free_colors_sizeof (u_buffer : System.Address; pixels_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:14354
   pragma Import (C, xcb_free_colors_sizeof, "xcb_free_colors_sizeof");

   function xcb_free_colors_checked
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      plane_mask : C99.Stdint.uint32_t;
      pixels_len : C99.Stdint.uint32_t;
      pixels : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14383
   pragma Import (C, xcb_free_colors_checked, "xcb_free_colors_checked");

   function xcb_free_colors
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      plane_mask : C99.Stdint.uint32_t;
      pixels_len : C99.Stdint.uint32_t;
      pixels : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14412
   pragma Import (C, xcb_free_colors, "xcb_free_colors");

   procedure xcb_coloritem_next (i : access xcb_coloritem_iterator_t);  -- /usr/include/xcb/xproto.h:14437
   pragma Import (C, xcb_coloritem_next, "xcb_coloritem_next");

   function xcb_coloritem_end (i : xcb_coloritem_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:14459
   pragma Import (C, xcb_coloritem_end, "xcb_coloritem_end");

   function xcb_store_colors_sizeof (u_buffer : System.Address; items_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:14462
   pragma Import (C, xcb_store_colors_sizeof, "xcb_store_colors_sizeof");

   function xcb_store_colors_checked
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      items_len : C99.Stdint.uint32_t;
      items : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14490
   pragma Import (C, xcb_store_colors_checked, "xcb_store_colors_checked");

   function xcb_store_colors
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      items_len : C99.Stdint.uint32_t;
      items : System.Address) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14517
   pragma Import (C, xcb_store_colors, "xcb_store_colors");

   function xcb_store_named_color_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:14523
   pragma Import (C, xcb_store_named_color_sizeof, "xcb_store_named_color_sizeof");

   function xcb_store_named_color_checked
     (c : xcb_connection_t_access;
      flags : C99.Stdint.uint8_t;
      cmap : xcb_colormap_t;
      pixel : C99.Stdint.uint32_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14552
   pragma Import (C, xcb_store_named_color_checked, "xcb_store_named_color_checked");

   function xcb_store_named_color
     (c : xcb_connection_t_access;
      flags : C99.Stdint.uint8_t;
      cmap : xcb_colormap_t;
      pixel : C99.Stdint.uint32_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14583
   pragma Import (C, xcb_store_named_color, "xcb_store_named_color");

   procedure xcb_rgb_next (i : access xcb_rgb_iterator_t);  -- /usr/include/xcb/xproto.h:14609
   pragma Import (C, xcb_rgb_next, "xcb_rgb_next");

   function xcb_rgb_end (i : xcb_rgb_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:14631
   pragma Import (C, xcb_rgb_end, "xcb_rgb_end");

   function xcb_query_colors_sizeof (u_buffer : System.Address; pixels_len : C99.Stdint.uint32_t) return int;  -- /usr/include/xcb/xproto.h:14634
   pragma Import (C, xcb_query_colors_sizeof, "xcb_query_colors_sizeof");

   function xcb_query_colors
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      pixels_len : C99.Stdint.uint32_t;
      pixels : access C99.Stdint.uint32_t) return xcb_query_colors_cookie_t;  -- /usr/include/xcb/xproto.h:14659
   pragma Import (C, xcb_query_colors, "xcb_query_colors");

   function xcb_query_colors_unchecked
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      pixels_len : C99.Stdint.uint32_t;
      pixels : access C99.Stdint.uint32_t) return xcb_query_colors_cookie_t;  -- /usr/include/xcb/xproto.h:14689
   pragma Import (C, xcb_query_colors_unchecked, "xcb_query_colors_unchecked");

   function xcb_query_colors_colors (R : System.Address) return access xcb_rgb_t;  -- /usr/include/xcb/xproto.h:14705
   pragma Import (C, xcb_query_colors_colors, "xcb_query_colors_colors");

   function xcb_query_colors_colors_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:14718
   pragma Import (C, xcb_query_colors_colors_length, "xcb_query_colors_colors_length");

   function xcb_query_colors_colors_iterator (R : System.Address) return xcb_rgb_iterator_t;  -- /usr/include/xcb/xproto.h:14731
   pragma Import (C, xcb_query_colors_colors_iterator, "xcb_query_colors_colors_iterator");

   function xcb_query_colors_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_colors_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_colors_reply_t;  -- /usr/include/xcb/xproto.h:14760
   pragma Import (C, xcb_query_colors_reply, "xcb_query_colors_reply");

   function xcb_lookup_color_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:14765
   pragma Import (C, xcb_lookup_color_sizeof, "xcb_lookup_color_sizeof");

   function xcb_lookup_color
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_lookup_color_cookie_t;  -- /usr/include/xcb/xproto.h:14789
   pragma Import (C, xcb_lookup_color, "xcb_lookup_color");

   function xcb_lookup_color_unchecked
     (c : xcb_connection_t_access;
      cmap : xcb_colormap_t;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_lookup_color_cookie_t;  -- /usr/include/xcb/xproto.h:14819
   pragma Import (C, xcb_lookup_color_unchecked, "xcb_lookup_color_unchecked");

   function xcb_lookup_color_reply
     (c : xcb_connection_t_access;
      cookie : xcb_lookup_color_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_lookup_color_reply_t;  -- /usr/include/xcb/xproto.h:14851
   pragma Import (C, xcb_lookup_color_reply, "xcb_lookup_color_reply");

   function xcb_create_cursor_checked
     (c : xcb_connection_t_access;
      cid : xcb_cursor_t;
      source : xcb_pixmap_t;
      mask : xcb_pixmap_t;
      fore_red : C99.Stdint.uint16_t;
      fore_green : C99.Stdint.uint16_t;
      fore_blue : C99.Stdint.uint16_t;
      back_red : C99.Stdint.uint16_t;
      back_green : C99.Stdint.uint16_t;
      back_blue : C99.Stdint.uint16_t;
      x : C99.Stdint.uint16_t;
      y : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14888
   pragma Import (C, xcb_create_cursor_checked, "xcb_create_cursor_checked");

   function xcb_create_cursor
     (c : xcb_connection_t_access;
      cid : xcb_cursor_t;
      source : xcb_pixmap_t;
      mask : xcb_pixmap_t;
      fore_red : C99.Stdint.uint16_t;
      fore_green : C99.Stdint.uint16_t;
      fore_blue : C99.Stdint.uint16_t;
      back_red : C99.Stdint.uint16_t;
      back_green : C99.Stdint.uint16_t;
      back_blue : C99.Stdint.uint16_t;
      x : C99.Stdint.uint16_t;
      y : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14931
   pragma Import (C, xcb_create_cursor, "xcb_create_cursor");

   function xcb_create_glyph_cursor_checked
     (c : xcb_connection_t_access;
      cid : xcb_cursor_t;
      source_font : xcb_font_t;
      mask_font : xcb_font_t;
      source_char : C99.Stdint.uint16_t;
      mask_char : C99.Stdint.uint16_t;
      fore_red : C99.Stdint.uint16_t;
      fore_green : C99.Stdint.uint16_t;
      fore_blue : C99.Stdint.uint16_t;
      back_red : C99.Stdint.uint16_t;
      back_green : C99.Stdint.uint16_t;
      back_blue : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:14999
   pragma Import (C, xcb_create_glyph_cursor_checked, "xcb_create_glyph_cursor_checked");

   function xcb_create_glyph_cursor
     (c : xcb_connection_t_access;
      cid : xcb_cursor_t;
      source_font : xcb_font_t;
      mask_font : xcb_font_t;
      source_char : C99.Stdint.uint16_t;
      mask_char : C99.Stdint.uint16_t;
      fore_red : C99.Stdint.uint16_t;
      fore_green : C99.Stdint.uint16_t;
      fore_blue : C99.Stdint.uint16_t;
      back_red : C99.Stdint.uint16_t;
      back_green : C99.Stdint.uint16_t;
      back_blue : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15064
   pragma Import (C, xcb_create_glyph_cursor, "xcb_create_glyph_cursor");

   function xcb_free_cursor_checked (c : xcb_connection_t_access; cursor : xcb_cursor_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15103
   pragma Import (C, xcb_free_cursor_checked, "xcb_free_cursor_checked");

   function xcb_free_cursor (c : xcb_connection_t_access; cursor : xcb_cursor_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15129
   pragma Import (C, xcb_free_cursor, "xcb_free_cursor");

   function xcb_recolor_cursor_checked
     (c : xcb_connection_t_access;
      cursor : xcb_cursor_t;
      fore_red : C99.Stdint.uint16_t;
      fore_green : C99.Stdint.uint16_t;
      fore_blue : C99.Stdint.uint16_t;
      back_red : C99.Stdint.uint16_t;
      back_green : C99.Stdint.uint16_t;
      back_blue : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15161
   pragma Import (C, xcb_recolor_cursor_checked, "xcb_recolor_cursor_checked");

   function xcb_recolor_cursor
     (c : xcb_connection_t_access;
      cursor : xcb_cursor_t;
      fore_red : C99.Stdint.uint16_t;
      fore_green : C99.Stdint.uint16_t;
      fore_blue : C99.Stdint.uint16_t;
      back_red : C99.Stdint.uint16_t;
      back_green : C99.Stdint.uint16_t;
      back_blue : C99.Stdint.uint16_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15196
   pragma Import (C, xcb_recolor_cursor, "xcb_recolor_cursor");

   function xcb_query_best_size
     (c : xcb_connection_t_access;
      u_class : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return xcb_query_best_size_cookie_t;  -- /usr/include/xcb/xproto.h:15228
   pragma Import (C, xcb_query_best_size, "xcb_query_best_size");

   function xcb_query_best_size_unchecked
     (c : xcb_connection_t_access;
      u_class : C99.Stdint.uint8_t;
      drawable : xcb_drawable_t;
      width : C99.Stdint.uint16_t;
      height : C99.Stdint.uint16_t) return xcb_query_best_size_cookie_t;  -- /usr/include/xcb/xproto.h:15260
   pragma Import (C, xcb_query_best_size_unchecked, "xcb_query_best_size_unchecked");

   function xcb_query_best_size_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_best_size_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_best_size_reply_t;  -- /usr/include/xcb/xproto.h:15293
   pragma Import (C, xcb_query_best_size_reply, "xcb_query_best_size_reply");

   function xcb_query_extension_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:15298
   pragma Import (C, xcb_query_extension_sizeof, "xcb_query_extension_sizeof");

   function xcb_query_extension
     (c : xcb_connection_t_access;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_query_extension_cookie_t;  -- /usr/include/xcb/xproto.h:15333
   pragma Import (C, xcb_query_extension, "xcb_query_extension");

   function xcb_query_extension_unchecked
     (c : xcb_connection_t_access;
      name_len : C99.Stdint.uint16_t;
      name : Interfaces.C.Strings.chars_ptr) return xcb_query_extension_cookie_t;  -- /usr/include/xcb/xproto.h:15373
   pragma Import (C, xcb_query_extension_unchecked, "xcb_query_extension_unchecked");

   function xcb_query_extension_reply
     (c : xcb_connection_t_access;
      cookie : xcb_query_extension_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_query_extension_reply_t;  -- /usr/include/xcb/xproto.h:15404
   pragma Import (C, xcb_query_extension_reply, "xcb_query_extension_reply");

   function xcb_list_extensions_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:15409
   pragma Import (C, xcb_list_extensions_sizeof, "xcb_list_extensions_sizeof");

   function xcb_list_extensions (c : xcb_connection_t) return xcb_list_extensions_cookie_t;  -- /usr/include/xcb/xproto.h:15430
   pragma Import (C, xcb_list_extensions, "xcb_list_extensions");

   function xcb_list_extensions_unchecked (c : xcb_connection_t) return xcb_list_extensions_cookie_t;  -- /usr/include/xcb/xproto.h:15454
   pragma Import (C, xcb_list_extensions_unchecked, "xcb_list_extensions_unchecked");

   function xcb_list_extensions_names_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:15467
   pragma Import (C, xcb_list_extensions_names_length, "xcb_list_extensions_names_length");

   function xcb_list_extensions_names_iterator (R : System.Address) return xcb_str_iterator_t;  -- /usr/include/xcb/xproto.h:15480
   pragma Import (C, xcb_list_extensions_names_iterator, "xcb_list_extensions_names_iterator");

   function xcb_list_extensions_reply
     (c : xcb_connection_t_access;
      cookie : xcb_list_extensions_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_list_extensions_reply_t;  -- /usr/include/xcb/xproto.h:15509
   pragma Import (C, xcb_list_extensions_reply, "xcb_list_extensions_reply");

   function xcb_change_keyboard_mapping_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:15514
   pragma Import (C, xcb_change_keyboard_mapping_sizeof, "xcb_change_keyboard_mapping_sizeof");

   function xcb_change_keyboard_mapping_checked
     (c : xcb_connection_t_access;
      keycode_count : C99.Stdint.uint8_t;
      first_keycode : xcb_keycode_t;
      keysyms_per_keycode : C99.Stdint.uint8_t;
      keysyms : access xcb_keysym_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15542
   pragma Import (C, xcb_change_keyboard_mapping_checked, "xcb_change_keyboard_mapping_checked");

   function xcb_change_keyboard_mapping
     (c : xcb_connection_t_access;
      keycode_count : C99.Stdint.uint8_t;
      first_keycode : xcb_keycode_t;
      keysyms_per_keycode : C99.Stdint.uint8_t;
      keysyms : access xcb_keysym_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15571
   pragma Import (C, xcb_change_keyboard_mapping, "xcb_change_keyboard_mapping");

   function xcb_get_keyboard_mapping_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:15578
   pragma Import (C, xcb_get_keyboard_mapping_sizeof, "xcb_get_keyboard_mapping_sizeof");

   function xcb_get_keyboard_mapping
     (c : xcb_connection_t_access;
      first_keycode : xcb_keycode_t;
      count : C99.Stdint.uint8_t) return xcb_get_keyboard_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:15601
   pragma Import (C, xcb_get_keyboard_mapping, "xcb_get_keyboard_mapping");

   function xcb_get_keyboard_mapping_unchecked
     (c : xcb_connection_t_access;
      first_keycode : xcb_keycode_t;
      count : C99.Stdint.uint8_t) return xcb_get_keyboard_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:15629
   pragma Import (C, xcb_get_keyboard_mapping_unchecked, "xcb_get_keyboard_mapping_unchecked");

   function xcb_get_keyboard_mapping_keysyms (R : System.Address) return access xcb_keysym_t;  -- /usr/include/xcb/xproto.h:15644
   pragma Import (C, xcb_get_keyboard_mapping_keysyms, "xcb_get_keyboard_mapping_keysyms");

   function xcb_get_keyboard_mapping_keysyms_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:15657
   pragma Import (C, xcb_get_keyboard_mapping_keysyms_length, "xcb_get_keyboard_mapping_keysyms_length");

   function xcb_get_keyboard_mapping_keysyms_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:15670
   pragma Import (C, xcb_get_keyboard_mapping_keysyms_end, "xcb_get_keyboard_mapping_keysyms_end");

   function xcb_get_keyboard_mapping_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_keyboard_mapping_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_keyboard_mapping_reply_t;  -- /usr/include/xcb/xproto.h:15699
   pragma Import (C, xcb_get_keyboard_mapping_reply, "xcb_get_keyboard_mapping_reply");

   function xcb_change_keyboard_control_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:15704
   pragma Import (C, xcb_change_keyboard_control_sizeof, "xcb_change_keyboard_control_sizeof");

   function xcb_change_keyboard_control_checked
     (c : xcb_connection_t_access;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15730
   pragma Import (C, xcb_change_keyboard_control_checked, "xcb_change_keyboard_control_checked");

   function xcb_change_keyboard_control
     (c : xcb_connection_t_access;
      value_mask : C99.Stdint.uint32_t;
      value_list : access C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15755
   pragma Import (C, xcb_change_keyboard_control, "xcb_change_keyboard_control");

   function xcb_get_keyboard_control (c : xcb_connection_t) return xcb_get_keyboard_control_cookie_t;  -- /usr/include/xcb/xproto.h:15778
   pragma Import (C, xcb_get_keyboard_control, "xcb_get_keyboard_control");

   function xcb_get_keyboard_control_unchecked (c : xcb_connection_t) return xcb_get_keyboard_control_cookie_t;  -- /usr/include/xcb/xproto.h:15802
   pragma Import (C, xcb_get_keyboard_control_unchecked, "xcb_get_keyboard_control_unchecked");

   function xcb_get_keyboard_control_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_keyboard_control_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_keyboard_control_reply_t;  -- /usr/include/xcb/xproto.h:15831
   pragma Import (C, xcb_get_keyboard_control_reply, "xcb_get_keyboard_control_reply");

   function xcb_bell_checked (c : xcb_connection_t_access; percent : C99.Stdint.int8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15858
   pragma Import (C, xcb_bell_checked, "xcb_bell_checked");

   function xcb_bell (c : xcb_connection_t_access; percent : C99.Stdint.int8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15881
   pragma Import (C, xcb_bell, "xcb_bell");

   function xcb_change_pointer_control_checked
     (c : xcb_connection_t_access;
      acceleration_numerator : C99.Stdint.int16_t;
      acceleration_denominator : C99.Stdint.int16_t;
      threshold : C99.Stdint.int16_t;
      do_acceleration : C99.Stdint.uint8_t;
      do_threshold : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15911
   pragma Import (C, xcb_change_pointer_control_checked, "xcb_change_pointer_control_checked");

   function xcb_change_pointer_control
     (c : xcb_connection_t_access;
      acceleration_numerator : C99.Stdint.int16_t;
      acceleration_denominator : C99.Stdint.int16_t;
      threshold : C99.Stdint.int16_t;
      do_acceleration : C99.Stdint.uint8_t;
      do_threshold : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:15942
   pragma Import (C, xcb_change_pointer_control, "xcb_change_pointer_control");

   function xcb_get_pointer_control (c : xcb_connection_t) return xcb_get_pointer_control_cookie_t;  -- /usr/include/xcb/xproto.h:15968
   pragma Import (C, xcb_get_pointer_control, "xcb_get_pointer_control");

   function xcb_get_pointer_control_unchecked (c : xcb_connection_t) return xcb_get_pointer_control_cookie_t;  -- /usr/include/xcb/xproto.h:15992
   pragma Import (C, xcb_get_pointer_control_unchecked, "xcb_get_pointer_control_unchecked");

   function xcb_get_pointer_control_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_pointer_control_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_pointer_control_reply_t;  -- /usr/include/xcb/xproto.h:16021
   pragma Import (C, xcb_get_pointer_control_reply, "xcb_get_pointer_control_reply");

   function xcb_set_screen_saver_checked
     (c : xcb_connection_t_access;
      timeout : C99.Stdint.int16_t;
      interval : C99.Stdint.int16_t;
      prefer_blanking : C99.Stdint.uint8_t;
      allow_exposures : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16051
   pragma Import (C, xcb_set_screen_saver_checked, "xcb_set_screen_saver_checked");

   function xcb_set_screen_saver
     (c : xcb_connection_t_access;
      timeout : C99.Stdint.int16_t;
      interval : C99.Stdint.int16_t;
      prefer_blanking : C99.Stdint.uint8_t;
      allow_exposures : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16080
   pragma Import (C, xcb_set_screen_saver, "xcb_set_screen_saver");

   function xcb_get_screen_saver (c : xcb_connection_t) return xcb_get_screen_saver_cookie_t;  -- /usr/include/xcb/xproto.h:16105
   pragma Import (C, xcb_get_screen_saver, "xcb_get_screen_saver");

   function xcb_get_screen_saver_unchecked (c : xcb_connection_t) return xcb_get_screen_saver_cookie_t;  -- /usr/include/xcb/xproto.h:16129
   pragma Import (C, xcb_get_screen_saver_unchecked, "xcb_get_screen_saver_unchecked");

   function xcb_get_screen_saver_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_screen_saver_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_screen_saver_reply_t;  -- /usr/include/xcb/xproto.h:16158
   pragma Import (C, xcb_get_screen_saver_reply, "xcb_get_screen_saver_reply");

   function xcb_change_hosts_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16163
   pragma Import (C, xcb_change_hosts_sizeof, "xcb_change_hosts_sizeof");

   function xcb_change_hosts_checked
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      family : C99.Stdint.uint8_t;
      address_len : C99.Stdint.uint16_t;
      address : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16191
   pragma Import (C, xcb_change_hosts_checked, "xcb_change_hosts_checked");

   function xcb_change_hosts
     (c : xcb_connection_t_access;
      mode : C99.Stdint.uint8_t;
      family : C99.Stdint.uint8_t;
      address_len : C99.Stdint.uint16_t;
      address : access C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16220
   pragma Import (C, xcb_change_hosts, "xcb_change_hosts");

   function xcb_host_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16227
   pragma Import (C, xcb_host_sizeof, "xcb_host_sizeof");

   function xcb_host_address (R : System.Address) return access C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:16240
   pragma Import (C, xcb_host_address, "xcb_host_address");

   function xcb_host_address_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:16253
   pragma Import (C, xcb_host_address_length, "xcb_host_address_length");

   function xcb_host_address_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:16266
   pragma Import (C, xcb_host_address_end, "xcb_host_address_end");

   procedure xcb_host_next (i : access xcb_host_iterator_t);  -- /usr/include/xcb/xproto.h:16287
   pragma Import (C, xcb_host_next, "xcb_host_next");

   function xcb_host_end (i : xcb_host_iterator_t) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:16309
   pragma Import (C, xcb_host_end, "xcb_host_end");

   function xcb_list_hosts_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16312
   pragma Import (C, xcb_list_hosts_sizeof, "xcb_list_hosts_sizeof");

   function xcb_list_hosts (c : xcb_connection_t) return xcb_list_hosts_cookie_t;  -- /usr/include/xcb/xproto.h:16333
   pragma Import (C, xcb_list_hosts, "xcb_list_hosts");

   function xcb_list_hosts_unchecked (c : xcb_connection_t) return xcb_list_hosts_cookie_t;  -- /usr/include/xcb/xproto.h:16357
   pragma Import (C, xcb_list_hosts_unchecked, "xcb_list_hosts_unchecked");

   function xcb_list_hosts_hosts_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:16370
   pragma Import (C, xcb_list_hosts_hosts_length, "xcb_list_hosts_hosts_length");

   function xcb_list_hosts_hosts_iterator (R : System.Address) return xcb_host_iterator_t;  -- /usr/include/xcb/xproto.h:16383
   pragma Import (C, xcb_list_hosts_hosts_iterator, "xcb_list_hosts_hosts_iterator");

   function xcb_list_hosts_reply
     (c : xcb_connection_t_access;
      cookie : xcb_list_hosts_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_list_hosts_reply_t;  -- /usr/include/xcb/xproto.h:16412
   pragma Import (C, xcb_list_hosts_reply, "xcb_list_hosts_reply");

   function xcb_set_access_control_checked (c : xcb_connection_t_access; mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16439
   pragma Import (C, xcb_set_access_control_checked, "xcb_set_access_control_checked");

   function xcb_set_access_control (c : xcb_connection_t_access; mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16462
   pragma Import (C, xcb_set_access_control, "xcb_set_access_control");

   function xcb_set_close_down_mode_checked (c : xcb_connection_t_access; mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16488
   pragma Import (C, xcb_set_close_down_mode_checked, "xcb_set_close_down_mode_checked");

   function xcb_set_close_down_mode (c : xcb_connection_t_access; mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16511
   pragma Import (C, xcb_set_close_down_mode, "xcb_set_close_down_mode");

   function xcb_kill_client_checked (c : xcb_connection_t_access; resource : C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16543
   pragma Import (C, xcb_kill_client_checked, "xcb_kill_client_checked");

   function xcb_kill_client (c : xcb_connection_t_access; resource : C99.Stdint.uint32_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16572
   pragma Import (C, xcb_kill_client, "xcb_kill_client");

   function xcb_rotate_properties_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16576
   pragma Import (C, xcb_rotate_properties_sizeof, "xcb_rotate_properties_sizeof");

   function xcb_rotate_properties_checked
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      atoms_len : C99.Stdint.uint16_t;
      c_delta : C99.Stdint.int16_t;
      atoms : access xcb_atom_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16604
   pragma Import (C, xcb_rotate_properties_checked, "xcb_rotate_properties_checked");

   function xcb_rotate_properties
     (c : xcb_connection_t_access;
      window : xcb_window_t;
      atoms_len : C99.Stdint.uint16_t;
      c_delta : C99.Stdint.int16_t;
      atoms : access xcb_atom_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16633
   pragma Import (C, xcb_rotate_properties, "xcb_rotate_properties");

   function xcb_force_screen_saver_checked (c : xcb_connection_t_access; mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16662
   pragma Import (C, xcb_force_screen_saver_checked, "xcb_force_screen_saver_checked");

   function xcb_force_screen_saver (c : xcb_connection_t_access; mode : C99.Stdint.uint8_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:16685
   pragma Import (C, xcb_force_screen_saver, "xcb_force_screen_saver");

   function xcb_set_pointer_mapping_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16689
   pragma Import (C, xcb_set_pointer_mapping_sizeof, "xcb_set_pointer_mapping_sizeof");

   function xcb_set_pointer_mapping
     (c : xcb_connection_t_access;
      map_len : C99.Stdint.uint8_t;
      map : access C99.Stdint.uint8_t) return xcb_set_pointer_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:16712
   pragma Import (C, xcb_set_pointer_mapping, "xcb_set_pointer_mapping");

   function xcb_set_pointer_mapping_unchecked
     (c : xcb_connection_t_access;
      map_len : C99.Stdint.uint8_t;
      map : access C99.Stdint.uint8_t) return xcb_set_pointer_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:16740
   pragma Import (C, xcb_set_pointer_mapping_unchecked, "xcb_set_pointer_mapping_unchecked");

   function xcb_set_pointer_mapping_reply
     (c : xcb_connection_t_access;
      cookie : xcb_set_pointer_mapping_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_set_pointer_mapping_reply_t;  -- /usr/include/xcb/xproto.h:16771
   pragma Import (C, xcb_set_pointer_mapping_reply, "xcb_set_pointer_mapping_reply");

   function xcb_get_pointer_mapping_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16776
   pragma Import (C, xcb_get_pointer_mapping_sizeof, "xcb_get_pointer_mapping_sizeof");

   function xcb_get_pointer_mapping (c : xcb_connection_t) return xcb_get_pointer_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:16797
   pragma Import (C, xcb_get_pointer_mapping, "xcb_get_pointer_mapping");

   function xcb_get_pointer_mapping_unchecked (c : xcb_connection_t) return xcb_get_pointer_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:16821
   pragma Import (C, xcb_get_pointer_mapping_unchecked, "xcb_get_pointer_mapping_unchecked");

   function xcb_get_pointer_mapping_map (R : System.Address) return access C99.Stdint.uint8_t;  -- /usr/include/xcb/xproto.h:16834
   pragma Import (C, xcb_get_pointer_mapping_map, "xcb_get_pointer_mapping_map");

   function xcb_get_pointer_mapping_map_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:16847
   pragma Import (C, xcb_get_pointer_mapping_map_length, "xcb_get_pointer_mapping_map_length");

   function xcb_get_pointer_mapping_map_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:16860
   pragma Import (C, xcb_get_pointer_mapping_map_end, "xcb_get_pointer_mapping_map_end");

   function xcb_get_pointer_mapping_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_pointer_mapping_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_pointer_mapping_reply_t;  -- /usr/include/xcb/xproto.h:16889
   pragma Import (C, xcb_get_pointer_mapping_reply, "xcb_get_pointer_mapping_reply");

   function xcb_set_modifier_mapping_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16894
   pragma Import (C, xcb_set_modifier_mapping_sizeof, "xcb_set_modifier_mapping_sizeof");

   function xcb_set_modifier_mapping
     (c : xcb_connection_t_access;
      keycodes_per_modifier : C99.Stdint.uint8_t;
      keycodes : access xcb_keycode_t) return xcb_set_modifier_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:16917
   pragma Import (C, xcb_set_modifier_mapping, "xcb_set_modifier_mapping");

   function xcb_set_modifier_mapping_unchecked
     (c : xcb_connection_t_access;
      keycodes_per_modifier : C99.Stdint.uint8_t;
      keycodes : access xcb_keycode_t) return xcb_set_modifier_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:16945
   pragma Import (C, xcb_set_modifier_mapping_unchecked, "xcb_set_modifier_mapping_unchecked");

   function xcb_set_modifier_mapping_reply
     (c : xcb_connection_t_access;
      cookie : xcb_set_modifier_mapping_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_set_modifier_mapping_reply_t;  -- /usr/include/xcb/xproto.h:16976
   pragma Import (C, xcb_set_modifier_mapping_reply, "xcb_set_modifier_mapping_reply");

   function xcb_get_modifier_mapping_sizeof (u_buffer : System.Address) return int;  -- /usr/include/xcb/xproto.h:16981
   pragma Import (C, xcb_get_modifier_mapping_sizeof, "xcb_get_modifier_mapping_sizeof");

   function xcb_get_modifier_mapping (c : xcb_connection_t) return xcb_get_modifier_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:17002
   pragma Import (C, xcb_get_modifier_mapping, "xcb_get_modifier_mapping");

   function xcb_get_modifier_mapping_unchecked (c : xcb_connection_t) return xcb_get_modifier_mapping_cookie_t;  -- /usr/include/xcb/xproto.h:17026
   pragma Import (C, xcb_get_modifier_mapping_unchecked, "xcb_get_modifier_mapping_unchecked");

   function xcb_get_modifier_mapping_keycodes (R : System.Address) return access xcb_keycode_t;  -- /usr/include/xcb/xproto.h:17039
   pragma Import (C, xcb_get_modifier_mapping_keycodes, "xcb_get_modifier_mapping_keycodes");

   function xcb_get_modifier_mapping_keycodes_length (R : System.Address) return int;  -- /usr/include/xcb/xproto.h:17052
   pragma Import (C, xcb_get_modifier_mapping_keycodes_length, "xcb_get_modifier_mapping_keycodes_length");

   function xcb_get_modifier_mapping_keycodes_end (R : System.Address) return XCB.xcb_generic_iterator_t;  -- /usr/include/xcb/xproto.h:17065
   pragma Import (C, xcb_get_modifier_mapping_keycodes_end, "xcb_get_modifier_mapping_keycodes_end");

   function xcb_get_modifier_mapping_reply
     (c : xcb_connection_t_access;
      cookie : xcb_get_modifier_mapping_cookie_t;
      e : access xcb_generic_error_t_access) return access xcb_get_modifier_mapping_reply_t;  -- /usr/include/xcb/xproto.h:17094
   pragma Import (C, xcb_get_modifier_mapping_reply, "xcb_get_modifier_mapping_reply");

   function xcb_no_operation_checked (c : xcb_connection_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:17120
   pragma Import (C, xcb_no_operation_checked, "xcb_no_operation_checked");

   function xcb_no_operation (c : xcb_connection_t) return XCB.xcb_void_cookie_t;  -- /usr/include/xcb/xproto.h:17141
   pragma Import (C, xcb_no_operation, "xcb_no_operation");

end XCB.XProto;
