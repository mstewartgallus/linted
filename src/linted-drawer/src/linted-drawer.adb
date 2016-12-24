-- Copyright 2016 Steven Stewart-Gallus
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http ://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
with Ada.Command_Line;
with Ada.Synchronous_Task_Control;
with Ada.Unchecked_Conversion;

with System;
with System.Storage_Elements;

with Interfaces.C;
with Interfaces.C.Strings;

with XCB;
with XCB.XProto;

with Libc.Stdint;

with Linted.Channels;
with Linted.Env;
with Linted.Errors;
with Linted.GPU;
with Linted.KOs;
with Linted.Logs;
with Linted.Poller;
with Linted.Update_Reader;
with Linted.Update;
with Linted.Window_Notifier;

package body Linted.Drawer with
     Spark_Mode => Off is

   package Command_Line renames Ada.Command_Line;
   package STC renames Ada.Synchronous_Task_Control;
   package Storage_Elements renames System.Storage_Elements;
   package C renames Interfaces.C;
   package C_Strings renames Interfaces.C.Strings;

   use type C.size_t;
   use type Interfaces.Unsigned_32;
   use type Errors.Error;
   use type XCB.xcb_connection_t_access;
   use type C.unsigned_char;
   use type C.C_float;

   type Notifier_Event is record
      null;
   end record;

   package Notifier is new Window_Notifier.Worker;
   package My_Poller is new Poller.Worker;
   package My_Update_Reader is new Update_Reader.Worker;

   package Update_Event_Channels is new Channels (Update_Reader.Event);
   package Poller_Event_Channels is new Channels (Poller.Event);
   package Notifier_Event_Channels is new Channels (Notifier_Event);

   task A;
   task B;
   task Task_C;
   task Main_Task;

   Event_Trigger : STC.Suspension_Object;
   Update_Event_Channel : Update_Event_Channels.Channel;
   Poller_Event_Channel : Poller_Event_Channels.Channel;
   Notifier_Event_Channel : Notifier_Event_Channels.Channel;

   Window_Opts : aliased array (1 .. 2) of aliased Libc.Stdint.uint32_t :=
     (XCB.XProto.XCB_EVENT_MASK_STRUCTURE_NOTIFY, 0);

   function Read_Window (Object : KOs.KO) return GPU.X11_Window;

   function XCB_Conn_Error
     (Connection : XCB.xcb_connection_t_access) return Errors.Error;
   procedure Get_New_Window
     (Window_KO : KOs.KO;
      Connection : XCB.xcb_connection_t_access;
      Context : GPU.Context_Access;
      Window : out GPU.X11_Window);

   task body Main_Task is
      Err : Errors.Error;

      Window_KO : KOs.KO;
      Window_Notifier_KO : KOs.KO;
      Updater_KO : KOs.KO;

      Connection : XCB.xcb_connection_t_access;
      Context : GPU.Context_Access;
      Window : GPU.X11_Window;
   begin
      if Command_Line.Argument_Count < 3 then
         raise Constraint_Error with "At least three arguments";
      end if;

      declare
         Maybe_Window_KO : constant KOs.KO_Results.Result :=
           KOs.Open (Command_Line.Argument (1), KOs.Read_Write);
      begin
         if Maybe_Window_KO.Erroneous then
            raise Constraint_Error with "Erroneous window path";
         end if;
         Window_KO := Maybe_Window_KO.Data;
      end;

      declare
         Maybe_Window_Notifier_KO : constant KOs.KO_Results.Result :=
           KOs.Open (Command_Line.Argument (2), KOs.Read_Write);
      begin
         if Maybe_Window_Notifier_KO.Erroneous then
            raise Constraint_Error with "Erroneous window notifier path";
         end if;
         Window_Notifier_KO := Maybe_Window_Notifier_KO.Data;
      end;

      declare
         Maybe_Updater_KO : constant KOs.KO_Results.Result :=
           KOs.Open (Command_Line.Argument (3), KOs.Read_Write);
      begin
         if Maybe_Updater_KO.Erroneous then
            raise Constraint_Error with "Erroneous updater path";
         end if;
         Updater_KO := Maybe_Updater_KO.Data;
      end;

      declare
         Display : String := Env.Get ("DISPLAY");
         Display_C_Str : aliased C.char_array := C.To_C (Display);
      begin
         Logs.Log (Logs.Info, "Display = " & Display);
         Connection :=
           XCB.xcb_connect
             (C_Strings.To_Chars_Ptr (Display_C_Str'Unchecked_Access),
              null);
      end;
      if null = Connection then
         raise Storage_Error with "Cannot create XCB connection";
      end if;
      Err := XCB_Conn_Error (Connection);
      if Err /= Errors.Success then
         raise Program_Error with Errors.To_String (Err);
      end if;

      Err := GPU.Context_Create (Context);
      if Err /= Errors.Success then
         raise Program_Error with Errors.To_String (Err);
      end if;

      Notifier.Start (Window_Notifier_KO);

      My_Poller.Poll
        (KOs.KO (XCB.xcb_get_file_descriptor (Connection)),
         (Poller.Readable => True, Poller.Writable => False));
      My_Update_Reader.Start (Updater_KO);

      Get_New_Window (Window_KO, Connection, Context, Window);
      Logs.Log (Logs.Info, "Window: " & GPU.X11_Window'Image (Window));
      loop
         STC.Suspend_Until_True (Event_Trigger);

         declare
            Option_Event : Update_Event_Channels.Option_Element_Ts.Option;
         begin
            Update_Event_Channel.Poll (Option_Event);
            if not Option_Event.Empty then
               declare
                  GPU_Update : aliased GPU.Update;
               begin
                  GPU_Update.X_Position :=
                    C.C_float (Option_Event.Data.Data.X_Position) *
                    (1.0 / 4096.0);
                  GPU_Update.Y_Position :=
                    C.C_float (Option_Event.Data.Data.Y_Position) *
                    (1.0 / 4096.0);
                  GPU_Update.Z_Position :=
                    C.C_float (Option_Event.Data.Data.Z_Position) *
                    (1.0 / 4096.0);
                  GPU_Update.MX_Position :=
                    C.C_float (Option_Event.Data.Data.MX_Position) *
                    (1.0 / 4096.0);
                  GPU_Update.MY_Position :=
                    C.C_float (Option_Event.Data.Data.MY_Position) *
                    (1.0 / 4096.0);
                  GPU_Update.MZ_Position :=
                    C.C_float (Option_Event.Data.Data.MZ_Position) *
                    (1.0 / 4096.0);
                  GPU_Update.Z_Rotation :=
                    C.C_float (Option_Event.Data.Data.Z_Rotation) *
                    (6.28318530717958647692528 /
                     (C.C_float (Update.Nat'Last) + 1.0));
                  GPU_Update.X_Rotation :=
                    C.C_float (Option_Event.Data.Data.X_Rotation) *
                    (6.28318530717958647692528 /
                     (C.C_float (Update.Nat'Last) + 1.0));

                  GPU.Update_State (Context, GPU_Update'Unchecked_Access);
               end;
            end if;
         end;
         declare
            Option_Event : Notifier_Event_Channels.Option_Element_Ts.Option;
            Ck : XCB.xcb_void_cookie_t;
            X : C.int;
            No_Opts : aliased array (1 .. 2) of aliased Libc.Stdint.uint32_t :=
              (0, 0);
         begin
            Notifier_Event_Channel.Poll (Option_Event);
            if not Option_Event.Empty then
               Ck :=
                 XCB.XProto.xcb_change_window_attributes
                   (Connection,
                    XCB.XProto.xcb_window_t (Window),
                    Libc.Stdint.uint32_t (XCB.XProto.XCB_CW_EVENT_MASK),
                    No_Opts (1)'Unchecked_Access);

               Err := XCB_Conn_Error (Connection);
               if Err /= 0 then
                  raise Program_Error with Errors.To_String (Err);
               end if;

               X := XCB.xcb_flush (Connection);

               Get_New_Window (Window_KO, Connection, Context, Window);
               Logs.Log
                 (Logs.Info,
                  "Window: " & GPU.X11_Window'Image (Window));
               X := XCB.xcb_flush (Connection);
            end if;
         end;

         declare
            Option_Event : Poller_Event_Channels.Option_Element_Ts.Option;
            Window_Destroyed : Boolean := False;
            My_Event : access XCB.xcb_generic_event_t;

            procedure Free (Event : access XCB.xcb_generic_event_t);
            pragma Import (C, Free, "free");
         begin
            Poller_Event_Channel.Poll (Option_Event);
            if not Option_Event.Empty then
               loop
                  My_Event := XCB.xcb_poll_for_event (Connection);
                  if null = My_Event then
                     exit;
                  end if;

                  case My_Event.response_type and not 16#80# is
                     when XCB.XProto.XCB_CONFIGURE_NOTIFY =>
                        declare
                           type A is access all XCB.xcb_generic_event_t;
                           type B is
                             access all XCB.XProto
                               .xcb_configure_notify_event_t;
                           function Convert is new Ada.Unchecked_Conversion
                             (A,
                              B);
                           Configure : B := Convert (My_Event);
                        begin
                           GPU.Resize
                             (Context,
                              C.unsigned (Configure.width),
                              C.unsigned (Configure.height));
                        end;

                     when XCB.XProto.XCB_UNMAP_NOTIFY =>
                        GPU.Hide (Context);

                     when XCB.XProto.XCB_MAP_NOTIFY =>
                        GPU.Show (Context);

                     when XCB.XProto.XCB_DESTROY_NOTIFY =>
                        Window_Destroyed := True;

                     when others =>
                        -- Unknown event type, ignore it
                        null;
                  end case;
                  Free (My_Event);
               end loop;

               if Window_Destroyed then
                  Err := GPU.Remove_Window (Context);
                  if Err /= 0 then
                     raise Program_Error with Errors.To_String (Err);
                  end if;
               end if;

               declare
                  X : C.int;
               begin
                  X := XCB.xcb_flush (Connection);
               end;

               My_Poller.Poll
                 (KOs.KO (XCB.xcb_get_file_descriptor (Connection)),
                  (Poller.Readable => True, Poller.Writable => False));
            end if;
         end;
      end loop;
   end Main_Task;

   task body A is
   begin
      loop
         declare
            Event : Update_Reader.Event;
         begin
            My_Update_Reader.Wait (Event);
            Update_Event_Channel.Push (Event);
            STC.Set_True (Event_Trigger);
         end;
      end loop;
   end A;

   task body B is
   begin
      loop
         Notifier.Wait;
         declare
            Event : Notifier_Event;
         begin
            Notifier_Event_Channel.Push (Event);
         end;
         STC.Set_True (Event_Trigger);
      end loop;
   end B;

   task body Task_C is
   begin
      loop
         declare
            Event : Poller.Event;
         begin
            My_Poller.Wait (Event);
            Poller_Event_Channel.Push (Event);
         end;
         STC.Set_True (Event_Trigger);
      end loop;
   end Task_C;

   function Read_Window (Object : KOs.KO) return GPU.X11_Window is
      Bytes : C.size_t;
      Err : Errors.Error;

      type A is access all Storage_Elements.Storage_Element;
      function Convert is new Ada.Unchecked_Conversion (A, System.Address);

      Data : Storage_Elements.Storage_Array (1 .. 4);
   begin
      declare
         Buf : aliased Storage_Elements.Storage_Array (1 .. 4);
      begin
         Err :=
           KOs.Pread (Object, Convert (Buf (1)'Unchecked_Access), 4, 0, Bytes);
         if Err /= 0 then
            raise Program_Error with Errors.To_String (Err);
         end if;
         Data := Buf;
      end;
      pragma Assert (Bytes = 4);
      return GPU.X11_Window
          (Interfaces.Unsigned_32 (Data (1)) or
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (Data (2)), 8) or
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (Data (3)), 16) or
           Interfaces.Shift_Left (Interfaces.Unsigned_32 (Data (4)), 24));
   end Read_Window;

   function XCB_Conn_Error
     (Connection : XCB.xcb_connection_t_access) return Errors.Error is
     (case XCB.xcb_connection_has_error (Connection) is
        when 0 => Errors.Success,
        when XCB.XCB_CONN_ERROR => Errors.Protocol,
        when XCB.XCB_CONN_CLOSED_EXT_NOTSUPPORTED => Errors.Unimplemented,
        when XCB.XCB_CONN_CLOSED_MEM_INSUFFICIENT => Errors.Out_Of_Memory,
        when XCB.XCB_CONN_CLOSED_REQ_LEN_EXCEED => Errors.Invalid_Parameter,
        when XCB.XCB_CONN_CLOSED_PARSE_ERR => Errors.Invalid_Parameter,
        when others => raise Constraint_Error);

   procedure Get_New_Window
     (Window_KO : KOs.KO;
      Connection : XCB.xcb_connection_t_access;
      Context : GPU.Context_Access;
      Window : out GPU.X11_Window)
   is
      New_Window : GPU.X11_Window;
      Geom_CK : XCB.XProto.xcb_get_geometry_cookie_t;
      Width : C.unsigned;
      Height : C.unsigned;
      X : C.int;
      Ck : XCB.xcb_void_cookie_t;
      Err : Errors.Error;
   begin
      New_Window := Read_Window (Window_KO);

      Ck :=
        XCB.XProto.xcb_change_window_attributes_checked
          (Connection,
           XCB.XProto.xcb_window_t (New_Window),
           Libc.Stdint.uint32_t (XCB.XProto.XCB_CW_EVENT_MASK),
           Window_Opts (1)'Unchecked_Access);
      Err := XCB_Conn_Error (Connection);
      if Err /= 0 then
         raise Program_Error with Errors.To_String (Err);
      end if;

      Geom_CK :=
        XCB.XProto.xcb_get_geometry
          (Connection,
           XCB.XProto.xcb_window_t (New_Window));
      Err := XCB_Conn_Error (Connection);
      if Err /= 0 then
         raise Program_Error with Errors.To_String (Err);
      end if;

      declare
         Error : aliased access XCB.xcb_generic_error_t;
         Reply : access XCB.XProto.xcb_get_geometry_reply_t;

         procedure Free (Event : access XCB.XProto.xcb_get_geometry_reply_t);
         pragma Import (C, Free, "free");

      begin
         Reply :=
           XCB.XProto.xcb_get_geometry_reply
             (Connection,
              Geom_CK,
              Error'Unchecked_Access);

         Err := XCB_Conn_Error (Connection);
         if Err /= 0 then
            raise Program_Error with Errors.To_String (Err);
         end if;

         if Error /= null then
            raise Program_Error;
         end if;

         Width := C.unsigned (Reply.width);
         Height := C.unsigned (Reply.height);

         Free (Reply);
      end;

      X := XCB.xcb_flush (Connection);

      Err := GPU.Remove_Window (Context);
      if Err /= 0 then
         raise Program_Error with Errors.To_String (Err);
      end if;

      Err := GPU.Set_X11_Window (Context, New_Window);
      if Err /= 0 then
         raise Program_Error with Errors.To_String (Err);
      end if;

      GPU.Resize (Context, Width, Height);
      if Err /= 0 then
         raise Program_Error with Errors.To_String (Err);
      end if;

      Window := New_Window;
   end Get_New_Window;

end Linted.Drawer;
