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
with System;

with Ada.Unchecked_Conversion;
with Ada.Numerics.Elementary_Functions;

private with Interfaces.C.Strings;
private with Interfaces.C;

private with Pulse.Context;
private with Pulse.Def;
private with Pulse.Error;
private with Pulse.Mainloop;
private with Pulse.Mainloop.API;
private with Pulse.Proplist;
private with Pulse.Sample;
private with Pulse.Stream;

private with Linted.Errors;
private with Linted.KOs;
private with Linted.Stdio;
private with Libc.Stddef;

package body Linted.Audio with
     Spark_Mode => Off is
   use Interfaces.C.Strings;
   use Interfaces.C;

   use Pulse.Context;
   use Pulse.Def;
   use Pulse.Error;
   use Pulse.Mainloop;
   use Pulse.Mainloop.API;
   use Pulse.Proplist;
   use Pulse.Sample;
   use Pulse.Stream;

   use type Errors.Error;

   A_TONE : constant Float := 440.0;
   SAMPLE_RATE : constant Float := 44100.0;
   GREATEST_PERIOD : constant Float := 1000.0 * (SAMPLE_RATE / A_TONE);

   Sampledata : array (0 .. Integer (GREATEST_PERIOD) - 1) of aliased short;

   Test_Sample_Spec : aliased constant pa_sample_spec :=
     (format => PA_SAMPLE_S16LE,
      rate => unsigned (SAMPLE_RATE),
      channels => 1);

   function Square_Wave
     (II : Integer;
      Freq : Float;
      Amplitude : Float;
      Sample : Float) return Float;
   function Triangle_Wave
     (II : Integer;
      Freq : Float;
      Amplitude : Float;
      Sample : Float) return Float;
   function Sin_Wave
     (II : Integer;
      Freq : Float;
      Amplitude : Float;
      Sample : Float) return Float;

   procedure On_Notify (c : pa_context_access; userdata : System.Address);
   pragma Convention (C, On_Notify);

   task Main_Task;
   task body Main_Task is
      Mainloop : pa_mainloop_access;
      Context : pa_context_access;
      Retval : int;
   begin
      for II in Sampledata'Range loop
         Sampledata (II) :=
           short
             (Triangle_Wave (II, A_TONE, 8000.0, SAMPLE_RATE) *
              Square_Wave (II, A_TONE / 100.0, 1.0, SAMPLE_RATE) *
              Sin_Wave (II, A_TONE / 1000.0, 1.0, SAMPLE_RATE));
      end loop;

      Mainloop := pa_mainloop_new;
      if null = Mainloop then
         raise Storage_Error with "Cannot create a mainloop";
      end if;

      declare
         Proplist : pa_proplist_access;
         Role_Str : chars_ptr;
         Role : chars_ptr;
         Game_Name : chars_ptr;
      begin
         Proplist := pa_proplist_new;
         if null = Proplist then
            raise Storage_Error with "Cannot create a property list";
         end if;

         Role_Str := New_String ("PULSE_PROP_media.role");
         Role := New_String ("game");

         if 0 < pa_proplist_sets (Proplist, Role, Role_Str) then
            raise Storage_Error with "Cannot set property list values";
         end if;

         Free (Role);
         Free (Role_Str);

         Game_Name := New_String ("Linted");

         Context :=
           pa_context_new_with_proplist
             (pa_mainloop_get_api (Mainloop),
              Game_Name,
              Proplist);
         if null = Context then
            raise Storage_Error with "Cannot create a new context";
         end if;

         Free (Game_Name);

         pa_proplist_free (Proplist);
      end;

      pa_context_set_state_callback
        (Context,
         On_Notify'Access,
         System.Null_Address);

      if pa_context_connect (Context, Null_Ptr, PA_CONTEXT_NOAUTOSPAWN, null) <
        0
      then
         raise Program_Error
           with Value (pa_strerror (pa_context_errno (Context)));
      end if;

      if pa_mainloop_run (Mainloop, Retval) < 0 then
         raise Program_Error
           with Value (pa_strerror (pa_context_errno (Context)));
      end if;

      pa_context_unref (Context);

      pa_mainloop_free (Mainloop);
   end Main_Task;

   function Square_Wave
     (II : Integer;
      Freq : Float;
      Amplitude : Float;
      Sample : Float) return Float
   is
      Frequency : constant Float := Freq / Sample;
      Period : constant Float := 1.0 / Frequency;
   begin
      return Amplitude *
        ((Float (II mod Integer (Period)) + (Period / 2.0)) / Period);
   end Square_Wave;

   function Triangle_Wave
     (II : Integer;
      Freq : Float;
      Amplitude : Float;
      Sample : Float) return Float
   is
      Frequency : constant Float := Freq / Sample;
      Period : constant Float := 1.0 / Frequency;
   begin

      return Amplitude * Float (II mod Integer (Period)) / Period;
   end Triangle_Wave;

   function Sin_Wave
     (II : Integer;
      Freq : Float;
      Amplitude : Float;
      Sample : Float) return Float
   is
      Frequency : constant Float := Freq / Sample;
      Tau : constant Float := 6.28318530718;
   begin
      return Amplitude *
        Ada.Numerics.Elementary_Functions.Sin (Tau * Frequency * Float (II));
   end Sin_Wave;

   procedure On_Ready (c : pa_context_access);

   procedure On_Notify (c : pa_context_access; userdata : System.Address) is
   begin
      case pa_context_get_state (c) is
         when PA_CONTEXT_UNCONNECTED =>
            Stdio.Write_Line (KOs.Standard_Error, "Unconnected");
         when PA_CONTEXT_CONNECTING =>
            Stdio.Write_Line (KOs.Standard_Error, "Connecting");
         when PA_CONTEXT_AUTHORIZING =>
            Stdio.Write_Line (KOs.Standard_Error, "Authorizing");
         when PA_CONTEXT_SETTING_NAME =>
            Stdio.Write_Line (KOs.Standard_Error, "Setting Name");
         when PA_CONTEXT_READY =>
            Stdio.Write_Line (KOs.Standard_Error, "Ready");
            On_Ready (c);
         when PA_CONTEXT_FAILED =>
            Stdio.Write_Line (KOs.Standard_Error, "Failed");
         when PA_CONTEXT_TERMINATED =>
            Stdio.Write_Line (KOs.Standard_Error, "Terminated");
      end case;
   end On_Notify;

   procedure On_Ok_To_Write
     (s : pa_stream_access;
      nbytes : Libc.Stddef.size_t;
      userdata : System.Address);
   pragma Convention (C, On_Ok_To_Write);

   procedure On_Ready (c : pa_context_access) is
      Stream : pa_stream_access;

      Buffer_Attr : aliased pa_buffer_attr;
      Latency : constant unsigned_long := 20000;

      Stream_Name : chars_ptr;
   begin
      Stream_Name := New_String ("Background Music");

      Stream := pa_stream_new (c, Stream_Name, Test_Sample_Spec'Access, null);

      Free (Stream_Name);

      pa_stream_set_write_callback
        (Stream,
         On_Ok_To_Write'Access,
         System.Null_Address);

      Buffer_Attr.maxlength :=
        unsigned (pa_usec_to_bytes (Latency, Test_Sample_Spec'Access));
      Buffer_Attr.minreq :=
        unsigned (pa_usec_to_bytes (0, Test_Sample_Spec'Access));
      Buffer_Attr.prebuf := -1;
      Buffer_Attr.tlength :=
        unsigned (pa_usec_to_bytes (Latency, Test_Sample_Spec'Access));

      if pa_stream_connect_playback
          (Stream,
           Null_Ptr,
           Buffer_Attr'Access,
           PA_STREAM_INTERPOLATE_TIMING or
           PA_STREAM_ADJUST_LATENCY or
           PA_STREAM_AUTO_TIMING_UPDATE,
           null,
           System.Null_Address) <
        0
      then
         raise Program_Error
           with "pa_stream_connect_playback: " &
           Value (pa_strerror (pa_context_errno (c)));
      end if;
   end On_Ready;

   Sampleoffs : Libc.Stddef.size_t := 0;
   procedure On_Ok_To_Write
     (s : pa_stream_access;
      nbytes : Libc.Stddef.size_t;
      userdata : System.Address)
   is
      Mybytes : Libc.Stddef.size_t := nbytes;
   begin
      if Sampleoffs * 2 + Mybytes >
        Libc.Stddef.size_t (GREATEST_PERIOD) * 2
      then
         Sampleoffs := 0;
      end if;

      if Mybytes > Libc.Stddef.size_t (GREATEST_PERIOD) * 2 then
         Mybytes := Libc.Stddef.size_t (GREATEST_PERIOD) * 2;
      end if;

      if pa_stream_write
          (s,
           Sampledata (Integer (Sampleoffs))'Address,
           Mybytes,
           null,
           0,
           PA_SEEK_RELATIVE) <
        0
      then
         raise Program_Error with "pa_stream_write";
      end if;

      Sampleoffs := Sampleoffs + nbytes / 2;
   end On_Ok_To_Write;
end Linted.Audio;
