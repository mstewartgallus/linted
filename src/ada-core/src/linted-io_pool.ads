-- Copyright 2015,2016,2017 Steven Stewart-Gallus
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
with Ada.Real_Time;

with Interfaces.C;
with System;

with Linted.Errors;
with Linted.KOs;
with Linted.Triggers;
with Linted.Wait_Lists;
pragma Elaborate_All (Linted.Wait_Lists);

package Linted.IO_Pool with
     Spark_Mode,
     Initializes => (State => Wait_Lists.State),
     Abstract_State => (State with External) is
   pragma Elaborate_Body;
   use type Interfaces.C.int;

   type Writer_Event is record
      Bytes_Written : Interfaces.C.size_t := 0;
      Err : Errors.Error;
   end record;

   type Reader_Event is record
      Bytes_Read : Interfaces.C.size_t := 0;
      Err : Errors.Error;
   end record;

   type Poller_Event_Type is (Readable, Writable);
   type Poller_Event_Set is array (Poller_Event_Type) of Boolean with
        Pack;
   type Poller_Event is record
      Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Err : Errors.Error;
   end record;

   type Remind_Me_Event is record
      Dummy : Natural := 0;
   end record;

   type Read_Future is limited private with
      Preelaborable_Initialization;

   function Read_Future_Is_Live (Future : Read_Future) return Boolean with
      Ghost,
      Global => null,
      Depends => (Read_Future_Is_Live'Result => Future);

   procedure Read
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Read_Future) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (State =>+ (Buf, Count, Object, Signaller, Wait_Lists.State),
       Future => State,
       Wait_Lists.State =>+ (Buf, Count, State, Object, Signaller)),
      Post => Read_Future_Is_Live (Future);

   procedure Read_Wait
     (Future : in out Read_Future;
      Event : out Reader_Event) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => null,
       Event => (State, Future),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Read_Future_Is_Live (Future),
      Post => not Read_Future_Is_Live (Future);

   procedure Read_Poll
     (Future : in out Read_Future;
      Event : out Reader_Event;
      Init : out Boolean) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future =>+ State,
       Event => (State, Future),
       Init => (State, Future),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Read_Future_Is_Live (Future),
      Post =>
      (if Init then not Read_Future_Is_Live (Future)
       else Read_Future_Is_Live (Future));

   type Write_Future is limited private with
      Preelaborable_Initialization;

   function Write_Future_Is_Live (Future : Write_Future) return Boolean with
      Ghost,
      Global => null,
      Depends => (Write_Future_Is_Live'Result => Future);

   procedure Write
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Write_Future) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => State,
       State =>+ (Buf, Count, Object, Signaller, Wait_Lists.State),
       Wait_Lists.State =>+ (Buf, Count, State, Object, Signaller)),
      Post => Write_Future_Is_Live (Future);

   procedure Write_Wait
     (Future : in out Write_Future;
      Event : out Writer_Event) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => null,
       Event => (State, Future),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Write_Future_Is_Live (Future),
      Post => not Write_Future_Is_Live (Future);

   procedure Write_Poll
     (Future : in out Write_Future;
      Event : out Writer_Event;
      Init : out Boolean) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future =>+ State,
       Event => (State, Future),
       Init => (State, Future),
       State =>+ (Future, State, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Write_Future_Is_Live (Future),
      Post =>
      (if Init then not Write_Future_Is_Live (Future)
       else Write_Future_Is_Live (Future));

   type Poll_Future is limited private with
      Preelaborable_Initialization;

   function Poll_Future_Is_Live (Future : Poll_Future) return Boolean with
      Ghost,
      Global => null,
      Depends => (Poll_Future_Is_Live'Result => Future);

   procedure Poll
     (Object : KOs.KO;
      Events : Poller_Event_Set;
      Signaller : Triggers.Signaller;
      Future : out Poll_Future) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => State,
       State =>+ (Events, State, Object, Signaller, Wait_Lists.State),
       Wait_Lists.State =>+ (Events, State, Object, Signaller)),
      Post => Poll_Future_Is_Live (Future);

   procedure Poll_Wait
     (Future : in out Poll_Future;
      Event : out Poller_Event) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => null,
       Event => (State, Future),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Poll_Future_Is_Live (Future),
      Post => not Poll_Future_Is_Live (Future);

   procedure Poll_Poll
     (Future : in out Poll_Future;
      Event : out Poller_Event;
      Init : out Boolean) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future =>+ State,
       Event => (State, Future),
       Init => (State, Future),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Poll_Future_Is_Live (Future),
      Post =>
      (if Init then not Poll_Future_Is_Live (Future)
       else Poll_Future_Is_Live (Future));

   type Remind_Me_Future is limited private with
      Preelaborable_Initialization;

   function Remind_Me_Future_Is_Live
     (Future : Remind_Me_Future) return Boolean with
      Ghost,
      Global => null,
      Depends => (Remind_Me_Future_Is_Live'Result => Future);

   procedure Remind_Me
     (Time : Ada.Real_Time.Time;
      Signaller : Triggers.Signaller;
      Future : out Remind_Me_Future) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => State,
       State =>+ (Signaller, Time, Wait_Lists.State),
       Wait_Lists.State =>+ (State, Signaller, Time)),
      Post => Remind_Me_Future_Is_Live (Future);

   procedure Remind_Me_Wait
     (Future : in out Remind_Me_Future;
      Event : out Remind_Me_Event) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future => null,
       Event => (Future, State),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Remind_Me_Future_Is_Live (Future),
      Post => not Remind_Me_Future_Is_Live (Future);

   procedure Remind_Me_Poll
     (Future : in out Remind_Me_Future;
      Event : out Remind_Me_Event;
      Init : out Boolean) with
      Global => (In_Out => (State, Wait_Lists.State)),
      Depends =>
      (Future =>+ State,
       Event => (Future, State),
       Init => (State, Future),
       State =>+ (Future, Wait_Lists.State),
       Wait_Lists.State =>+ (Future, State)),
      Pre => Remind_Me_Future_Is_Live (Future),
      Post =>
      (if Init then not Remind_Me_Future_Is_Live (Future)
       else Remind_Me_Future_Is_Live (Future));

private
   Max_Read_Futures : constant := 10;
   Max_Write_Futures : constant := 10;
   Max_Poll_Futures : constant := 10;
   Max_Remind_Me_Futures : constant := 10;
   Max_Command_Queue_Capacity : constant := 10;

   type Read_Future is mod Max_Read_Futures + 1 with
        Default_Value => 0;
   type Write_Future is mod Max_Write_Futures + 1 with
        Default_Value => 0;
   type Poll_Future is mod Max_Poll_Futures + 1 with
        Default_Value => 0;
   type Remind_Me_Future is mod Max_Remind_Me_Futures + 1 with
        Default_Value => 0;
end Linted.IO_Pool;
