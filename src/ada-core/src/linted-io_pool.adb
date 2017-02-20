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
with Libc.Errno;
with Libc.Errno.POSIX_2008;
with Libc.Sys.Poll;
with Libc.Sys.Types;
with Libc.Unistd;

with Linted.Channels;
with Linted.Queue;
with Linted.Stack;
with Linted.Last_Chance;

package body Linted.IO_Pool with
  Refined_State => (Command_Queue => (My_Command_Queue.State),
		    Event_Queue => (Read_Future_Channels,
				    Write_Future_Channels,
				    Poll_Future_Channels,
				    Remind_Me_Future_Channels),
		    Various => Worker_Tasks,
		    Future_Pool => (Spare_Read_Futures.State, Spare_Write_Futures.State, Spare_Poll_Futures.State, Spare_Remind_Me_Futures.State))
is
   package C renames Interfaces.C;

   package Errno renames Libc.Errno;

   use type Libc.Sys.Types.ssize_t;
   use type C.size_t;
   use type Interfaces.C.unsigned;

   type Live_Read_Future is new Read_Future range 1 .. Read_Future'Last with
     Default_Value => 1;
   type Live_Write_Future is new Write_Future range 1 .. Write_Future'Last with
     Default_Value => 1;
   type Live_Poll_Future is new Poll_Future range 1 .. Poll_Future'Last with
     Default_Value => 1;
   type Live_Remind_Me_Future is new Remind_Me_Future range 1 .. Remind_Me_Future'Last with
     Default_Value => 1;

   type Write_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : Live_Write_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Read_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : Live_Read_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Poller_Command is record
      Object : KOs.KO;
      Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : Live_Poll_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Remind_Me_Command is record
      Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      Replier : Live_Remind_Me_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Command_Type is (Invalid_Type, Write_Type, Read_Type, Poll_Type, Remind_Me_Type);

   type Command (T : Command_Type := Invalid_Type) is record
      case T is
         when Invalid_Type =>
            null;
         when Write_Type =>
            Write_Object : Write_Command;
         when Read_Type =>
            Read_Object : Read_Command;
         when Poll_Type =>
            Poll_Object : Poller_Command;
         when Remind_Me_Type =>
            Remind_Me_Object : Remind_Me_Command;
      end case;
   end record;

   --  We have to wrap the type because subtyping breaks out
   --  parameters.  Imagine writing to an out parameter of type
   --  Command that is passed a Command (Write_Type).
   type Any_Command is record
      C : Command;
   end record;

   type Read_Ix is mod Max_Read_Futures + 2;
   type Write_Ix is mod Max_Write_Futures + 2;
   type Poll_Ix is mod Max_Poll_Futures + 2;
   type Remind_Me_Ix is mod Max_Remind_Me_Futures + 2;
   type Command_Ix is mod Max_Command_Queue_Capacity + 2;

   function Read_Is_Valid (Element : Live_Read_Future) return Boolean is (True);
   function Write_Is_Valid (Element : Live_Write_Future) return Boolean is (True);
   function Poll_Is_Valid (Element : Live_Poll_Future) return Boolean is (True);
   function Remind_Me_Is_Valid (Element : Live_Remind_Me_Future) return Boolean is (True);
   function Command_Is_Valid (Element : Any_Command) return Boolean is (Element.C.T /= Invalid_Type);

   package Spare_Read_Futures is new Stack (Live_Read_Future, Read_Ix, Read_Is_Valid);
   package Spare_Write_Futures is new Stack (Live_Write_Future, Write_Ix, Write_Is_Valid);
   package Spare_Poll_Futures is new Stack (Live_Poll_Future, Poll_Ix, Poll_Is_Valid);
   package Spare_Remind_Me_Futures is new Stack (Live_Remind_Me_Future, Remind_Me_Ix, Remind_Me_Is_Valid);
   package My_Command_Queue is new Queue (Any_Command, Command_Ix, Command_Is_Valid);

   task type Worker_Task with Global => (Input => Ada.Real_Time.Clock_Time,
					 In_Out => (My_Command_Queue.State,
   						    Read_Future_Channels,
   						    Write_Future_Channels,
   						    Poll_Future_Channels,
						    Remind_Me_Future_Channels,
						    Wait_Lists.State
   						   )),
     Spark_Mode,
     Depends => (My_Command_Queue.State =>+ Wait_Lists.State,
   		 Read_Future_Channels =>+ (Wait_Lists.State, My_Command_Queue.State),
   		 Write_Future_Channels =>+ (Wait_Lists.State, My_Command_Queue.State),
   		 Poll_Future_Channels =>+ (Wait_Lists.State, My_Command_Queue.State),
		 Remind_Me_Future_Channels =>+ (Wait_Lists.State, My_Command_Queue.State),
		 Wait_Lists.State =>+ My_Command_Queue.State,
   		 Worker_Task'Result => Worker_Task,
   		 Worker_Task => null,
		 null => Ada.Real_Time.Clock_Time
   		);

   type Worker_Array is array (1 .. 16) of Worker_Task;

   Worker_Tasks : Worker_Array;

   package Writer_Event_Channels is new Channels (Writer_Event);
   package Reader_Event_Channels is new Channels (Reader_Event);
   package Poller_Event_Channels is new Channels (Poller_Event);
   package Remind_Me_Event_Channels is new Channels (Remind_Me_Event);

   type Read_Future_Channels_Array is
     array (Live_Read_Future) of Reader_Event_Channels.Channel;
   type Write_Future_Channels_Array is
     array (Live_Write_Future) of Writer_Event_Channels.Channel;
   type Poller_Future_Channels_Array is
     array (Live_Poll_Future) of Poller_Event_Channels.Channel;
   type Remind_Me_Future_Channels_Array is
     array (Live_Remind_Me_Future) of Remind_Me_Event_Channels.Channel;

   Read_Future_Channels : Read_Future_Channels_Array;
   Write_Future_Channels : Write_Future_Channels_Array;
   Poll_Future_Channels : Poller_Future_Channels_Array;
   Remind_Me_Future_Channels : Remind_Me_Future_Channels_Array;

   procedure Do_Work with Spark_Mode,
     Global => (In_Out => (My_Command_Queue.State,
			   Read_Future_Channels,
			   Write_Future_Channels,
			   Poll_Future_Channels,
			   Remind_Me_Future_Channels,
			   Wait_Lists.State
			  ),
	       Input => Ada.Real_Time.Clock_Time),
     Depends => (My_Command_Queue.State =>+ Wait_Lists.State,
   		 Read_Future_Channels =>+ (My_Command_Queue.State, Wait_Lists.State),
   		 Write_Future_Channels =>+ (My_Command_Queue.State,Wait_Lists.State),
   		 Poll_Future_Channels =>+ (My_Command_Queue.State,Wait_Lists.State),
		 Remind_Me_Future_Channels =>+ (My_Command_Queue.State,Wait_Lists.State),
		 Wait_Lists.State =>+ My_Command_Queue.State,
		null => Ada.Real_Time.Clock_Time);

   procedure Do_Write (W : Write_Command) with
     Global => (In_Out => Write_Future_Channels),
      Depends => (Write_Future_Channels =>+ W);
   procedure Do_Read (R : Read_Command) with
      Global => (In_Out => Read_Future_Channels),
      Depends => (Read_Future_Channels =>+ R);
   procedure Do_Poll (P : Poller_Command) with
      Global => (In_Out => Poll_Future_Channels),
      Depends => (Poll_Future_Channels =>+ P);
   procedure Do_Remind_Me (R : Remind_Me_Command) with
      Global => (In_Out => Remind_Me_Future_Channels, Input => Ada.Real_Time.Clock_Time),
     Depends => (Remind_Me_Future_Channels =>+ R,
					      null => Ada.Real_Time.Clock_Time);

   procedure Read
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Read_Future) with
     Refined_Global => (
			In_Out => (Spare_Read_Futures.State,
				   My_Command_Queue.State,
				   Wait_Lists.State))
   is
      Live : Live_Read_Future;
   begin
      Spare_Read_Futures.Pop (Live);
      My_Command_Queue.Enqueue (Any_Command'(C => (Read_Type,
      				 (Object,
      				  Buf,
      				  Count,
      				  Live,
      				  Signaller))));
      Future := Read_Future (Live);
   end Read;

   procedure Read_Wait
     (Future : in out Read_Future;
      Event : out Reader_Event) with
     Refined_Global => (
			In_Out => (Spare_Read_Futures.State, Read_Future_Channels,
				   Wait_Lists.State))
   is
      Live : Live_Read_Future := Live_Read_Future (Future);
   begin
      Reader_Event_Channels.Pop (Read_Future_Channels (Live), Event);
      Spare_Read_Futures.Push (Live);
      Future := 0;
   end Read_Wait;

   procedure Read_Poll
     (Future : in out Read_Future;
      Event : out Reader_Event;
      Init : out Boolean) with
     Refined_Global => (
			In_Out => (Spare_Read_Futures.State, Read_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Read_Future := Live_Read_Future (Future);
   begin
      Reader_Event_Channels.Poll (Read_Future_Channels (Live), Event, Init);
      if Init then
	 Spare_Read_Futures.Push (Live);
         Future := 0;
      end if;
   end Read_Poll;

   procedure Write
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Write_Future) with
     Refined_Global => (
			In_Out => (Spare_Write_Futures.State, My_Command_Queue.State,
				  Wait_Lists.State))
   is
      Live : Live_Write_Future;
   begin
      Spare_Write_Futures.Pop (Live);
      My_Command_Queue.Enqueue (Any_Command'(C => (Write_Type, (Object, Buf, Count, Live, Signaller))));
      Future := Write_Future (Live);
   end Write;

   procedure Write_Wait
     (Future : in out Write_Future;
      Event : out Writer_Event) with
     Refined_Global => (
			In_Out => (Spare_Write_Futures.State, Write_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Write_Future := Live_Write_Future (Future);
   begin
      Writer_Event_Channels.Pop (Write_Future_Channels (Live), Event);
      Spare_Write_Futures.Push (Live);
      Future := 0;
   end Write_Wait;

   procedure Write_Poll
     (Future : in out Write_Future;
      Event : out Writer_Event;
      Init : out Boolean) with
     Refined_Global => (
			In_Out => (Spare_Write_Futures.State, Write_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Write_Future := Live_Write_Future (Future);
   begin
      Writer_Event_Channels.Poll (Write_Future_Channels (Live), Event, Init);
      if Init then
	 Spare_Write_Futures.Push (Live);
         Future := 0;
      end if;
   end Write_Poll;

   procedure Poll
     (Object : KOs.KO;
      Events : Poller_Event_Set;
      Signaller : Triggers.Signaller;
      Future : out Poll_Future) with
     Refined_Global => (
			In_Out => (Spare_Poll_Futures.State, My_Command_Queue.State,
				  Wait_Lists.State))
   is
      Live : Live_Poll_Future;
   begin
      Spare_Poll_Futures.Pop (Live);
      My_Command_Queue.Enqueue (Any_Command'(C => (Poll_Type, (Object, Events, Live, Signaller))));
      Future := Poll_Future (Live);
   end Poll;

   procedure Poll_Wait
     (Future : in out Poll_Future;
      Event : out Poller_Event) with
     Refined_Global => (
			In_Out => (Spare_Poll_Futures.State, Poll_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Poll_Future := Live_Poll_Future (Future);
   begin
      Poller_Event_Channels.Pop (Poll_Future_Channels (Live), Event);
      Spare_Poll_Futures.Push (Live);
      Future := 0;
   end Poll_Wait;

   procedure Poll_Poll
     (Future : in out Poll_Future;
      Event : out Poller_Event;
      Init : out Boolean) with
     Refined_Global => (In_Out => (Spare_Poll_Futures.State, Poll_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Poll_Future := Live_Poll_Future (Future);
   begin
      Poller_Event_Channels.Poll (Poll_Future_Channels (Live), Event, Init);
      if Init then
	 Spare_Poll_Futures.Push (Live);
         Future := 0;
      end if;
   end Poll_Poll;

   procedure Remind_Me
     (Time : Ada.Real_Time.Time;
      Signaller : Triggers.Signaller;
      Future : out Remind_Me_Future) with
     Refined_Global => (
			In_Out => (Spare_Remind_Me_Futures.State, My_Command_Queue.State,
				  Wait_Lists.State))
   is
      Live : Live_Remind_Me_Future;
   begin
      Spare_Remind_Me_Futures.Pop (Live);
      My_Command_Queue.Enqueue (Any_Command'(C => (Remind_Me_Type, (Time, Live, Signaller))));
      Future := Remind_Me_Future (Live);
   end Remind_Me;

   procedure Remind_Me_Wait
     (Future : in out Remind_Me_Future;
      Event : out Remind_Me_Event) with
     Refined_Global => (
			In_Out => (Spare_Remind_Me_Futures.State, Remind_Me_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Remind_Me_Future := Live_Remind_Me_Future (Future);
   begin
      Remind_Me_Event_Channels.Pop (Remind_Me_Future_Channels (Live), Event);
      Spare_Remind_Me_Futures.Push (Live);
      Future := 0;
   end Remind_Me_Wait;

   procedure Remind_Me_Poll
     (Future : in out Remind_Me_Future;
      Event : out Remind_Me_Event;
      Init : out Boolean) with
     Refined_Global => (
			In_Out => (Spare_Remind_Me_Futures.State, Remind_Me_Future_Channels,
				  Wait_Lists.State))
   is
      Live : Live_Remind_Me_Future := Live_Remind_Me_Future (Future);
   begin
      Remind_Me_Event_Channels.Poll (Remind_Me_Future_Channels (Live), Event, Init);
      if Init then
	 Spare_Remind_Me_Futures.Push (Live);
         Future := 0;
      end if;
   end Remind_Me_Poll;

   procedure Do_Write (W : Write_Command) with Spark_Mode => Off is
      Err : C.int;
      Bytes_Written : C.size_t := 0;

      Object : constant KOs.KO := W.Object;
      Buf : constant System.Address := W.Buf;
      Count : constant C.size_t := W.Count;
      Replier : constant Live_Write_Future := W.Replier;
      Signaller : constant Triggers.Signaller := W.Signaller;

      Result : Libc.Sys.Types.ssize_t;
   begin
      loop
         Result :=
           Libc.Unistd.write (C.int (Object), Buf, Count - Bytes_Written);
         if Result < 0 then
            Errno.Errno_Get (Err);
            exit when Err /= Libc.Errno.POSIX_2008.EINTR;
         else
            Err := 0;
            Bytes_Written := Bytes_Written + C.size_t (Result);
            exit when Bytes_Written = Count;
         end if;
      end loop;

      Writer_Event_Channels.Push
        (Write_Future_Channels (Replier),
         (Err => Errors.Error (Err), Bytes_Written => Bytes_Written));
      if not Triggers.Is_Null_Signaller (Signaller) then
         Triggers.Broadcast (Signaller);
      end if;
   end Do_Write;

   procedure Do_Read (R : Read_Command) with
      Spark_Mode => Off is
      Err : C.int;
      Bytes_Read : C.size_t := 0;

      Object : constant KOs.KO := R.Object;
      Buf : constant System.Address := R.Buf;
      Count : constant C.size_t := R.Count;
      Replier : constant Live_Read_Future := R.Replier;
      Signaller : constant Triggers.Signaller := R.Signaller;

      Result : Libc.Sys.Types.ssize_t;
   begin
      loop
         Result := Libc.Unistd.read (C.int (Object), Buf, Count - Bytes_Read);
         if Result < 0 then
            Errno.Errno_Get (Err);
            exit when Err /= Libc.Errno.POSIX_2008.EINTR;
         elsif 0 = Result then
            Err := 0;
            exit;
         else
            Err := 0;
            Bytes_Read := Bytes_Read + C.size_t (Result);
            exit when Bytes_Read = Count;
         end if;
      end loop;

      Reader_Event_Channels.Push
        (Read_Future_Channels (Replier),
         (Err => Errors.Error (Err), Bytes_Read => Bytes_Read));
      if not Triggers.Is_Null_Signaller (Signaller) then
         Triggers.Broadcast (Signaller);
      end if;
   end Do_Read;

   procedure Do_Poll (P : Poller_Command) with
      Spark_Mode => Off is
      Err : C.int;

      Object : constant KOs.KO := P.Object;
      Listen_Events : constant Poller_Event_Set := P.Events;
      Heard_Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : constant Live_Poll_Future := P.Replier;
      Signaller : constant Triggers.Signaller := P.Signaller;

      Nfds : C.int;
   begin
      loop
         declare
            Pollfd : Libc.Sys.Poll.pollfd :=
              (Interfaces.C.int (Object), 0, 0);
         begin
            if Listen_Events (Readable) then
               Pollfd.events :=
                 Interfaces.C.short
                   (Interfaces.C.unsigned (Pollfd.events) or
                    Libc.Sys.Poll.POLLIN);
            end if;
            if Listen_Events (Writable) then
               Pollfd.events :=
                 Interfaces.C.short
                   (Interfaces.C.unsigned (Pollfd.events) or
                    Libc.Sys.Poll.POLLOUT);
            end if;

            Nfds := Libc.Sys.Poll.poll (Pollfd, 1, -1);

            if
              (Interfaces.C.unsigned (Pollfd.events) and
               Libc.Sys.Poll.POLLIN) >
              0
            then
               Heard_Events (Readable) := True;
            end if;
            if
              (Interfaces.C.unsigned (Pollfd.events) and
               Libc.Sys.Poll.POLLOUT) >
              0
            then
               Heard_Events (Writable) := True;
            end if;
         end;
         if Nfds < 0 then
            Errno.Errno_Get (Err);
            exit when Err /= Libc.Errno.POSIX_2008.EINTR;
         else
            Err := 0;
            exit;
         end if;
      end loop;

      Poller_Event_Channels.Push
        (Poll_Future_Channels (Replier),
         (Err => Errors.Error (Err), Events => Heard_Events));
      if not Triggers.Is_Null_Signaller (Signaller) then
         Triggers.Broadcast (Signaller);
      end if;
   end Do_Poll;

   procedure Do_Remind_Me (R : Remind_Me_Command) is
      Time : constant Ada.Real_Time.Time := R.Time;
      Replier : constant Live_Remind_Me_Future := R.Replier;
      Signaller : constant Triggers.Signaller := R.Signaller;
      Event : Remind_Me_Event;
   begin
      delay until Time;

      Remind_Me_Event_Channels.Push (Remind_Me_Future_Channels (Replier), Event);
      if not Triggers.Is_Null_Signaller (Signaller) then
         Triggers.Broadcast (Signaller);
      end if;
   end Do_Remind_Me;

   procedure Do_Work with Spark_Mode is
   begin
      loop
	 declare
	    New_Command : Any_Command;
	 begin
	    My_Command_Queue.Dequeue (New_Command);

	    case New_Command.C.T is
	       when Invalid_Type =>
		  raise Program_Error;

	       when Write_Type =>
		  Do_Write (New_Command.C.Write_Object);
	       when Read_Type =>
		  Do_Read (New_Command.C.Read_Object);
	       when Poll_Type =>
		  Do_Poll (New_Command.C.Poll_Object);
	       when Remind_Me_Type =>
		  Do_Remind_Me (New_Command.C.Remind_Me_Object);
	    end case;
	 end;
      end loop;
   end Do_Work;

   task body Worker_Task is
   begin
      Do_Work;
   end Worker_Task;

   function Read_Future_Is_Live
     (Future : Read_Future) return Boolean is
     (Future /= 0);
   function Write_Future_Is_Live
     (Future : Write_Future) return Boolean is
     (Future /= 0);
   function Poll_Future_Is_Live
     (Future : Poll_Future) return Boolean is
     (Future /= 0);
   function Remind_Me_Future_Is_Live
     (Future : Remind_Me_Future) return Boolean is
     (Future /= 0);

begin
   for II in 1 .. Max_Read_Futures loop
      Spare_Read_Futures.Push (Live_Read_Future (Read_Future (II)));
   end loop;

   for II in 1 .. Max_Write_Futures loop
      Spare_Write_Futures.Push (Live_Write_Future (Write_Future (II)));
   end loop;

   for II in 1 .. Max_Poll_Futures loop
      Spare_Poll_Futures.Push (Live_Poll_Future (Poll_Future (II)));
   end loop;

   for II in 1 .. Max_Remind_Me_Futures loop
      Spare_Remind_Me_Futures.Push (Live_Remind_Me_Future (Remind_Me_Future (II)));
   end loop;
end Linted.IO_Pool;
