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
with Linted.Queues;
with Linted.Wait_Lists;

package body Linted.IO_Pool with
     Spark_Mode => Off,
     Refined_State =>
     (Command_Queue => (My_Command_Queue, Command_Wait_List),
      Event_Queue =>
        (Read_Future_Channels, Write_Future_Channels, Poll_Future_Channels),
      Various =>
        (Worker_Tasks,
         Command_Queues.State,
	 Command_Queues.Structure,

	 Read_Future_Queues.State,
	 Write_Future_Queues.State,
	 Poll_Future_Queues.State,

	 Read_Future_Queues.Structure,
	 Write_Future_Queues.Structure,
	 Poll_Future_Queues.Structure,

         Read_Future_Wait_List,
         Write_Future_Wait_List,
         Poll_Future_Wait_List),
      Future_Pool =>
        (Spare_Write_Futures, Spare_Read_Futures, Spare_Poll_Futures))
is
   Max_Read_Futures : constant := 32;
   Max_Write_Futures : constant := 32;
   Max_Poll_Futures : constant := 32;
   Max_Command_Queue_Capacity : constant := 32;

   package C renames Interfaces.C;

   package Errno renames Libc.Errno;

   use type Libc.Sys.Types.ssize_t;
   use type C.size_t;
   use type Interfaces.C.unsigned;

   type Write_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : Write_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Read_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : Read_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Poller_Command is record
      Object : KOs.KO;
      Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : Poll_Future;
      Signaller : Triggers.Signaller;
   end record;

   type Command_Type is (Invalid_Type, Write_Type, Read_Type, Poll_Type);

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
      end case;
   end record;

   Command_Wait_List : Wait_Lists.Wait_List;
   task type Worker_Task with Global => (In_Out => (Command_Queues.State,
						    Command_Queues.Structure,
						    Command_Wait_List,
					 My_Command_Queue,
					 Read_Future_Channels,
					 Write_Future_Channels,
						    Poll_Future_Channels
						   )),
     Depends => (Command_Wait_List => (My_Command_Queue, Command_Wait_List, Command_Queues.State, Command_Queues.Structure),
		 Command_Queues.State => (My_Command_Queue, Command_Queues.State, Command_Queues.Structure),
		 Command_Queues.Structure => (My_Command_Queue, Command_Queues.State, Command_Queues.Structure),
		 My_Command_Queue => (Command_Queues.State, Command_Queues.Structure, My_Command_Queue),
		 Read_Future_Channels => (Command_Queues.State, Command_Queues.Structure, Read_Future_Channels, My_Command_Queue),
		 Write_Future_Channels => (Command_Queues.State, Command_Queues.Structure, Write_Future_Channels, My_Command_Queue),
		 Poll_Future_Channels => (Command_Queues.State, Command_Queues.Structure, Poll_Future_Channels, My_Command_Queue),
		 Worker_Task'Result => Worker_Task,
		 Worker_Task => null
		);

   Worker_Tasks : array (1 .. 16) of Worker_Task;

   package Writer_Event_Channels is new Channels (Writer_Event);
   package Reader_Event_Channels is new Channels (Reader_Event);
   package Poller_Event_Channels is new Channels (Poller_Event);

   type Read_Future_Channels_Array is
     array (Read_Future range <>) of Reader_Event_Channels.Channel;
   type Write_Future_Channels_Array is
     array (Write_Future range <>) of Writer_Event_Channels.Channel;
   type Poller_Future_Channels_Array is
     array (Poll_Future range <>) of Poller_Event_Channels.Channel;

   Read_Future_Channels : Read_Future_Channels_Array (1 .. Max_Read_Futures);
   Write_Future_Channels : Write_Future_Channels_Array
   (1 .. Max_Write_Futures);
   Poll_Future_Channels : Poller_Future_Channels_Array (1 .. Max_Poll_Futures);

   Read_Future_Wait_List : Wait_Lists.Wait_List;
   Write_Future_Wait_List : Wait_Lists.Wait_List;
   Poll_Future_Wait_List : Wait_Lists.Wait_List;

   package Read_Future_Queues is new Queues (Read_Future, Max_Read_Futures);
   package Write_Future_Queues is new Queues (Write_Future, Max_Write_Futures);
   package Poll_Future_Queues is new Queues (Poll_Future, Max_Poll_Futures);

   Spare_Read_Futures : Read_Future_Queues.Queue;
   Spare_Write_Futures : Write_Future_Queues.Queue;
   Spare_Poll_Futures : Poll_Future_Queues.Queue;

   package Command_Queues is new Queues (Command, Max_Command_Queue_Capacity);

   My_Command_Queue : Command_Queues.Queue;

   procedure Do_Write (W : Write_Command) with
      Global => (In_Out => Write_Future_Channels),
      Depends => (Write_Future_Channels => (W, Write_Future_Channels));
   procedure Do_Read (R : Read_Command) with
      Global => (In_Out => Read_Future_Channels),
      Depends => (Read_Future_Channels => (R, Read_Future_Channels));
   procedure Do_Poll (P : Poller_Command) with
      Global => (In_Out => Poll_Future_Channels),
      Depends => (Poll_Future_Channels => (P, Poll_Future_Channels));

   procedure Read
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Read_Future)
   is
      Init : Boolean;
   begin
      loop
         Read_Future_Queues.Try_Dequeue (Spare_Read_Futures, Future, Init);
	 exit when Init;
         Wait_Lists.Wait (Read_Future_Wait_List);
      end loop;

      Command_Queues.Enqueue
        (My_Command_Queue,
         (Read_Type, (Object, Buf, Count, Future, Signaller)));
      Wait_Lists.Signal (Command_Wait_List);
   end Read;

   procedure Read_Wait
     (Future : in out Read_Future;
      Event : out Reader_Event)
   is
   begin
      Reader_Event_Channels.Pop (Read_Future_Channels (Future), Event);
      Read_Future_Queues.Enqueue (Spare_Read_Futures, Future);
      Wait_Lists.Signal (Read_Future_Wait_List);
      Future := 0;
   end Read_Wait;

   procedure Read_Poll
     (Future : in out Read_Future;
      Event : out Reader_Event;
      Init : out Boolean)
   is
      Dummy : Reader_Event;
      Maybe_Event : Reader_Event_Channels.Option_Element_Ts.Option;
   begin
      Reader_Event_Channels.Poll (Read_Future_Channels (Future), Maybe_Event);
      if Maybe_Event.Empty then
	 Event := Dummy;
         Init := False;
      else
         Read_Future_Queues.Enqueue (Spare_Read_Futures, Future);
         Wait_Lists.Signal (Read_Future_Wait_List);
         Event := Maybe_Event.Data;
         Future := 0;
         Init := True;
      end if;
   end Read_Poll;

   procedure Write
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Write_Future)
   is
      Init : Boolean;
   begin
      loop
         Write_Future_Queues.Try_Dequeue (Spare_Write_Futures, Future, Init);
         exit when Init;
         Wait_Lists.Wait (Write_Future_Wait_List);
      end loop;

      Command_Queues.Enqueue
        (My_Command_Queue,
         (Write_Type, (Object, Buf, Count, Future, Signaller)));
      Wait_Lists.Signal (Command_Wait_List);
   end Write;

   procedure Write_Wait
     (Future : in out Write_Future;
      Event : out Writer_Event)
   is
   begin
      Writer_Event_Channels.Pop (Write_Future_Channels (Future), Event);
      Write_Future_Queues.Enqueue (Spare_Write_Futures, Future);
      Wait_Lists.Signal (Write_Future_Wait_List);
      Future := 0;
   end Write_Wait;

   procedure Write_Poll
     (Future : in out Write_Future;
      Event : out Writer_Event;
      Init : out Boolean)
   is
      Dummy : Writer_Event;
      Maybe_Event : Writer_Event_Channels.Option_Element_Ts.Option;
   begin
      Writer_Event_Channels.Poll (Write_Future_Channels (Future), Maybe_Event);
      if Maybe_Event.Empty then
	 Event := Dummy;
         Init := False;
      else
         Write_Future_Queues.Enqueue (Spare_Write_Futures, Future);
         Wait_Lists.Signal (Write_Future_Wait_List);
         Event := Maybe_Event.Data;
         Future := 0;
         Init := True;
      end if;
   end Write_Poll;

   procedure Poll
     (Object : KOs.KO;
      Events : Poller_Event_Set;
      Signaller : Triggers.Signaller;
      Future : out Poll_Future)
   is
      Init : Boolean;
   begin
      loop
         Poll_Future_Queues.Try_Dequeue (Spare_Poll_Futures, Future, Init);
         exit when Init;
         Wait_Lists.Wait (Poll_Future_Wait_List);
      end loop;

      Command_Queues.Enqueue
        (My_Command_Queue,
         (Poll_Type, (Object, Events, Future, Signaller)));
      Wait_Lists.Signal (Command_Wait_List);
   end Poll;

   procedure Poll_Wait
     (Future : in out Poll_Future;
      Event : out Poller_Event)
   is
   begin
      Poller_Event_Channels.Pop (Poll_Future_Channels (Future), Event);
      Poll_Future_Queues.Enqueue (Spare_Poll_Futures, Future);
      Wait_Lists.Signal (Poll_Future_Wait_List);
      Future := 0;
   end Poll_Wait;

   procedure Poll_Poll
     (Future : in out Poll_Future;
      Event : out Poller_Event;
      Init : out Boolean)
   is
      Dummy : Poller_Event;
      Maybe_Event : Poller_Event_Channels.Option_Element_Ts.Option;
   begin
      Poller_Event_Channels.Poll (Poll_Future_Channels (Future), Maybe_Event);
      if Maybe_Event.Empty then
	 Event := Dummy;
         Init := False;
      else
         Poll_Future_Queues.Enqueue (Spare_Poll_Futures, Future);
         Wait_Lists.Signal (Poll_Future_Wait_List);
         Event := Maybe_Event.Data;
         Future := 0;
         Init := True;
      end if;
   end Poll_Poll;

   procedure Do_Write (W : Write_Command) is
      Err : C.int;
      Bytes_Written : C.size_t := 0;

      Object : constant KOs.KO := W.Object;
      Buf : constant System.Address := W.Buf;
      Count : constant C.size_t := W.Count;
      Replier : constant Write_Future := W.Replier;
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
         Triggers.Signal (Signaller);
      end if;
   end Do_Write;

   procedure Do_Read (R : Read_Command) with
      Spark_Mode => Off is
      Err : C.int;
      Bytes_Read : C.size_t := 0;

      Object : constant KOs.KO := R.Object;
      Buf : constant System.Address := R.Buf;
      Count : constant C.size_t := R.Count;
      Replier : constant Read_Future := R.Replier;
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
         Triggers.Signal (Signaller);
      end if;
   end Do_Read;

   procedure Do_Poll (P : Poller_Command) with
      Spark_Mode => Off is
      Err : C.int;

      Object : constant KOs.KO := P.Object;
      Listen_Events : constant Poller_Event_Set := P.Events;
      Heard_Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : constant Poll_Future := P.Replier;
      Signaller : constant Triggers.Signaller := P.Signaller;

      Nfds : C.int;
   begin
      loop
         declare
            Pollfd : aliased Libc.Sys.Poll.pollfd :=
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

            Nfds := Libc.Sys.Poll.poll (Pollfd'Unchecked_Access, 1, -1);

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
         Triggers.Signal (Signaller);
      end if;
   end Do_Poll;

   task body Worker_Task is
      New_Command : Command;
      Init : Boolean;
   begin
      loop
         loop
            Command_Queues.Try_Dequeue (My_Command_Queue, New_Command, Init);
            exit when Init;
            Wait_Lists.Wait (Command_Wait_List);
         end loop;

         case New_Command.T is
            when Invalid_Type =>
               raise Program_Error;

            when Write_Type =>
               Do_Write (New_Command.Write_Object);
            when Read_Type =>
               Do_Read (New_Command.Read_Object);
            when Poll_Type =>
               Do_Poll (New_Command.Poll_Object);
         end case;
      end loop;
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

begin
   for II in 1 .. Max_Read_Futures loop
      Read_Future_Queues.Enqueue (Spare_Read_Futures, Read_Future (II));
   end loop;
   for II in 1 .. Max_Write_Futures loop
      Write_Future_Queues.Enqueue (Spare_Write_Futures, Write_Future (II));
   end loop;
   for II in 1 .. Max_Poll_Futures loop
      Poll_Future_Queues.Enqueue (Spare_Poll_Futures, Poll_Future (II));
   end loop;
end Linted.IO_Pool;
