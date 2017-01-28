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

package body Linted.IO_Pool with
     Spark_Mode => Off is
   Max_Read_Futures : constant := 32;
   Max_Command_Queue_Capacity : constant := 32;

   package C renames Interfaces.C;

   package Errno renames Libc.Errno;

   use type Libc.Sys.Types.ssize_t;
   use type C.size_t;
   use type Interfaces.C.unsigned;

   type Writer_Event_Channel;
   type Read_Event_Channel;
   type Poller_Event_Channel;

   type Writer_Event_Channel_Access is
     not null access all Writer_Event_Channel;
   type Read_Event_Channel_Access is not null access all Read_Event_Channel;
   type Poller_Event_Channel_Access is
     not null access all Poller_Event_Channel;

   type Write_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : Writer_Event_Channel_Access;
   end record;

   type Read_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : Read_Event_Channel_Access;
      Signaller : Triggers.Signaller;
   end record;

   type Poller_Command is record
      Object : KOs.KO;
      Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : Poller_Event_Channel_Access;
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

   package Command_Queues is new Queues (Command, Max_Command_Queue_Capacity);

   package Writer_Event_Channels is new Channels (Writer_Event);
   type Writer_Event_Channel is record
      Channel : Writer_Event_Channels.Channel;
   end record;

   package Reader_Event_Channels is new Channels (Reader_Event);
   type Read_Event_Channel is record
      Channel : Reader_Event_Channels.Channel;
   end record;

   package Poller_Event_Channels is new Channels (Poller_Event);
   type Poller_Event_Channel is record
      Channel : Poller_Event_Channels.Channel;
   end record;

   My_Command_Channel : Command_Queues.Queue;

   task type Worker_Task;

   Worker_Tasks : array (1 .. 16) of Worker_Task;

   procedure Do_Write (W : Write_Command);
   procedure Do_Read (R : Read_Command);
   procedure Do_Poll (P : Poller_Command);

   task body Worker_Task is
      New_Command : Command;
   begin
      loop
         Command_Queues.Dequeue (My_Command_Channel, New_Command);

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

   procedure Do_Write (W : Write_Command) is
      Err : C.int;
      Bytes_Written : C.size_t := 0;

      Object : constant KOs.KO := W.Object;
      Buf : constant System.Address := W.Buf;
      Count : constant C.size_t := W.Count;
      Replier : constant Writer_Event_Channel_Access := W.Replier;
      Result : Libc.Sys.Types.ssize_t;
   begin
      loop
         Result :=
           Libc.Unistd.write (C.int (Object), Buf, Count - Bytes_Written);
         if Result < 0 then
            Err := Libc.Errno.Errno;
            if Err /= Libc.Errno.POSIX_2008.EINTR then
               exit;
            end if;
         else
            Err := 0;
            Bytes_Written := Bytes_Written + C.size_t (Result);
            if Bytes_Written = Count then
               exit;
            end if;
         end if;
      end loop;

      Writer_Event_Channels.Push
        (Replier.Channel,
         (Err => Errors.Error (Err), Bytes_Written => Bytes_Written));
   end Do_Write;

   procedure Do_Read (R : Read_Command) is
      Err : C.int;
      Bytes_Read : C.size_t := 0;

      Object : constant KOs.KO := R.Object;
      Buf : constant System.Address := R.Buf;
      Count : constant C.size_t := R.Count;
      Replier : constant Read_Event_Channel_Access := R.Replier;
      Signaller : constant Triggers.Signaller := R.Signaller;

      Result : Libc.Sys.Types.ssize_t;
   begin
      loop
         Result := Libc.Unistd.read (C.int (Object), Buf, Count - Bytes_Read);
         if Result < 0 then
            Err := Errno.Errno;
            if Err /= Libc.Errno.POSIX_2008.EINTR then
               exit;
            end if;
         elsif 0 = Result then
            Err := 0;
            exit;
         else
            Err := 0;
            Bytes_Read := Bytes_Read + C.size_t (Result);
            if Bytes_Read = Count then
               exit;
            end if;
         end if;
      end loop;

      Reader_Event_Channels.Push
        (Replier.Channel,
         (Err => Errors.Error (Err), Bytes_Read => Bytes_Read));
      Triggers.Signal (Signaller);
   end Do_Read;

   procedure Do_Poll (P : Poller_Command) is
      Err : C.int;

      Object : constant KOs.KO := P.Object;
      Listen_Events : constant Poller_Event_Set := P.Events;
      Heard_Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : constant Poller_Event_Channel_Access := P.Replier;

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
            Err := Errno.Errno;
            if Err /= Libc.Errno.POSIX_2008.EINTR then
               exit;
            end if;
         else
            Err := 0;
            exit;
         end if;
      end loop;

      Poller_Event_Channels.Push
        (Replier.Channel,
         (Err => Errors.Error (Err), Events => Heard_Events));
   end Do_Poll;

   package body Writer_Worker with
        Spark_Mode => Off is
      My_Event_Channel : aliased Writer_Event_Channel;

      task Waiter;

      task body Waiter is
         Event : Writer_Event;
      begin
         loop
            Writer_Event_Channels.Pop (My_Event_Channel.Channel, Event);
            Notify (Event);
         end loop;
      end Waiter;

      procedure Write
        (Object : KOs.KO;
         Buf : System.Address;
         Count : C.size_t)
      is
      begin
         Command_Queues.Enqueue
           (My_Command_Channel,
            (Write_Type,
             (Object, Buf, Count, My_Event_Channel'Unchecked_Access)));
      end Write;
   end Writer_Worker;

   package body Reader_Worker with
        Spark_Mode => Off is
      My_Event_Channel : aliased Read_Event_Channel;

      task Waiter;

      task body Waiter is
         Event : Reader_Event;
      begin
         loop
            Reader_Event_Channels.Pop (My_Event_Channel.Channel, Event);
            On_Event (Event);
         end loop;
      end Waiter;

      procedure Read
        (Object : KOs.KO;
         Buf : System.Address;
         Count : C.size_t)
      is
      begin
         Command_Queues.Enqueue
           (My_Command_Channel,
            (Read_Type,
             (Object,
              Buf,
              Count,
              My_Event_Channel'Unchecked_Access,
              Triggers.Null_Signaller)));
      end Read;
   end Reader_Worker;

   package body Poller_Worker with
        Spark_Mode => Off is
      My_Event_Channel : aliased Poller_Event_Channel;

      task Waiter;

      task body Waiter is
         Event : Poller_Event;
      begin
         loop
            Poller_Event_Channels.Pop (My_Event_Channel.Channel, Event);
            On_Event (Event);
         end loop;
      end Waiter;

      procedure Poll (Object : KOs.KO; Events : Poller_Event_Set) is
      begin
         Command_Queues.Enqueue
           (My_Command_Channel,
            (Poll_Type, (Object, Events, My_Event_Channel'Unchecked_Access)));
      end Poll;
   end Poller_Worker;

   package Read_Future_Queues is new Queues (Read_Future, Max_Read_Futures);

   Read_Future_Channels : array
   (Read_Future range 1 .. Max_Read_Futures) of aliased Read_Event_Channel;
   Spare_Read_Futures : Read_Future_Queues.Queue;

   function Read_Future_Is_Live (Future : Read_Future) return Boolean is
   begin
      return Future /= 0;
   end Read_Future_Is_Live;

   procedure Read
     (Object : KOs.KO;
      Buf : System.Address;
      Count : Interfaces.C.size_t;
      Signaller : Triggers.Signaller;
      Future : out Read_Future)
   is
   begin
      Read_Future_Queues.Dequeue (Spare_Read_Futures, Future);
      Command_Queues.Enqueue
        (My_Command_Channel,
         (Read_Type,
          (Object,
           Buf,
           Count,
           Read_Future_Channels (Future)'Unchecked_Access,
           Signaller)));
   end Read;

   procedure Read_Wait
     (Future : in out Read_Future;
      Event : out Reader_Event)
   is
   begin
      Reader_Event_Channels.Pop (Read_Future_Channels (Future).Channel, Event);
      Read_Future_Queues.Enqueue (Spare_Read_Futures, Future);
      Future := 0;
   end Read_Wait;

   procedure Read_Poll
     (Future : in out Read_Future;
      Event : out Reader_Event;
      Init : out Boolean)
   is
      Maybe_Event : Reader_Event_Channels.Option_Element_Ts.Option;
   begin
      Reader_Event_Channels.Poll
        (Read_Future_Channels (Future).Channel,
         Maybe_Event);
      if Maybe_Event.Empty then
         Init := False;
      else
	 Event := Maybe_Event.Data;
         Read_Future_Queues.Enqueue (Spare_Read_Futures, Future);
         Future := 0;
         Init := True;
      end if;
   end Read_Poll;

begin
   for II in 1 .. Max_Read_Futures loop
      Read_Future_Queues.Enqueue (Spare_Read_Futures, Read_Future (II));
   end loop;
end Linted.IO_Pool;
