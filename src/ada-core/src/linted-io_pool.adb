-- Copyright 2015,2016 Steven Stewart-Gallus
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
     Spark_Mode => Off is
   package C renames Interfaces.C;

   package Errno renames Libc.Errno;

   use type Libc.Sys.Types.ssize_t;
   use type C.size_t;
   use type Interfaces.C.unsigned;

   type Writer_Event_Channel;
   type Read_Event_Channel;
   type Poller_Event_Channel;

   type Write_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : access Writer_Event_Channel;
   end record;

   type Read_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
      Replier : access Read_Event_Channel;
   end record;

   type Poller_Command is record
      Object : KOs.KO;
      Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Replier : access Poller_Event_Channel;
   end record;

   package CWQueues is new Queues (Write_Command, 32);
   package Writer_Event_Channels is new Channels (Writer_Event);
   type Writer_Event_Channel is new Writer_Event_Channels.Channel;

   package CRQueues is new Queues (Read_Command, 32);
   package Reader_Event_Channels is new Channels (Reader_Event);
   type Read_Event_Channel is new Reader_Event_Channels.Channel;

   package CPQueues is new Queues (Poller_Command, 32);
   package Poller_Event_Channels is new Channels (Poller_Event);
   type Poller_Event_Channel is new Poller_Event_Channels.Channel;

   Writer_Trigger : Wait_Lists.Wait_List;
   My_Write_Command_Channel : CWQueues.Queue;

   Reader_Trigger : Wait_Lists.Wait_List;
   My_Read_Command_Channel : CRQueues.Queue;

   Poller_Trigger : Wait_Lists.Wait_List;
   My_Poller_Command_Channel : CPQueues.Queue;

   task type Writer_Task;
   task type Reader_Task;
   task type Poller_Task;

   Writer_Tasks : array (1 .. 8) of Writer_Task;
   Reader_Tasks : array (1 .. 8) of Reader_Task;
   Poller_Tasks : array (1 .. 8) of Poller_Task;

   task body Writer_Task is
      New_Write_Command : Write_Command;
      Result : Libc.Sys.Types.ssize_t;
      Init : Boolean;
   begin
      loop
         Wait_Lists.Wait (Writer_Trigger);
         loop
            CWQueues.Remove
              (My_Write_Command_Channel,
               New_Write_Command,
               Init);
            if not Init then
               exit;
            end if;

            declare
               Err : C.int;
               Bytes_Written : C.size_t := 0;

               Object : constant KOs.KO := New_Write_Command.Object;
               Buf : constant System.Address := New_Write_Command.Buf;
               Count : constant C.size_t := New_Write_Command.Count;
               Replier : constant access Writer_Event_Channel :=
                 New_Write_Command.Replier;
            begin
               loop
                  Result :=
                    Libc.Unistd.write
                      (C.int (Object),
                       Buf,
                       Count - Bytes_Written);
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

               Replier.Push
                 (Writer_Event'
                    (Err => Errors.Error (Err),
                     Bytes_Written => Bytes_Written));
            end;
         end loop;
      end loop;
   end Writer_Task;

   task body Reader_Task is
      New_Read_Command : Read_Command;
      Result : Libc.Sys.Types.ssize_t;
      Init : Boolean;
   begin
      loop
         Wait_Lists.Wait (Reader_Trigger);
         loop
            CRQueues.Remove (My_Read_Command_Channel, New_Read_Command, Init);
            if not Init then
               exit;
            end if;

            declare
               Err : C.int;
               Bytes_Read : C.size_t := 0;

               Object : constant KOs.KO := New_Read_Command.Object;
               Buf : constant System.Address := New_Read_Command.Buf;
               Count : constant C.size_t := New_Read_Command.Count;
               Replier : constant access Read_Event_Channel :=
                 New_Read_Command.Replier;
            begin
               loop
                  Result :=
                    Libc.Unistd.read (C.int (Object), Buf, Count - Bytes_Read);
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

               Replier.Push
                 ((Err => Errors.Error (Err), Bytes_Read => Bytes_Read));
            end;
         end loop;
      end loop;
   end Reader_Task;

   task body Poller_Task is
      New_Poller_Command : Poller_Command;
      Init : Boolean;
   begin
      loop
         Wait_Lists.Wait (Poller_Trigger);
         loop
            CPQueues.Remove
              (My_Poller_Command_Channel,
               New_Poller_Command,
               Init);
            if not Init then
               exit;
            end if;

            declare
               Err : C.int;

               Object : constant KOs.KO := New_Poller_Command.Object;
               Listen_Events : constant Poller_Event_Set :=
                 New_Poller_Command.Events;
               Heard_Events : Poller_Event_Set :=
                 (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
               Replier : constant access Poller_Event_Channel :=
                 New_Poller_Command.Replier;

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

                     Nfds :=
                       Libc.Sys.Poll.poll (Pollfd'Unchecked_Access, 1, -1);

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

               Replier.Push
                 ((Err => Errors.Error (Err), Events => Heard_Events));
            end;
         end loop;
      end loop;
   end Poller_Task;

   package body Writer_Worker with
        Spark_Mode => Off is
      My_Event_Channel : aliased Writer_Event_Channel;

      procedure Wait (Event : out Writer_Event) is
      begin
         My_Event_Channel.Pop (Event);
      end Wait;

      procedure Write
        (Object : KOs.KO;
         Buf : System.Address;
         Count : C.size_t)
      is
      begin
         CWQueues.Insert
           (My_Write_Command_Channel,
            (Object, Buf, Count, My_Event_Channel'Unchecked_Access));
         Wait_Lists.Broadcast (Writer_Trigger);
      end Write;
   end Writer_Worker;

   package body Reader_Worker with
        Spark_Mode => Off is
      My_Event_Channel : aliased Read_Event_Channel;

      procedure Wait (Event : out Reader_Event) is
      begin
         My_Event_Channel.Pop (Event);
      end Wait;

      procedure Read
        (Object : KOs.KO;
         Buf : System.Address;
         Count : C.size_t)
      is
      begin
         CRQueues.Insert
           (My_Read_Command_Channel,
            (Object, Buf, Count, My_Event_Channel'Unchecked_Access));
         Wait_Lists.Broadcast (Reader_Trigger);
      end Read;
   end Reader_Worker;

   package body Poller_Worker with
        Spark_Mode => Off is
      My_Event_Channel : aliased Poller_Event_Channel;

      procedure Poll (Object : KOs.KO; Events : Poller_Event_Set) is
      begin
         CPQueues.Insert
           (My_Poller_Command_Channel,
            (Object, Events, My_Event_Channel'Unchecked_Access));
         Wait_Lists.Broadcast (Poller_Trigger);
      end Poll;

      procedure Wait (Event : out Poller_Event) is
      begin
         My_Event_Channel.Pop (Event);
      end Wait;
   end Poller_Worker;
end Linted.IO_Pool;
