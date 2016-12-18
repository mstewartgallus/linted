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
with Libc.Sys.Types;
with Libc.Unistd;
with Libc.Errno.POSIX_2008;
with Libc.Errno;

with Linted.Channels;

package body Linted.IO_Pool is
   package C renames Interfaces.C;

   package Errno renames Libc.Errno;

   type Write_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
   end record;

   package Write_Command_Channels is new Linted.Channels (Write_Command);
   package Writer_Event_Channels is new Linted.Channels (Writer_Event);

   type Read_Command is record
      Object : KOs.KO;
      Buf : System.Address := System.Null_Address;
      Count : C.size_t := 0;
   end record;

   package Read_Command_Channels is new Linted.Channels (Read_Command);
   package Reader_Event_Channels is new Linted.Channels (Reader_Event);

   package body Writer_Worker with SPARK_Mode => Off is
      task Writer_Task;
      My_Command_Channel : Write_Command_Channels.Channel;
      My_Event_Channel : Writer_Event_Channels.Channel;

      procedure Wait (Event : out Writer_Event) is begin
	 My_Event_Channel.Pop (Event);
      end Wait;

      procedure Write (Object : KOs.KO; Buf : System.Address; Count : C.size_t) is
      begin
	 My_Command_Channel.Push ((Object, Buf, Count));
      end Write;

      task body Writer_Task is
	 use type Libc.Sys.Types.ssize_t;
	 use type C.size_t;

	 New_Write_Command : Write_Command;
	 Result : Libc.Sys.Types.ssize_t;
      begin
	 loop
	    My_Command_Channel.Pop (New_Write_Command);

	    declare
	       Err : C.int;
	       Bytes_Written : C.size_t := 0;

	       Object : constant KOs.KO := New_Write_Command.Object;
	       Buf : constant System.Address := New_Write_Command.Buf;
	       Count : constant C.size_t := New_Write_Command.Count;
	    begin
	       loop
		  Result := Libc.Unistd.write (C.int (Object), Buf, Count - Bytes_Written);
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

	       My_Event_Channel.Push (Writer_Event'(Err => Errors.Error (Err), Bytes_Written => Bytes_Written));
	    end;
	 end loop;
      end Writer_Task;
   end Writer_Worker;

   package body Reader_Worker with SPARK_Mode => Off is
      task Reader_Task;

      My_Command_Channel : Read_Command_Channels.Channel;
      My_Event_Channel : Reader_Event_Channels.Channel;

      procedure Wait (Event : out Reader_Event) is begin
	 My_Event_Channel.Pop (Event);
      end Wait;

      procedure Read (Object : KOs.KO; Buf : System.Address; Count : C.size_t) is
      begin
	 My_Command_Channel.Push ((Object, Buf, Count));
      end Read;

      task body Reader_Task is
	 use type Libc.Sys.Types.ssize_t;
	 use type C.size_t;

	 New_Read_Command : Read_Command;
	 Result : Libc.Sys.Types.ssize_t;
      begin
	 loop
	    My_Command_Channel.Pop (New_Read_Command);

	    declare
	       Err : C.int;
	       Bytes_Read : C.size_t := 0;

	       Object : constant KOs.KO := New_Read_Command.Object;
	       Buf : constant System.Address := New_Read_Command.Buf;
	       Count : constant C.size_t := New_Read_Command.Count;
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

	       My_Event_Channel.Push ((Err => Errors.Error (Err), Bytes_Read => Bytes_Read));
	    end;
	 end loop;
      end Reader_Task;
   end Reader_Worker;
end Linted.IO_Pool;
