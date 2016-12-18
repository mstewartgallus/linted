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
with Interfaces.C.Strings;
with System;

package Libc.Stdio with SPARK_Mode => Off is
   pragma Preelaborate;

   --  unsupported macro: BUFSIZ _IO_BUFSIZ
   SEEK_SET : constant := 0;
   SEEK_CUR : constant := 1;

   type FILE is limited private;

   --  subtype off_t is Libc.Sys.Types.uu_off_t;  -- /usr/include/stdio.h:90
   --  subtype off64_t is Libc.Sys.Types.uu_off64_t;  -- /usr/include/stdio.h:97

   --  subtype ssize_t is Libc.Sys.Types.uu_ssize_t;  -- /usr/include/stdio.h:102

   --  subtype fpos_t is Libc.uG_config_h.u_G_fpos_t;
   --  subtype fpos64_t is Libc.uG_config_h.u_G_fpos64_t;

   stdin : access FILE;  -- /usr/include/stdio.h:168
   pragma Import (C, stdin, "stdin");

   stdout : access FILE;  -- /usr/include/stdio.h:169
   pragma Import (C, stdout, "stdout");

   stderr : access FILE;  -- /usr/include/stdio.h:170
   pragma Import (C, stderr, "stderr");

   function remove (uu_filename : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdio.h:178
   pragma Import (C, remove, "remove");

   function rename (uu_old : Interfaces.C.Strings.chars_ptr; uu_new : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdio.h:180
   pragma Import (C, rename, "rename");

   function tmpfile return access FILE;  -- /usr/include/stdio.h:195
   pragma Import (C, tmpfile, "tmpfile");

   function tmpnam (uu_s : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdio.h:209
   pragma Import (C, tmpnam, "tmpnam");

   function fclose (uu_stream : access FILE) return int;  -- /usr/include/stdio.h:237
   pragma Import (C, fclose, "fclose");

   function fflush (uu_stream : access FILE) return int;  -- /usr/include/stdio.h:242
   pragma Import (C, fflush, "fflush");

   function fopen (uu_filename : Interfaces.C.Strings.chars_ptr; uu_modes : Interfaces.C.Strings.chars_ptr) return access FILE;  -- /usr/include/stdio.h:272
   pragma Import (C, fopen, "fopen");

   function freopen
     (uu_filename : Interfaces.C.Strings.chars_ptr;
      uu_modes : Interfaces.C.Strings.chars_ptr;
      uu_stream : access FILE) return access FILE;  -- /usr/include/stdio.h:278
   pragma Import (C, freopen, "freopen");
   procedure setbuf (uu_stream : access FILE; uu_buf : Interfaces.C.Strings.chars_ptr);  -- /usr/include/stdio.h:332
   pragma Import (C, setbuf, "setbuf");

   function setvbuf
     (uu_stream : access FILE;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_modes : int;
      uu_n : size_t) return int;  -- /usr/include/stdio.h:336
   pragma Import (C, setvbuf, "setvbuf");

   procedure setbuffer
     (uu_stream : access FILE;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_size : size_t);  -- /usr/include/stdio.h:343
   pragma Import (C, setbuffer, "setbuffer");

   function fprintf (uu_stream : access FILE; uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:356
   pragma Import (C, fprintf, "fprintf");

   function fscanf (uu_stream : access FILE; uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:425
   pragma Import (C, fscanf, "fscanf");

   function printf (uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:362
   pragma Import (C, printf, "printf");

   function scanf (uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:431
   pragma Import (C, scanf, "scanf");

   function snprintf
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_maxlen : size_t;
      uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:386
   pragma Import (C, snprintf, "snprintf");

   function sprintf (uu_s : Interfaces.C.Strings.chars_ptr; uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:364
   pragma Import (C, sprintf, "sprintf");

   function sscanf (uu_s : Interfaces.C.Strings.chars_ptr; uu_format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:433
   pragma Import (C, sscanf, "sscanf");

   function vfprintf
     (uu_s : access FILE;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:371
   pragma Import (C, vfprintf, "vfprintf");

   function vprintf (uu_format : Interfaces.C.Strings.chars_ptr; uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:377
   pragma Import (C, vprintf, "vprintf");

   function vsprintf
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:379
   pragma Import (C, vsprintf, "vsprintf");

   function vsnprintf
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_maxlen : size_t;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:390
   pragma Import (C, vsnprintf, "vsnprintf");

   function vasprintf
     (uu_ptr : System.Address;
      uu_f : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:399
   pragma Import (C, vasprintf, "vasprintf");

   function asprintf (uu_ptr : System.Address; uu_fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:405
   pragma Import (C, asprintf, "asprintf");

   function vdprintf
     (uu_fd : int;
      uu_fmt : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:412
   pragma Import (C, vdprintf, "vdprintf");

   function dprintf (uu_fd : int; uu_fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:415
   pragma Import (C, dprintf, "dprintf");

   function vfscanf
     (uu_s : access FILE;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:471
   pragma Import (C, vfscanf, "vfscanf");

   function vscanf (uu_format : Interfaces.C.Strings.chars_ptr; uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:479
   pragma Import (C, vscanf, "vscanf");

   function vsscanf
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_arg : access System.Address) return int;  -- /usr/include/stdio.h:483
   pragma Import (C, vsscanf, "vsscanf");

   function fgetc (uu_stream : access FILE) return int;  -- /usr/include/stdio.h:531
   pragma Import (C, fgetc, "fgetc");

   function getc (uu_stream : access FILE) return int;  -- /usr/include/stdio.h:532
   pragma Import (C, getc, "getc");

   function getchar return int;  -- /usr/include/stdio.h:538
   pragma Import (C, getchar, "getchar");

   function fputc (uu_c : int; uu_stream : access FILE) return int;  -- /usr/include/stdio.h:573
   pragma Import (C, fputc, "fputc");

   function putc (uu_c : int; uu_stream : access FILE) return int;  -- /usr/include/stdio.h:574
   pragma Import (C, putc, "putc");

   function putchar (uu_c : int) return int;  -- /usr/include/stdio.h:580
   pragma Import (C, putchar, "putchar");

   function fgets
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_n : int;
      uu_stream : access FILE) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdio.h:622
   pragma Import (C, fgets, "fgets");

   function gets (uu_s : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdio.h:638
   pragma Import (C, gets, "gets");

   function fputs (uu_s : Interfaces.C.Strings.chars_ptr; uu_stream : access FILE) return int;  -- /usr/include/stdio.h:689
   pragma Import (C, fputs, "fputs");

   function puts (uu_s : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdio.h:695
   pragma Import (C, puts, "puts");

   function ungetc (uu_c : int; uu_stream : access FILE) return int;  -- /usr/include/stdio.h:702
   pragma Import (C, ungetc, "ungetc");

   function fread
     (uu_ptr : System.Address;
      uu_size : size_t;
      uu_n : size_t;
      uu_stream : access FILE) return size_t;  -- /usr/include/stdio.h:709
   pragma Import (C, fread, "fread");

   function fwrite
     (uu_ptr : System.Address;
      uu_size : size_t;
      uu_n : size_t;
      uu_s : access FILE) return size_t;  -- /usr/include/stdio.h:715
   pragma Import (C, fwrite, "fwrite");

   function fseek
     (uu_stream : access FILE;
      uu_off : long;
      uu_whence : int) return int;  -- /usr/include/stdio.h:749
   pragma Import (C, fseek, "fseek");

   function ftell (uu_stream : access FILE) return long;  -- /usr/include/stdio.h:754
   pragma Import (C, ftell, "ftell");

   procedure rewind (uu_stream : access FILE);  -- /usr/include/stdio.h:759
   pragma Import (C, rewind, "rewind");

   --  function fseeko
   --    (uu_stream : access FILE;
   --     uu_off : Libc.Sys.Types.uu_off_t;
   --     uu_whence : int) return int;  -- /usr/include/stdio.h:773
   --  pragma Import (C, fseeko, "fseeko");

   --  function ftello (uu_stream : access FILE) return Libc.Sys.Types.uu_off_t;  -- /usr/include/stdio.h:778
   --  pragma Import (C, ftello, "ftello");

   --  function fgetpos (uu_stream : access FILE; uu_pos : access fpos_t) return int;  -- /usr/include/stdio.h:798
   --  pragma Import (C, fgetpos, "fgetpos");

   --  function fsetpos (uu_stream : access FILE; uu_pos : access fpos_t) return int;  -- /usr/include/stdio.h:803
   --  pragma Import (C, fsetpos, "fsetpos");

   procedure clearerr (uu_stream : access FILE);  -- /usr/include/stdio.h:826
   pragma Import (C, clearerr, "clearerr");

   function feof (uu_stream : access FILE) return int;  -- /usr/include/stdio.h:828
   pragma Import (C, feof, "feof");

   function ferror (uu_stream : access FILE) return int;  -- /usr/include/stdio.h:830
   pragma Import (C, ferror, "ferror");

   procedure perror (uu_s : Interfaces.C.Strings.chars_ptr);  -- /usr/include/stdio.h:846
   pragma Import (C, perror, "perror");
private
   type FILE is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, FILE);
end Libc.Stdio;
