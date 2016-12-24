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

package Libc.Stdio with
     Spark_Mode => Off is
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

   function remove
     (filename : Interfaces.C.Strings.chars_ptr)
     return int;  -- /usr/include/stdio.h:178
   pragma Import (C, remove, "remove");

   function rename
     (old : Interfaces.C.Strings.chars_ptr;
      uu_new : Interfaces.C.Strings.chars_ptr)
     return int;  -- /usr/include/stdio.h:180
   pragma Import (C, rename, "rename");

   function tmpfile return access FILE;  -- /usr/include/stdio.h:195
   pragma Import (C, tmpfile, "tmpfile");

   function tmpnam
     (s : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdio.h:209
   pragma Import (C, tmpnam, "tmpnam");

   function fclose
     (stream : access FILE) return int;  -- /usr/include/stdio.h:237
   pragma Import (C, fclose, "fclose");

   function fflush
     (stream : access FILE) return int;  -- /usr/include/stdio.h:242
   pragma Import (C, fflush, "fflush");

   function fopen
     (filename : Interfaces.C.Strings.chars_ptr;
      modes : Interfaces.C.Strings.chars_ptr)
     return access FILE;  -- /usr/include/stdio.h:272
   pragma Import (C, fopen, "fopen");

   function freopen
     (filename : Interfaces.C.Strings.chars_ptr;
      modes : Interfaces.C.Strings.chars_ptr;
      stream : access FILE) return access FILE;  -- /usr/include/stdio.h:278
   pragma Import (C, freopen, "freopen");
   procedure setbuf
     (stream : access FILE;
      buf : Interfaces.C.Strings.chars_ptr);  -- /usr/include/stdio.h:332
   pragma Import (C, setbuf, "setbuf");

   function setvbuf
     (stream : access FILE;
      buf : Interfaces.C.Strings.chars_ptr;
      modes : int;
      n : size_t) return int;  -- /usr/include/stdio.h:336
   pragma Import (C, setvbuf, "setvbuf");

   procedure setbuffer
     (stream : access FILE;
      buf : Interfaces.C.Strings.chars_ptr;
      size : size_t);  -- /usr/include/stdio.h:343
   pragma Import (C, setbuffer, "setbuffer");

   function fprintf
     (stream : access FILE;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:356
   pragma Import (C, fprintf, "fprintf");

   function fscanf
     (stream : access FILE;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:425
   pragma Import (C, fscanf, "fscanf");

   function printf (format : Interfaces.C.Strings.chars_ptr  -- , ...
   ) return int;  -- /usr/include/stdio.h:362
   pragma Import (C, printf, "printf");

   function scanf (format : Interfaces.C.Strings.chars_ptr  -- , ...
   ) return int;  -- /usr/include/stdio.h:431
   pragma Import (C, scanf, "scanf");

   function snprintf
     (s : Interfaces.C.Strings.chars_ptr;
      maxlen : size_t;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:386
   pragma Import (C, snprintf, "snprintf");

   function sprintf
     (s : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:364
   pragma Import (C, sprintf, "sprintf");

   function sscanf
     (s : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:433
   pragma Import (C, sscanf, "sscanf");

   function vfprintf
     (s : access FILE;
      format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:371
   pragma Import (C, vfprintf, "vfprintf");

   function vprintf
     (format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:377
   pragma Import (C, vprintf, "vprintf");

   function vsprintf
     (s : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:379
   pragma Import (C, vsprintf, "vsprintf");

   function vsnprintf
     (s : Interfaces.C.Strings.chars_ptr;
      maxlen : size_t;
      format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:390
   pragma Import (C, vsnprintf, "vsnprintf");

   function vasprintf
     (ptr : System.Address;
      f : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:399
   pragma Import (C, vasprintf, "vasprintf");

   function asprintf
     (ptr : System.Address;
      fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:405
   pragma Import (C, asprintf, "asprintf");

   function vdprintf
     (fd : int;
      fmt : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:412
   pragma Import (C, vdprintf, "vdprintf");

   function dprintf
     (fd : int;
      fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/stdio.h:415
   pragma Import (C, dprintf, "dprintf");

   function vfscanf
     (s : access FILE;
      format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:471
   pragma Import (C, vfscanf, "vfscanf");

   function vscanf
     (format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:479
   pragma Import (C, vscanf, "vscanf");

   function vsscanf
     (s : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr;
      arg : access System.Address) return int;  -- /usr/include/stdio.h:483
   pragma Import (C, vsscanf, "vsscanf");

   function fgetc
     (stream : access FILE) return int;  -- /usr/include/stdio.h:531
   pragma Import (C, fgetc, "fgetc");

   function getc
     (stream : access FILE) return int;  -- /usr/include/stdio.h:532
   pragma Import (C, getc, "getc");

   function getchar return int;  -- /usr/include/stdio.h:538
   pragma Import (C, getchar, "getchar");

   function fputc
     (c : int;
      stream : access FILE) return int;  -- /usr/include/stdio.h:573
   pragma Import (C, fputc, "fputc");

   function putc
     (c : int;
      stream : access FILE) return int;  -- /usr/include/stdio.h:574
   pragma Import (C, putc, "putc");

   function putchar (c : int) return int;  -- /usr/include/stdio.h:580
   pragma Import (C, putchar, "putchar");

   function fgets
     (s : Interfaces.C.Strings.chars_ptr;
      n : int;
      stream : access FILE)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdio.h:622
   pragma Import (C, fgets, "fgets");

   function gets
     (s : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdio.h:638
   pragma Import (C, gets, "gets");

   function fputs
     (s : Interfaces.C.Strings.chars_ptr;
      stream : access FILE) return int;  -- /usr/include/stdio.h:689
   pragma Import (C, fputs, "fputs");

   function puts
     (s : Interfaces.C.Strings.chars_ptr)
     return int;  -- /usr/include/stdio.h:695
   pragma Import (C, puts, "puts");

   function ungetc
     (c : int;
      stream : access FILE) return int;  -- /usr/include/stdio.h:702
   pragma Import (C, ungetc, "ungetc");

   function fread
     (ptr : System.Address;
      size : size_t;
      n : size_t;
      stream : access FILE) return size_t;  -- /usr/include/stdio.h:709
   pragma Import (C, fread, "fread");

   function fwrite
     (ptr : System.Address;
      size : size_t;
      n : size_t;
      s : access FILE) return size_t;  -- /usr/include/stdio.h:715
   pragma Import (C, fwrite, "fwrite");

   function fseek
     (stream : access FILE;
      off : long;
      whence : int) return int;  -- /usr/include/stdio.h:749
   pragma Import (C, fseek, "fseek");

   function ftell
     (stream : access FILE) return long;  -- /usr/include/stdio.h:754
   pragma Import (C, ftell, "ftell");

   procedure rewind (stream : access FILE);  -- /usr/include/stdio.h:759
   pragma Import (C, rewind, "rewind");

   --  function fseeko
   --    (stream : access FILE;
   --     off : Libc.Sys.Types.off_t;
   --     whence : int) return int;  -- /usr/include/stdio.h:773
   --  pragma Import (C, fseeko, "fseeko");

   --  function ftello (stream : access FILE) return Libc.Sys.Types.off_t;  -- /usr/include/stdio.h:778
   --  pragma Import (C, ftello, "ftello");

   --  function fgetpos (stream : access FILE; pos : access fpos_t) return int;  -- /usr/include/stdio.h:798
   --  pragma Import (C, fgetpos, "fgetpos");

   --  function fsetpos (stream : access FILE; pos : access fpos_t) return int;  -- /usr/include/stdio.h:803
   --  pragma Import (C, fsetpos, "fsetpos");

   procedure clearerr (stream : access FILE);  -- /usr/include/stdio.h:826
   pragma Import (C, clearerr, "clearerr");

   function feof
     (stream : access FILE) return int;  -- /usr/include/stdio.h:828
   pragma Import (C, feof, "feof");

   function ferror
     (stream : access FILE) return int;  -- /usr/include/stdio.h:830
   pragma Import (C, ferror, "ferror");

   procedure perror
     (s : Interfaces.C.Strings.chars_ptr);  -- /usr/include/stdio.h:846
   pragma Import (C, perror, "perror");
private
   type FILE is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, FILE);
end Libc.Stdio;
