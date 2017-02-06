-- Copyright 2015,2017 Steven Stewart-Gallus
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
with Libc.Sys.Types;
with Libc.Stdint;

package Libc.Unistd is
   pragma Preelaborate;

   STDIN_FILENO : constant := 0;
   STDOUT_FILENO : constant := 1;
   STDERR_FILENO : constant := 2;

   R_OK : constant := 4;
   W_OK : constant := 2;
   X_OK : constant := 1;
   F_OK : constant := 0;

   SEEK_SET : constant := 0;
   SEEK_CUR : constant := 1;
   SEEK_END : constant := 2;
   SEEK_DATA : constant := 3;
   SEEK_HOLE : constant := 4;

   L_SET : constant := SEEK_SET;
   L_INCR : constant := SEEK_CUR;
   L_XTND : constant := SEEK_END;

   F_ULOCK : constant := 0;
   F_LOCK : constant := 1;
   F_TLOCK : constant := 2;
   F_TEST : constant := 3;

   --  arg-macro: function TEMP_FAILURE_RETRY (expression)
   --    return __extension__ ({ long int __result; do __result := (long int) (expression); while (__result = -1L  and then  errno = EINTR); __result; });

   function c_access
     (name : Interfaces.C.Strings.chars_ptr;
      c_type : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:287
   pragma Import (C, c_access, "access");

   function euidaccess
     (name : Interfaces.C.Strings.chars_ptr;
      c_type : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:292
   pragma Import (C, euidaccess, "euidaccess");

   function eaccess
     (name : Interfaces.C.Strings.chars_ptr;
      c_type : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:296
   pragma Import (C, eaccess, "eaccess");

   function faccessat
     (fd : int;
      file : Interfaces.C.Strings.chars_ptr;
      c_type : int;
      flag : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:304
   pragma Import (C, faccessat, "faccessat");

   function lseek
     (fd : int;
      offset : Libc.Sys.Types.off_t;
      whence : int) return Libc.Sys.Types.off_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:334
   pragma Import (C, lseek, "lseek");

   function lseek64
     (fd : int;
      offset : Libc.Sys.Types.off64_t;
      whence : int) return Libc.Sys.Types.off64_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:345
   pragma Import (C, lseek64, "lseek64");

   function close (fd : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:353
   pragma Import (C, close, "close");

   function read
     (fd : int;
      buf : System.Address;
      nbytes : size_t) return Libc.Sys.Types.ssize_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:360
   pragma Import (C, read, "read");

   function write
     (fd : int;
      buf : System.Address;
      n : size_t) return Libc.Sys.Types.ssize_t;  -- /usr/include/unistd.h:366
   pragma Import (C, write, "write");

   function pread
     (fd : int;
      buf : System.Address;
      nbytes : size_t;
      offset : Libc.Sys.Types.off_t) return Libc.Sys.Types.ssize_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:376
   pragma Import (C, pread, "pread");

   function pwrite
     (fd : int;
      buf : System.Address;
      n : size_t;
      offset : Libc.Sys.Types.off_t) return Libc.Sys.Types.ssize_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:384
   pragma Import (C, pwrite, "pwrite");

   function pread64
     (fd : int;
      buf : System.Address;
      nbytes : size_t;
      offset : Libc.Sys.Types.off64_t) return Libc.Sys.Types.ssize_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:404
   pragma Import (C, pread64, "pread64");

   function pwrite64
     (fd : int;
      buf : System.Address;
      n : size_t;
      offset : Libc.Sys.Types.off64_t) return Libc.Sys.Types.ssize_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:408
   pragma Import (C, pwrite64, "pwrite64");

   function pipe (pipedes : access int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:417
   pragma Import (C, pipe, "pipe");

   function pipe2 (pipedes : access int; flags : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:422
   pragma Import (C, pipe2, "pipe2");

   function alarm (seconds : unsigned) return unsigned with
      Spark_Mode => Off;  -- /usr/include/unistd.h:432
   pragma Import (C, alarm, "alarm");

   function sleep (seconds : unsigned) return unsigned with
      Spark_Mode => Off;  -- /usr/include/unistd.h:444
   pragma Import (C, sleep, "sleep");

   function ualarm
     (value : Libc.Sys.Types.useconds_t;
      interval : Libc.Sys.Types.useconds_t)
      return Libc.Sys.Types.useconds_t with
      Spark_Mode => Off;  -- /usr/include/unistd.h:452
   pragma Import (C, ualarm, "ualarm");

   function usleep (useconds : Libc.Sys.Types.useconds_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:460
   pragma Import (C, usleep, "usleep");

   function pause return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:469
   pragma Import (C, pause, "pause");

   function chown
     (file : Interfaces.C.Strings.chars_ptr;
      owner : Libc.Sys.Types.uid_t;
      group : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:473
   pragma Import (C, chown, "chown");

   function fchown
     (fd : int;
      owner : Libc.Sys.Types.uid_t;
      group : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:478
   pragma Import (C, fchown, "fchown");

   function lchown
     (file : Interfaces.C.Strings.chars_ptr;
      owner : Libc.Sys.Types.uid_t;
      group : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:483
   pragma Import (C, lchown, "lchown");

   function fchownat
     (fd : int;
      file : Interfaces.C.Strings.chars_ptr;
      owner : Libc.Sys.Types.uid_t;
      group : Libc.Sys.Types.gid_t;
      flag : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:491
   pragma Import (C, fchownat, "fchownat");

   function chdir (path : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:497
   pragma Import (C, chdir, "chdir");

   function fchdir (fd : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:501
   pragma Import (C, fchdir, "fchdir");

   function getcwd
     (buf : Interfaces.C.Strings.chars_ptr;
      size : size_t) return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:511
   pragma Import (C, getcwd, "getcwd");

   function get_current_dir_name return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:517
   pragma Import (C, get_current_dir_name, "get_current_dir_name");

   function getwd
     (buf : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:525
   pragma Import (C, getwd, "getwd");

   function dup (fd : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:531
   pragma Import (C, dup, "dup");

   function dup2 (fd : int; fd2 : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:534
   pragma Import (C, dup2, "dup2");

   function dup3 (fd : int; fd2 : int; flags : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:539
   pragma Import (C, dup3, "dup3");

   environ : System.Address;  -- /usr/include/unistd.h:545
   pragma Import (C, environ, "environ");

   function execve
     (path : Interfaces.C.Strings.chars_ptr;
      argv : System.Address;
      envp : System.Address) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:551
   pragma Import (C, execve, "execve");

   function fexecve
     (fd : int;
      argv : System.Address;
      envp : System.Address) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:557
   pragma Import (C, fexecve, "fexecve");

   function execv
     (path : Interfaces.C.Strings.chars_ptr;
      argv : System.Address) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:563
   pragma Import (C, execv, "execv");

   function execle
     (path : Interfaces.C.Strings.chars_ptr;
      arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:568
   pragma Import (C, execle, "execle");

   function execl
     (path : Interfaces.C.Strings.chars_ptr;
      arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:573
   pragma Import (C, execl, "execl");

   function execvp
     (file : Interfaces.C.Strings.chars_ptr;
      argv : System.Address) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:578
   pragma Import (C, execvp, "execvp");

   function execlp
     (file : Interfaces.C.Strings.chars_ptr;
      arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:584
   pragma Import (C, execlp, "execlp");

   function execvpe
     (file : Interfaces.C.Strings.chars_ptr;
      argv : System.Address;
      envp : System.Address) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:590
   pragma Import (C, execvpe, "execvpe");

   function nice (inc : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:598
   pragma Import (C, nice, "nice");

   procedure u_exit (status : int) with
      No_Return,
      Spark_Mode => Off;
   pragma Import (C, u_exit, "_exit");

   function pathconf
     (path : Interfaces.C.Strings.chars_ptr;
      name : int) return long with
      Spark_Mode => Off;  -- /usr/include/unistd.h:612
   pragma Import (C, pathconf, "pathconf");

   function fpathconf (fd : int; name : int) return long with
      Spark_Mode => Off;  -- /usr/include/unistd.h:616
   pragma Import (C, fpathconf, "fpathconf");

   function sysconf (name : int) return long with
      Spark_Mode => Off;  -- /usr/include/unistd.h:619
   pragma Import (C, sysconf, "sysconf");

   function confstr
     (name : int;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t) return size_t;  -- /usr/include/unistd.h:623
   pragma Import (C, confstr, "confstr");

   function getpid return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:628
   pragma Import (C, getpid, "getpid");

   function getppid return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:631
   pragma Import (C, getppid, "getppid");

   function getpgrp return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:634
   pragma Import (C, getpgrp, "getpgrp");

   function getpgid
     (pid : Libc.Sys.Types.pid_t)
     return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:639
   pragma Import (C, getpgid, "getpgid");

   function setpgid
     (pid : Libc.Sys.Types.pid_t;
      pgid : Libc.Sys.Types.pid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:646
   pragma Import (C, setpgid, "setpgid");

   function setpgrp return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:660
   pragma Import (C, setpgrp, "setpgrp");

   function setsid return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:667
   pragma Import (C, setsid, "setsid");

   function getsid
     (pid : Libc.Sys.Types.pid_t)
     return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:671
   pragma Import (C, getsid, "getsid");

   function getuid return Libc.Sys.Types.uid_t;  -- /usr/include/unistd.h:675
   pragma Import (C, getuid, "getuid");

   function geteuid return Libc.Sys.Types.uid_t;  -- /usr/include/unistd.h:678
   pragma Import (C, geteuid, "geteuid");

   function getgid return Libc.Sys.Types.gid_t;  -- /usr/include/unistd.h:681
   pragma Import (C, getgid, "getgid");

   function getegid return Libc.Sys.Types.gid_t;  -- /usr/include/unistd.h:684
   pragma Import (C, getegid, "getegid");

   function getgroups
     (size : int;
      list : access Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:689
   pragma Import (C, getgroups, "getgroups");

   function group_member (gid : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:693
   pragma Import (C, group_member, "group_member");

   function setuid (uid : Libc.Sys.Types.uid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:700
   pragma Import (C, setuid, "setuid");

   function setreuid
     (ruid : Libc.Sys.Types.uid_t;
      euid : Libc.Sys.Types.uid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:705
   pragma Import (C, setreuid, "setreuid");

   function seteuid (uid : Libc.Sys.Types.uid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:710
   pragma Import (C, seteuid, "seteuid");

   function setgid (gid : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:717
   pragma Import (C, setgid, "setgid");

   function setregid
     (rgid : Libc.Sys.Types.gid_t;
      egid : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:722
   pragma Import (C, setregid, "setregid");

   function setegid (gid : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:727
   pragma Import (C, setegid, "setegid");

   function getresuid
     (ruid : access Libc.Sys.Types.uid_t;
      euid : access Libc.Sys.Types.uid_t;
      suid : access Libc.Sys.Types.uid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:733
   pragma Import (C, getresuid, "getresuid");

   function getresgid
     (rgid : access Libc.Sys.Types.gid_t;
      egid : access Libc.Sys.Types.gid_t;
      sgid : access Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:738
   pragma Import (C, getresgid, "getresgid");

   function setresuid
     (ruid : Libc.Sys.Types.uid_t;
      euid : Libc.Sys.Types.uid_t;
      suid : Libc.Sys.Types.uid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:743
   pragma Import (C, setresuid, "setresuid");

   function setresgid
     (rgid : Libc.Sys.Types.gid_t;
      egid : Libc.Sys.Types.gid_t;
      sgid : Libc.Sys.Types.gid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:748
   pragma Import (C, setresgid, "setresgid");

   function fork return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:756
   pragma Import (C, fork, "fork");

   function vfork return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:764
   pragma Import (C, vfork, "vfork");

   function ttyname (fd : int) return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:770
   pragma Import (C, ttyname, "ttyname");

   function ttyname_r
     (fd : int;
      buf : Interfaces.C.Strings.chars_ptr;
      buflen : size_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:774
   pragma Import (C, ttyname_r, "ttyname_r");

   function isatty (fd : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:779
   pragma Import (C, isatty, "isatty");

   function ttyslot return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:785
   pragma Import (C, ttyslot, "ttyslot");

   function link
     (from : Interfaces.C.Strings.chars_ptr;
      to : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:790
   pragma Import (C, link, "link");

   function linkat
     (fromfd : int;
      from : Interfaces.C.Strings.chars_ptr;
      tofd : int;
      to : Interfaces.C.Strings.chars_ptr;
      flags : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:796
   pragma Import (C, linkat, "linkat");

   function symlink
     (from : Interfaces.C.Strings.chars_ptr;
      to : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:803
   pragma Import (C, symlink, "symlink");

   function readlink
     (path : Interfaces.C.Strings.chars_ptr;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t)
     return Libc.Sys.Types.ssize_t;  -- /usr/include/unistd.h:809
   pragma Import (C, readlink, "readlink");

   function symlinkat
     (from : Interfaces.C.Strings.chars_ptr;
      tofd : int;
      to : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:816
   pragma Import (C, symlinkat, "symlinkat");

   function readlinkat
     (fd : int;
      path : Interfaces.C.Strings.chars_ptr;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t)
     return Libc.Sys.Types.ssize_t;  -- /usr/include/unistd.h:820
   pragma Import (C, readlinkat, "readlinkat");

   function unlink (name : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:826
   pragma Import (C, unlink, "unlink");

   function unlinkat
     (fd : int;
      name : Interfaces.C.Strings.chars_ptr;
      flag : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:830
   pragma Import (C, unlinkat, "unlinkat");

   function rmdir (path : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:835
   pragma Import (C, rmdir, "rmdir");

   function tcgetpgrp
     (fd : int) return Libc.Sys.Types.pid_t;  -- /usr/include/unistd.h:839
   pragma Import (C, tcgetpgrp, "tcgetpgrp");

   function tcsetpgrp
     (fd : int;
      pgrp_id : Libc.Sys.Types.pid_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:842
   pragma Import (C, tcsetpgrp, "tcsetpgrp");

   function getlogin return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:849
   pragma Import (C, getlogin, "getlogin");

   function getlogin_r
     (name : Interfaces.C.Strings.chars_ptr;
      name_len : size_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:857
   pragma Import (C, getlogin_r, "getlogin_r");

   function setlogin (name : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:862
   pragma Import (C, setlogin, "setlogin");

   function gethostname
     (name : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:879
   pragma Import (C, gethostname, "gethostname");

   function sethostname
     (name : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:886
   pragma Import (C, sethostname, "sethostname");

   function sethostid (id : long) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:891
   pragma Import (C, sethostid, "sethostid");

   function getdomainname
     (name : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:897
   pragma Import (C, getdomainname, "getdomainname");

   function setdomainname
     (name : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:899
   pragma Import (C, setdomainname, "setdomainname");

   function vhangup return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:906
   pragma Import (C, vhangup, "vhangup");

   function revoke (file : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:909
   pragma Import (C, revoke, "revoke");

   function profil
     (sample_buffer : access unsigned_short;
      size : size_t;
      offset : size_t;
      scale : unsigned) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:917
   pragma Import (C, profil, "profil");

   function acct (name : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:925
   pragma Import (C, acct, "acct");

   function getusershell return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:929
   pragma Import (C, getusershell, "getusershell");

   procedure endusershell;  -- /usr/include/unistd.h:930
   pragma Import (C, endusershell, "endusershell");

   procedure setusershell;  -- /usr/include/unistd.h:931
   pragma Import (C, setusershell, "setusershell");

   function daemon (nochdir : int; noclose : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:937
   pragma Import (C, daemon, "daemon");

   function chroot (path : Interfaces.C.Strings.chars_ptr) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:944
   pragma Import (C, chroot, "chroot");

   function getpass
     (prompt : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:948
   pragma Import (C, getpass, "getpass");

   function fsync (fd : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:956
   pragma Import (C, fsync, "fsync");

   function syncfs (fd : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:962
   pragma Import (C, syncfs, "syncfs");

   function gethostid return long with
      Spark_Mode => Off;  -- /usr/include/unistd.h:969
   pragma Import (C, gethostid, "gethostid");

   procedure sync;  -- /usr/include/unistd.h:972
   pragma Import (C, sync, "sync");

   function getpagesize return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:978
   pragma Import (C, getpagesize, "getpagesize");

   function getdtablesize return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:983
   pragma Import (C, getdtablesize, "getdtablesize");

   function truncate
     (file : Interfaces.C.Strings.chars_ptr;
      length : Libc.Sys.Types.off_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:993
   pragma Import (C, truncate, "truncate");

   function truncate64
     (file : Interfaces.C.Strings.chars_ptr;
      length : Libc.Sys.Types.off64_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1005
   pragma Import (C, truncate64, "truncate64");

   function ftruncate (fd : int; length : Libc.Sys.Types.off_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1016
   pragma Import (C, ftruncate, "ftruncate");

   function ftruncate64
     (fd : int;
      length : Libc.Sys.Types.off64_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1026
   pragma Import (C, ftruncate64, "ftruncate64");

   function brk (addr : System.Address) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1037
   pragma Import (C, brk, "brk");

   function sbrk (c_delta : Libc.Stdint.intptr_t) return System.Address with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1043
   pragma Import (C, sbrk, "sbrk");

   function syscall (sysno : long  -- , ...
   ) return long with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1058
   pragma Import (C, syscall, "syscall");

   function lockf
     (fd : int;
      cmd : int;
      len : Libc.Sys.Types.off_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1081
   pragma Import (C, lockf, "lockf");

   function lockf64
     (fd : int;
      cmd : int;
      len : Libc.Sys.Types.off64_t) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1091
   pragma Import (C, lockf64, "lockf64");

   function fdatasync (fildes : int) return int with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1112
   pragma Import (C, fdatasync, "fdatasync");

   function crypt
     (key : Interfaces.C.Strings.chars_ptr;
      salt : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1120
   pragma Import (C, crypt, "crypt");

   procedure encrypt
     (glibc_block : Interfaces.C.Strings.chars_ptr;
      edflag : int) with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1125
   pragma Import (C, encrypt, "encrypt");

   procedure swab
     (from : System.Address;
      to : System.Address;
      n : Libc.Sys.Types.ssize_t) with
      Spark_Mode => Off;  -- /usr/include/unistd.h:1133
   pragma Import (C, swab, "swab");

end Libc.Unistd;
