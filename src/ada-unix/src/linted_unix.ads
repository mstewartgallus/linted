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
with Interfaces.C;
limited with System;

package Linted_Unix is
   pragma Pure;

   type ssize_t is range -2 ** (Standard'Address_Size - 1) ..  2 ** (Standard'Address_Size - 1) - 1
     with Convention => C;
   type mode_t is mod 2 ** 32
     with Convention => C;

   type Oflag is mod 2 ** (32 - 1)
     with Convention => C;

   type pollfd is record
      fd : Interfaces.C.int;
      events : Interfaces.C.short;
      revents : Interfaces.C.short;
   end record
     with Convention => C;

   subtype nfds_t is Interfaces.C.unsigned_long;

   type pollfd_array is array (nfds_t range <>) of aliased pollfd
     with Convention => C;

   subtype off_t is Interfaces.C.unsigned_long;

   O_ACCMODE : constant Oflag := 3;
   O_RDONLY : constant Oflag := 0;
   O_WRONLY : constant Oflag := 1;
   O_RDWR : constant Oflag := 2;

   O_CREAT : constant Oflag := 8#100#;
   O_EXCL : constant Oflag := 8#200#;
   O_NOCTTY : constant Oflag := 8#400#;
   O_TRUNC : constant Oflag := 8#1000#;
   O_APPEND : constant Oflag := 8#2000#;
   O_NONBLOCK : constant Oflag := 8#4000#;

   O_CLOEXEC : constant Oflag := 8#2000000#;

   E2BIG : constant Interfaces.C.int := 7;
   EACCES : constant Interfaces.C.int := 13;
   EADDRINUSE : constant Interfaces.C.int := 98;
   EADDRNOTAVAIL : constant Interfaces.C.int := 99;
   EADV : constant Interfaces.C.int := 68;
   EAFNOSUPPORT : constant Interfaces.C.int := 97;
   EAGAIN : constant Interfaces.C.int := 11;
   EALREADY : constant Interfaces.C.int := 114;
   EBADE : constant Interfaces.C.int := 52;
   EBADF : constant Interfaces.C.int := 9;
   EBADFD : constant Interfaces.C.int := 77;
   EBADMSG : constant Interfaces.C.int := 74;
   EBADR : constant Interfaces.C.int := 53;
   EBADRQC : constant Interfaces.C.int := 56;
   EBADSLT : constant Interfaces.C.int := 57;
   EBFONT : constant Interfaces.C.int := 59;
   EBUSY : constant Interfaces.C.int := 16;
   ECANCELED : constant Interfaces.C.int := 125;
   ECHILD : constant Interfaces.C.int := 10;
   ECHRNG : constant Interfaces.C.int := 44;
   ECOMM : constant Interfaces.C.int := 70;
   ECONNABORTED : constant Interfaces.C.int := 103;
   ECONNREFUSED : constant Interfaces.C.int := 111;
   ECONNRESET : constant Interfaces.C.int := 104;
   EDEADLK : constant Interfaces.C.int := 35;
   EDEADLOCK : constant Interfaces.C.int := EDEADLK;
   EDESTADDRREQ : constant Interfaces.C.int := 89;
   EDOM : constant Interfaces.C.int := 33;
   EDOTDOT : constant Interfaces.C.int := 73;
   EDQUOT : constant Interfaces.C.int := 122;
   EEXIST : constant Interfaces.C.int := 17;
   EFAULT : constant Interfaces.C.int := 14;
   EFBIG : constant Interfaces.C.int := 27;
   EHOSTDOWN : constant Interfaces.C.int := 112;
   EHOSTUNREACH : constant Interfaces.C.int := 113;
   EHWPOISON : constant Interfaces.C.int := 133;
   EIDRM : constant Interfaces.C.int := 43;
   EILSEQ : constant Interfaces.C.int := 84;
   EINPROGRESS : constant Interfaces.C.int := 115;
   EINTR : constant Interfaces.C.int := 4;
   EINVAL : constant Interfaces.C.int := 22;
   EIO : constant Interfaces.C.int := 5;
   EISCONN : constant Interfaces.C.int := 106;
   EISDIR : constant Interfaces.C.int := 21;
   EISNAM : constant Interfaces.C.int := 120;
   EKEYEXPIRED : constant Interfaces.C.int := 127;
   EKEYREJECTED : constant Interfaces.C.int := 129;
   EKEYREVOKED : constant Interfaces.C.int := 128;
   EL2HLT : constant Interfaces.C.int := 51;
   EL2NSYNC : constant Interfaces.C.int := 45;
   EL3HLT : constant Interfaces.C.int := 46;
   EL3RST : constant Interfaces.C.int := 47;
   ELIBACC : constant Interfaces.C.int := 79;
   ELIBBAD : constant Interfaces.C.int := 80;
   ELIBEXEC : constant Interfaces.C.int := 83;
   ELIBMAX : constant Interfaces.C.int := 82;
   ELIBSCN : constant Interfaces.C.int := 81;
   ELNRNG : constant Interfaces.C.int := 48;
   ELOOP : constant Interfaces.C.int := 40;
   EMEDIUMTYPE : constant Interfaces.C.int := 124;
   EMFILE : constant Interfaces.C.int := 24;
   EMLINK : constant Interfaces.C.int := 31;
   EMSGSIZE : constant Interfaces.C.int := 90;
   EMULTIHOP : constant Interfaces.C.int := 72;
   ENAMETOOLONG : constant Interfaces.C.int := 36;
   ENAVAIL : constant Interfaces.C.int := 119;
   ENETDOWN : constant Interfaces.C.int := 100;
   ENETRESET : constant Interfaces.C.int := 102;
   ENETUNREACH : constant Interfaces.C.int := 101;
   ENFILE : constant Interfaces.C.int := 23;
   ENOANO : constant Interfaces.C.int := 55;
   ENOBUFS : constant Interfaces.C.int := 105;
   ENOCSI : constant Interfaces.C.int := 50;
   ENODATA : constant Interfaces.C.int := 61;
   ENODEV : constant Interfaces.C.int := 19;
   ENOENT : constant Interfaces.C.int := 2;
   ENOEXEC : constant Interfaces.C.int := 8;
   ENOKEY : constant Interfaces.C.int := 126;
   ENOLCK : constant Interfaces.C.int := 37;
   ENOLINK : constant Interfaces.C.int := 67;
   ENOMEDIUM : constant Interfaces.C.int := 123;
   ENOMEM : constant Interfaces.C.int := 12;
   ENOMSG : constant Interfaces.C.int := 42;
   ENONET : constant Interfaces.C.int := 64;
   ENOPKG : constant Interfaces.C.int := 65;
   ENOPROTOOPT : constant Interfaces.C.int := 92;
   ENOSPC : constant Interfaces.C.int := 28;
   ENOSR : constant Interfaces.C.int := 63;
   ENOSTR : constant Interfaces.C.int := 60;
   ENOSYS : constant Interfaces.C.int := 38;
   ENOTBLK : constant Interfaces.C.int := 15;
   ENOTCONN : constant Interfaces.C.int := 107;
   ENOTDIR : constant Interfaces.C.int := 20;
   ENOTEMPTY : constant Interfaces.C.int := 39;
   ENOTNAM : constant Interfaces.C.int := 118;
   ENOTRECOVERABLE : constant Interfaces.C.int := 131;
   ENOTSOCK : constant Interfaces.C.int := 88;
   ENOTTY : constant Interfaces.C.int := 25;
   ENOTUNIQ : constant Interfaces.C.int := 76;
   ENXIO : constant Interfaces.C.int := 6;
   EOPNOTSUPP : constant Interfaces.C.int := 95;
   EOVERFLOW : constant Interfaces.C.int := 75;
   EOWNERDEAD : constant Interfaces.C.int := 130;
   EPERM : constant Interfaces.C.int := 1;
   EPFNOSUPPORT : constant Interfaces.C.int := 96;
   EPIPE : constant Interfaces.C.int := 32;
   EPROTO : constant Interfaces.C.int := 71;
   EPROTONOSUPPORT : constant Interfaces.C.int := 93;
   EPROTOTYPE : constant Interfaces.C.int := 91;
   ERANGE : constant Interfaces.C.int := 34;
   EREMCHG : constant Interfaces.C.int := 78;
   EREMOTE : constant Interfaces.C.int := 66;
   EREMOTEIO : constant Interfaces.C.int := 121;
   ERESTART : constant Interfaces.C.int := 85;
   ERFKILL : constant Interfaces.C.int := 132;
   EROFS : constant Interfaces.C.int := 30;
   ESHUTDOWN : constant Interfaces.C.int := 108;
   ESOCKTNOSUPPORT : constant Interfaces.C.int := 94;
   ESPIPE : constant Interfaces.C.int := 29;
   ESRCH : constant Interfaces.C.int := 3;
   ESRMNT : constant Interfaces.C.int := 69;
   ESTALE : constant Interfaces.C.int := 116;
   ESTRPIPE : constant Interfaces.C.int := 86;
   ETIME : constant Interfaces.C.int := 62;
   ETIMEDOUT : constant Interfaces.C.int := 110;
   ETOOMANYREFS : constant Interfaces.C.int := 109;
   ETXTBSY : constant Interfaces.C.int := 26;
   EUCLEAN : constant Interfaces.C.int := 117;
   EUNATCH : constant Interfaces.C.int := 49;
   EUSERS : constant Interfaces.C.int := 87;
   EWOULDBLOCK : constant Interfaces.C.int := EAGAIN;
   EXDEV : constant Interfaces.C.int := 18;
   EXFULL : constant Interfaces.C.int := 54;

   function Open (Pathname : Interfaces.C.char_array; Flags : Oflag; Mode : mode_t) return Interfaces.C.int;

   function Close (Object : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Close, "close");

   function Poll (Fds : in out pollfd_array; Nfds : nfds_t; Timeout : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Poll, "poll");

   function PRead (Fd : Interfaces.C.int;
		   Buf : System.Address;
		   Count : Interfaces.C.size_t;
		   Offset : off_t) return ssize_t;
   pragma Import (C, PRead, "pread");

   function Write (Fd : Interfaces.C.int; Buf : System.Address; Count : Interfaces.C.size_t) return ssize_t;
   pragma Import (C, Write, "write");

   function Read (Fd : Interfaces.C.int; Buf : System.Address; Count : Interfaces.C.size_t) return ssize_t;
   pragma Import (C, Read, "read");

   procedure Quick_Exit (Status : Interfaces.C.int);
   pragma Import (C, Quick_Exit, "_Exit");
end Linted_Unix;
