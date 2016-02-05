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
package Libc.Errno.POSIX_2008 is
   pragma Pure;

   E2BIG : constant := 7;
   EBADF : constant := 9;
   ECHILD : constant := 10;
   EAGAIN : constant := 11;
   EACCES : constant := 13;
   EFAULT : constant := 14;
   EFBIG : constant := 27;
   EHOSTUNREACH : constant := 113;
   EIDRM : constant := 43;
   EINPROGRESS : constant := 115;
   EINTR : constant := 4;
   EINVAL : constant := 22;
   EIO : constant := 5;
   EISCONN : constant := 106;
   EISDIR : constant := 21;
   ELOOP : constant := 40;
   EMFILE : constant := 24;
   EMLINK : constant := 31;
   EMSGSIZE : constant := 90;
   EMULTIHOP : constant := 72;
   ENAMETOOLONG : constant := 36;
   ENETDOWN : constant := 100;
   ENETRESET : constant := 102;
   ENETUNREACH : constant := 101;
   ENFILE : constant := 23;
   ENOBUFS : constant := 105;
   ENODATA : constant := 61;
   ENODEV : constant := 19;
   ENOENT : constant := 2;
   ENOEXEC : constant := 8;
   ENOLCK : constant := 37;
   ENOLINK : constant := 67;
   ENOMEM : constant := 12;
   ENOMSG : constant := 42;
   ENOPROTOOPT : constant := 92;
   ENOSPC : constant := 28;
   ENOSR : constant := 63;
   ENOSTR : constant := 60;
   ENOSYS : constant := 38;
   ENOTCONN : constant := 107;
   ENOTDIR : constant := 20;
   ENOTEMPTY : constant := 39;
   ENOTRECOVERABLE : constant := 131;
   ENOTSOCK : constant := 88;
   EOPNOTSUPP : constant := 95;
   ENOTTY : constant := 25;
   ENXIO : constant := 6;
   EOVERFLOW : constant := 75;
   EOWNERDEAD : constant := 130;
   EPERM : constant := 1;
   EPIPE : constant := 32;
   EPROTO : constant := 71;
   EPROTONOSUPPORT : constant := 93;
   EPROTOTYPE : constant := 91;
   EROFS : constant := 30;
   ESPIPE : constant := 29;
   ESRCH : constant := 3;
   ESTALE : constant := 116;
   ETIME : constant := 62;
   ETIMEDOUT : constant := 110;
   ETXTBSY : constant := 26;
   EWOULDBLOCK : constant := EAGAIN;
   EXDEV : constant := 18;
   EBUSY : constant := 16;
   EEXIST : constant := 17;
   EDEADLK : constant := 35;
   EBADMSG : constant := 74;
   EDESTADDRREQ : constant := 89;
   EAFNOSUPPORT : constant := 97;
   EADDRINUSE : constant := 98;
   ECONNABORTED : constant := 103;
   ECONNRESET : constant := 104;
   ECONNREFUSED : constant := 111;
   EALREADY : constant := 114;
   EDQUOT : constant := 122;
   ECANCELED : constant := 125;
end Libc.Errno.POSIX_2008;
