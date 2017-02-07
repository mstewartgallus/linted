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
with Libc.Signal.GNU;
with Libc.Time.GNU;

package Libc.Sys.Poll with
     Spark_Mode => Off is

   subtype nfds_t is unsigned_long;  -- /usr/include/sys/poll.h:36

   type pollfd is record
      fd : int;  -- /usr/include/sys/poll.h:41
      events : short;  -- /usr/include/sys/poll.h:42
      revents : short;  -- /usr/include/sys/poll.h:43
   end record;

   function poll
     (fds : in out pollfd;
      nfds : nfds_t;
      timeout : int) return int;  -- /usr/include/sys/poll.h:57
   pragma Import (C, poll, "poll");

   function ppoll
     (fds : in out pollfd;
      nfds : nfds_t;
      timeout : access constant Libc.Time.GNU.timespec;
      ss : access constant Libc.Signal.GNU.sigset_t)
     return int;  -- /usr/include/sys/poll.h:66
   pragma Import (C, ppoll, "ppoll");

   POLLIN : constant := 16#001#;
   POLLPRI : constant := 16#002#;
   POLLOUT : constant := 16#004#;
   POLLERR : constant := 16#008#;
   POLLHUP : constant := 16#010#;
   POLLNVAL : constant := 16#020#;

end Libc.Sys.Poll;
