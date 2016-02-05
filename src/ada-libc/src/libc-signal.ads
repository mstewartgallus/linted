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

package Libc.Signal is
   pragma Pure;

   subtype sig_atomic_t is int;  -- /usr/include/signal.h:40

   -- SIG_DFL
   -- SIG_ERR
   -- SIG_IGN

   -- SIGABRT
   -- SIGFPE
   -- SIGILL
   -- SIGINT
   -- SIGSEGV
   -- SIGTERM
   type sighandler_t is access procedure (arg1 : int);
   pragma Convention (C, sighandler_t);  -- /usr/include/signal.h:85

   function signal (sig : int; handler : sighandler_t) return sighandler_t;  -- /usr/include/signal.h:102
   pragma Import (C, signal, "signal");

   function c_raise (sig : int) return int;  -- /usr/include/signal.h:139
   pragma Import (C, c_raise, "raise");
end Libc.Signal;
