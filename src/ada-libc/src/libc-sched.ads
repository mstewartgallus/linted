-- Copyright 2017 Steven Stewart-Gallus
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
package Libc.Sched is
   pragma Preelaborate;

--  unsupported macro: sched_priority __sched_priority
--  unsupported macro: CPU_SETSIZE __CPU_SETSIZE
--  arg-macro: procedure CPU_SET __CPU_SET_S (cpu, sizeof (cpu_set_t), cpusetp)
   --    __CPU_SET_S (cpu, sizeof (cpu_set_t), cpusetp)
--  arg-macro: procedure CPU_CLR __CPU_CLR_S (cpu, sizeof (cpu_set_t), cpusetp)
   --    __CPU_CLR_S (cpu, sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_ISSET __CPU_ISSET_S (cpu, sizeof (cpu_set_t), cpusetp)
   --    __CPU_ISSET_S (cpu, sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_ZERO __CPU_ZERO_S (sizeof (cpu_set_t), cpusetp)
   --    __CPU_ZERO_S (sizeof (cpu_set_t), cpusetp)
--  arg-macro: procedure CPU_COUNT __CPU_COUNT_S (sizeof (cpu_set_t), cpusetp)
   --    __CPU_COUNT_S (sizeof (cpu_set_t), cpusetp)
   --  arg-macro: procedure CPU_SET_S __CPU_SET_S (cpu, setsize, cpusetp)
   --    __CPU_SET_S (cpu, setsize, cpusetp)
   --  arg-macro: procedure CPU_CLR_S __CPU_CLR_S (cpu, setsize, cpusetp)
   --    __CPU_CLR_S (cpu, setsize, cpusetp)
   --  arg-macro: procedure CPU_ISSET_S __CPU_ISSET_S (cpu, setsize, cpusetp)
   --    __CPU_ISSET_S (cpu, setsize, cpusetp)
   --  arg-macro: procedure CPU_ZERO_S __CPU_ZERO_S (setsize, cpusetp)
   --    __CPU_ZERO_S (setsize, cpusetp)
   --  arg-macro: procedure CPU_COUNT_S __CPU_COUNT_S (setsize, cpusetp)
   --    __CPU_COUNT_S (setsize, cpusetp)
   --  arg-macro: procedure CPU_EQUAL __CPU_EQUAL_S (sizeof (cpu_set_t), cpusetp1, cpusetp2)
   --    __CPU_EQUAL_S (sizeof (cpu_set_t), cpusetp1, cpusetp2)
   --  arg-macro: procedure CPU_EQUAL_S __CPU_EQUAL_S (setsize, cpusetp1, cpusetp2)
   --    __CPU_EQUAL_S (setsize, cpusetp1, cpusetp2)
   --  arg-macro: procedure CPU_AND __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, and)
   --    __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, and)
   --  arg-macro: procedure CPU_OR __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, or)
   --    __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, or)
   --  arg-macro: procedure CPU_XOR __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, xor)
   --    __CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, xor)
   --  arg-macro: procedure CPU_AND_S __CPU_OP_S (setsize, destset, srcset1, srcset2, and)
   --    __CPU_OP_S (setsize, destset, srcset1, srcset2, and)
   --  arg-macro: procedure CPU_OR_S __CPU_OP_S (setsize, destset, srcset1, srcset2, or)
   --    __CPU_OP_S (setsize, destset, srcset1, srcset2, or)
   --  arg-macro: procedure CPU_XOR_S __CPU_OP_S (setsize, destset, srcset1, srcset2, xor)
   --    __CPU_OP_S (setsize, destset, srcset1, srcset2, xor)
   --  arg-macro: procedure CPU_ALLOC_SIZE __CPU_ALLOC_SIZE (count)
   --    __CPU_ALLOC_SIZE (count)
   --  arg-macro: procedure CPU_ALLOC __CPU_ALLOC (count)
   --    __CPU_ALLOC (count)
   --  arg-macro: procedure CPU_FREE __CPU_FREE (cpuset)
   --    __CPU_FREE (cpuset)
   --  subtype pid_t is x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/sched.h:35

   --  function sched_setparam (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_param : access constant x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/sched.h:49
   --  pragma Import (C, sched_setparam, "sched_setparam");

   --  function sched_getparam (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_param : access x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/sched.h:53
   --  pragma Import (C, sched_getparam, "sched_getparam");

   --  function sched_setscheduler
   --    (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
   --     uu_policy : int;
   --     uu_param : access constant x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/sched.h:56
   --  pragma Import (C, sched_setscheduler, "sched_setscheduler");

   --  function sched_getscheduler (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return int;  -- /usr/include/sched.h:60
   --  pragma Import (C, sched_getscheduler, "sched_getscheduler");

   procedure sched_yield;  -- /usr/include/sched.h:63
   pragma Import (C, sched_yield, "sched_yield");

   --  function sched_get_priority_max (uu_algorithm : int) return int;  -- /usr/include/sched.h:66
   --  pragma Import (C, sched_get_priority_max, "sched_get_priority_max");

   --  function sched_get_priority_min (uu_algorithm : int) return int;  -- /usr/include/sched.h:69
   --  pragma Import (C, sched_get_priority_min, "sched_get_priority_min");

   --  function sched_rr_get_interval (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_t : access time_h.timespec) return int;  -- /usr/include/sched.h:72
   --  pragma Import (C, sched_rr_get_interval, "sched_rr_get_interval");

   --  function sched_setaffinity
   --    (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
   --     uu_cpusetsize : stddef_h.size_t;
   --     uu_cpuset : access constant x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int;  -- /usr/include/sched.h:116
   --  pragma Import (C, sched_setaffinity, "sched_setaffinity");

   --  function sched_getaffinity
   --    (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
   --     uu_cpusetsize : stddef_h.size_t;
   --     uu_cpuset : access x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int;  -- /usr/include/sched.h:120
   --  pragma Import (C, sched_getaffinity, "sched_getaffinity");

end Libc.Sched;
