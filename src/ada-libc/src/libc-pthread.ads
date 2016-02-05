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
with System;
limited with Libc.Time;
limited with Libc.x86_64_linux_gnu_bits_sched_h;
with Interfaces.C.Strings;
with Libc.x86_64_linux_gnu_bits_setjmp_h;
with Libc.x86_64_linux_gnu_bits_types_h;

package Libc.Pthread is
   pragma Preelaborate;
   --  unsupported macro: PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_JOINABLE
   --  unsupported macro: PTHREAD_CREATE_DETACHED PTHREAD_CREATE_DETACHED
   --  unsupported macro: PTHREAD_MUTEX_INITIALIZER { { 0, 0, 0, 0, 0, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_RECURSIVE_NP, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_ERRORCHECK_NP, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_ADAPTIVE_NP, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_RWLOCK_INITIALIZER { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } }
   --  unsupported macro: PTHREAD_RWLOCK_WRITER_NONRECURSIVE_INITIALIZER_NP { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP } }
   --  unsupported macro: PTHREAD_INHERIT_SCHED PTHREAD_INHERIT_SCHED
   --  unsupported macro: PTHREAD_EXPLICIT_SCHED PTHREAD_EXPLICIT_SCHED
   --  unsupported macro: PTHREAD_SCOPE_SYSTEM PTHREAD_SCOPE_SYSTEM
   --  unsupported macro: PTHREAD_SCOPE_PROCESS PTHREAD_SCOPE_PROCESS
   --  unsupported macro: PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_PRIVATE
   --  unsupported macro: PTHREAD_PROCESS_SHARED PTHREAD_PROCESS_SHARED
   --  unsupported macro: PTHREAD_COND_INITIALIZER { { 0, 0, 0, 0, 0, (void *) 0, 0, 0 } }
   --  unsupported macro: PTHREAD_CANCEL_ENABLE PTHREAD_CANCEL_ENABLE
   --  unsupported macro: PTHREAD_CANCEL_DISABLE PTHREAD_CANCEL_DISABLE
   --  unsupported macro: PTHREAD_CANCEL_DEFERRED PTHREAD_CANCEL_DEFERRED
   --  unsupported macro: PTHREAD_CANCEL_ASYNCHRONOUS PTHREAD_CANCEL_ASYNCHRONOUS
   --  unsupported macro: PTHREAD_CANCELED ((void *) -1)
   --  unsupported macro: PTHREAD_ONCE_INIT 0
   --  unsupported macro: PTHREAD_BARRIER_SERIAL_THREAD -1
   --  arg-macro: procedure pthread_cleanup_push (routine, arg)
   --    do { __pthread_cleanup_class __clframe (routine, arg)
   --  arg-macro: procedure pthread_cleanup_pop (execute)
   --    __clframe.__setdoit (execute); } while (0)
   --  arg-macro: procedure pthread_cleanup_push_defer_np (routine, arg)
   --    do { __pthread_cleanup_class __clframe (routine, arg); __clframe.__defer ()
   --  arg-macro: procedure pthread_cleanup_pop_restore_np (execute)
   --    __clframe.__restore (); __clframe.__setdoit (execute); } while (0)
   type u_pthread_cleanup_buffer is record
      routine : access procedure (arg1 : System.Address);  -- /usr/include/pthread.h:203
      arg : System.Address;  -- /usr/include/pthread.h:204
      canceltype : aliased int;  -- /usr/include/pthread.h:205
      prev : access u_pthread_cleanup_buffer;  -- /usr/include/pthread.h:206
   end record;
   pragma Convention (C_Pass_By_Copy, u_pthread_cleanup_buffer);  -- /usr/include/pthread.h:201

   function pthread_create
     (newthread : access pthread_t;
      attr : access constant pthread_attr_t;
      start_routine : access function (arg1 : System.Address) return System.Address;
      arg : System.Address) return int;  -- /usr/include/pthread.h:244
   pragma Import (C, pthread_create, "pthread_create");

   procedure pthread_exit (retval : System.Address);  -- /usr/include/pthread.h:253
   pragma Import (C, pthread_exit, "pthread_exit");

   function pthread_join (th : pthread_t; thread_return : System.Address) return int;  -- /usr/include/pthread.h:261
   pragma Import (C, pthread_join, "pthread_join");

   function pthread_tryjoin_np (th : pthread_t; thread_return : System.Address) return int;  -- /usr/include/pthread.h:266
   pragma Import (C, pthread_tryjoin_np, "pthread_tryjoin_np");

   function pthread_timedjoin_np
     (th : pthread_t;
      thread_return : System.Address;
      abstime : access constant Libc.Time.timespec) return int;  -- /usr/include/pthread.h:274
   pragma Import (C, pthread_timedjoin_np, "pthread_timedjoin_np");

   function pthread_detach (th : pthread_t) return int;  -- /usr/include/pthread.h:282
   pragma Import (C, pthread_detach, "pthread_detach");

   function pthread_self return pthread_t;  -- /usr/include/pthread.h:286
   pragma Import (C, pthread_self, "pthread_self");

   function pthread_equal (thread1 : pthread_t; thread2 : pthread_t) return int;  -- /usr/include/pthread.h:289
   pragma Import (C, pthread_equal, "pthread_equal");

   function pthread_attr_init (attr : access pthread_attr_t) return int;  -- /usr/include/pthread.h:298
   pragma Import (C, pthread_attr_init, "pthread_attr_init");

   function pthread_attr_destroy (attr : access pthread_attr_t) return int;  -- /usr/include/pthread.h:301
   pragma Import (C, pthread_attr_destroy, "pthread_attr_destroy");

   function pthread_attr_getdetachstate (attr : access constant pthread_attr_t; detachstate : access int) return int;  -- /usr/include/pthread.h:305
   pragma Import (C, pthread_attr_getdetachstate, "pthread_attr_getdetachstate");

   function pthread_attr_setdetachstate (attr : access pthread_attr_t; detachstate : int) return int;  -- /usr/include/pthread.h:310
   pragma Import (C, pthread_attr_setdetachstate, "pthread_attr_setdetachstate");

   function pthread_attr_getguardsize (attr : access constant pthread_attr_t; guardsize : access size_t) return int;  -- /usr/include/pthread.h:316
   pragma Import (C, pthread_attr_getguardsize, "pthread_attr_getguardsize");

   function pthread_attr_setguardsize (attr : access pthread_attr_t; guardsize : size_t) return int;  -- /usr/include/pthread.h:321
   pragma Import (C, pthread_attr_setguardsize, "pthread_attr_setguardsize");

   function pthread_attr_getschedparam (attr : access constant pthread_attr_t; param : access Libc.x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/pthread.h:327
   pragma Import (C, pthread_attr_getschedparam, "pthread_attr_getschedparam");

   function pthread_attr_setschedparam (attr : access pthread_attr_t; param : access constant Libc.x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/pthread.h:332
   pragma Import (C, pthread_attr_setschedparam, "pthread_attr_setschedparam");

   function pthread_attr_getschedpolicy (attr : access constant pthread_attr_t; policy : access int) return int;  -- /usr/include/pthread.h:337
   pragma Import (C, pthread_attr_getschedpolicy, "pthread_attr_getschedpolicy");

   function pthread_attr_setschedpolicy (attr : access pthread_attr_t; policy : int) return int;  -- /usr/include/pthread.h:342
   pragma Import (C, pthread_attr_setschedpolicy, "pthread_attr_setschedpolicy");

   function pthread_attr_getinheritsched (attr : access constant pthread_attr_t; inherit : access int) return int;  -- /usr/include/pthread.h:346
   pragma Import (C, pthread_attr_getinheritsched, "pthread_attr_getinheritsched");

   function pthread_attr_setinheritsched (attr : access pthread_attr_t; inherit : int) return int;  -- /usr/include/pthread.h:351
   pragma Import (C, pthread_attr_setinheritsched, "pthread_attr_setinheritsched");

   function pthread_attr_getscope (attr : access constant pthread_attr_t; scope : access int) return int;  -- /usr/include/pthread.h:357
   pragma Import (C, pthread_attr_getscope, "pthread_attr_getscope");

   function pthread_attr_setscope (attr : access pthread_attr_t; scope : int) return int;  -- /usr/include/pthread.h:362
   pragma Import (C, pthread_attr_setscope, "pthread_attr_setscope");

   function pthread_attr_getstackaddr (attr : access constant pthread_attr_t; stackaddr : System.Address) return int;  -- /usr/include/pthread.h:366
   pragma Import (C, pthread_attr_getstackaddr, "pthread_attr_getstackaddr");

   function pthread_attr_setstackaddr (attr : access pthread_attr_t; stackaddr : System.Address) return int;  -- /usr/include/pthread.h:374
   pragma Import (C, pthread_attr_setstackaddr, "pthread_attr_setstackaddr");

   function pthread_attr_getstacksize (attr : access constant pthread_attr_t; stacksize : access size_t) return int;  -- /usr/include/pthread.h:379
   pragma Import (C, pthread_attr_getstacksize, "pthread_attr_getstacksize");

   function pthread_attr_setstacksize (attr : access pthread_attr_t; stacksize : size_t) return int;  -- /usr/include/pthread.h:386
   pragma Import (C, pthread_attr_setstacksize, "pthread_attr_setstacksize");

   function pthread_attr_getstack
     (attr : access constant pthread_attr_t;
      stackaddr : System.Address;
      stacksize : access size_t) return int;  -- /usr/include/pthread.h:392
   pragma Import (C, pthread_attr_getstack, "pthread_attr_getstack");

   function pthread_attr_setstack
     (attr : access pthread_attr_t;
      stackaddr : System.Address;
      stacksize : size_t) return int;  -- /usr/include/pthread.h:400
   pragma Import (C, pthread_attr_setstack, "pthread_attr_setstack");

   function pthread_attr_setaffinity_np
     (attr : access pthread_attr_t;
      cpusetsize : size_t;
      cpuset : access constant Libc.x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int;  -- /usr/include/pthread.h:407
   pragma Import (C, pthread_attr_setaffinity_np, "pthread_attr_setaffinity_np");

   function pthread_attr_getaffinity_np
     (attr : access constant pthread_attr_t;
      cpusetsize : size_t;
      cpuset : access Libc.x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int;  -- /usr/include/pthread.h:414
   pragma Import (C, pthread_attr_getaffinity_np, "pthread_attr_getaffinity_np");

   function pthread_getattr_default_np (attr : access pthread_attr_t) return int;  -- /usr/include/pthread.h:420
   pragma Import (C, pthread_getattr_default_np, "pthread_getattr_default_np");

   function pthread_setattr_default_np (attr : access constant pthread_attr_t) return int;  -- /usr/include/pthread.h:425
   pragma Import (C, pthread_setattr_default_np, "pthread_setattr_default_np");

   function pthread_getattr_np (th : pthread_t; attr : access pthread_attr_t) return int;  -- /usr/include/pthread.h:431
   pragma Import (C, pthread_getattr_np, "pthread_getattr_np");

   function pthread_setschedparam
     (target_thread : pthread_t;
      policy : int;
      param : access constant Libc.x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/pthread.h:440
   pragma Import (C, pthread_setschedparam, "pthread_setschedparam");

   function pthread_getschedparam
     (target_thread : pthread_t;
      policy : access int;
      param : access Libc.x86_64_linux_gnu_bits_sched_h.sched_param) return int;  -- /usr/include/pthread.h:445
   pragma Import (C, pthread_getschedparam, "pthread_getschedparam");

   function pthread_setschedprio (target_thread : pthread_t; prio : int) return int;  -- /usr/include/pthread.h:451
   pragma Import (C, pthread_setschedprio, "pthread_setschedprio");

   function pthread_getname_np
     (target_thread : pthread_t;
      buf : Interfaces.C.Strings.chars_ptr;
      buflen : size_t) return int;  -- /usr/include/pthread.h:457
   pragma Import (C, pthread_getname_np, "pthread_getname_np");

   function pthread_setname_np (target_thread : pthread_t; name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/pthread.h:462
   pragma Import (C, pthread_setname_np, "pthread_setname_np");

   function pthread_getconcurrency return int;  -- /usr/include/pthread.h:469
   pragma Import (C, pthread_getconcurrency, "pthread_getconcurrency");

   function pthread_setconcurrency (level : int) return int;  -- /usr/include/pthread.h:472
   pragma Import (C, pthread_setconcurrency, "pthread_setconcurrency");

   function pthread_yield return int;  -- /usr/include/pthread.h:480
   pragma Import (C, pthread_yield, "pthread_yield");

   function pthread_setaffinity_np
     (th : pthread_t;
      cpusetsize : size_t;
      cpuset : access constant Libc.x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int;  -- /usr/include/pthread.h:485
   pragma Import (C, pthread_setaffinity_np, "pthread_setaffinity_np");

   function pthread_getaffinity_np
     (th : pthread_t;
      cpusetsize : size_t;
      cpuset : access Libc.x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int;  -- /usr/include/pthread.h:490
   pragma Import (C, pthread_getaffinity_np, "pthread_getaffinity_np");

   function pthread_once (once_control : access pthread_once_t; init_routine : access procedure) return int;  -- /usr/include/pthread.h:505
   pragma Import (C, pthread_once, "pthread_once");

   function pthread_setcancelstate (state : int; oldstate : access int) return int;  -- /usr/include/pthread.h:517
   pragma Import (C, pthread_setcancelstate, "pthread_setcancelstate");

   function pthread_setcanceltype (c_type : int; oldtype : access int) return int;  -- /usr/include/pthread.h:521
   pragma Import (C, pthread_setcanceltype, "pthread_setcanceltype");

   function pthread_cancel (th : pthread_t) return int;  -- /usr/include/pthread.h:524
   pragma Import (C, pthread_cancel, "pthread_cancel");

   procedure pthread_testcancel;  -- /usr/include/pthread.h:529
   pragma Import (C, pthread_testcancel, "pthread_testcancel");

   type pthread_unwind_buf_t_cancel_jmp_buf_array is array (0 .. 0) of aliased anon_23;
   type pthread_unwind_buf_t_pad_array is array (0 .. 3) of System.Address;
   type pthread_unwind_buf_t;
   type anon_23 is record
      cancel_jmp_buf : aliased Libc.x86_64_linux_gnu_bits_setjmp_h.jmp_buf;  -- /usr/include/pthread.h:538
      mask_was_saved : aliased int;  -- /usr/include/pthread.h:539
   end record;
   pragma Convention (C_Pass_By_Copy, anon_23);
   type pthread_unwind_buf_t is record
      cancel_jmp_buf : aliased pthread_unwind_buf_t_cancel_jmp_buf_array;  -- /usr/include/pthread.h:540
      pad : aliased pthread_unwind_buf_t_pad_array;  -- /usr/include/pthread.h:541
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_unwind_buf_t);  -- /usr/include/pthread.h:542

   --  skipped anonymous struct anon_22

   type pthread_cleanup_frame is record
      cancel_routine : access procedure (arg1 : System.Address);  -- /usr/include/pthread.h:553
      cancel_arg : System.Address;  -- /usr/include/pthread.h:554
      do_it : aliased int;  -- /usr/include/pthread.h:555
      cancel_type : aliased int;  -- /usr/include/pthread.h:556
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_cleanup_frame);  -- /usr/include/pthread.h:551

   package Class_pthread_cleanup_class is
      type pthread_cleanup_class is limited record
         cancel_routine : access procedure (arg1 : System.Address);  -- /usr/include/pthread.h:564
         cancel_arg : System.Address;  -- /usr/include/pthread.h:565
         do_it : aliased int;  -- /usr/include/pthread.h:566
         cancel_type : aliased int;  -- /usr/include/pthread.h:567
      end record;
      pragma Import (CPP, pthread_cleanup_class);


   end;
   use Class_pthread_cleanup_class;
   --  skipped empty struct jmp_buf_tag

   function pthread_mutex_init (mutex : access pthread_mutex_t; mutexattr : access constant pthread_mutexattr_t) return int;  -- /usr/include/pthread.h:760
   pragma Import (C, pthread_mutex_init, "pthread_mutex_init");

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:765
   pragma Import (C, pthread_mutex_destroy, "pthread_mutex_destroy");

   function pthread_mutex_trylock (mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:769
   pragma Import (C, pthread_mutex_trylock, "pthread_mutex_trylock");

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:773
   pragma Import (C, pthread_mutex_lock, "pthread_mutex_lock");

   function pthread_mutex_timedlock (mutex : access pthread_mutex_t; abstime : access constant Libc.Time.timespec) return int;  -- /usr/include/pthread.h:778
   pragma Import (C, pthread_mutex_timedlock, "pthread_mutex_timedlock");

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:784
   pragma Import (C, pthread_mutex_unlock, "pthread_mutex_unlock");

   function pthread_mutex_getprioceiling (mutex : access constant pthread_mutex_t; prioceiling : access int) return int;  -- /usr/include/pthread.h:789
   pragma Import (C, pthread_mutex_getprioceiling, "pthread_mutex_getprioceiling");

   function pthread_mutex_setprioceiling
     (mutex : access pthread_mutex_t;
      prioceiling : int;
      old_ceiling : access int) return int;  -- /usr/include/pthread.h:796
   pragma Import (C, pthread_mutex_setprioceiling, "pthread_mutex_setprioceiling");

   function pthread_mutex_consistent (mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:804
   pragma Import (C, pthread_mutex_consistent, "pthread_mutex_consistent");

   function pthread_mutex_consistent_np (mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:807
   pragma Import (C, pthread_mutex_consistent_np, "pthread_mutex_consistent_np");

   function pthread_mutexattr_init (attr : access pthread_mutexattr_t) return int;  -- /usr/include/pthread.h:817
   pragma Import (C, pthread_mutexattr_init, "pthread_mutexattr_init");

   function pthread_mutexattr_destroy (attr : access pthread_mutexattr_t) return int;  -- /usr/include/pthread.h:821
   pragma Import (C, pthread_mutexattr_destroy, "pthread_mutexattr_destroy");

   function pthread_mutexattr_getpshared (attr : access constant pthread_mutexattr_t; pshared : access int) return int;  -- /usr/include/pthread.h:825
   pragma Import (C, pthread_mutexattr_getpshared, "pthread_mutexattr_getpshared");

   function pthread_mutexattr_setpshared (attr : access pthread_mutexattr_t; pshared : int) return int;  -- /usr/include/pthread.h:831
   pragma Import (C, pthread_mutexattr_setpshared, "pthread_mutexattr_setpshared");

   function pthread_mutexattr_gettype (attr : access constant pthread_mutexattr_t; kind : access int) return int;  -- /usr/include/pthread.h:837
   pragma Import (C, pthread_mutexattr_gettype, "pthread_mutexattr_gettype");

   function pthread_mutexattr_settype (attr : access pthread_mutexattr_t; kind : int) return int;  -- /usr/include/pthread.h:844
   pragma Import (C, pthread_mutexattr_settype, "pthread_mutexattr_settype");

   function pthread_mutexattr_getprotocol (attr : access constant pthread_mutexattr_t; protocol : access int) return int;  -- /usr/include/pthread.h:849
   pragma Import (C, pthread_mutexattr_getprotocol, "pthread_mutexattr_getprotocol");

   function pthread_mutexattr_setprotocol (attr : access pthread_mutexattr_t; protocol : int) return int;  -- /usr/include/pthread.h:856
   pragma Import (C, pthread_mutexattr_setprotocol, "pthread_mutexattr_setprotocol");

   function pthread_mutexattr_getprioceiling (attr : access constant pthread_mutexattr_t; prioceiling : access int) return int;  -- /usr/include/pthread.h:861
   pragma Import (C, pthread_mutexattr_getprioceiling, "pthread_mutexattr_getprioceiling");

   function pthread_mutexattr_setprioceiling (attr : access pthread_mutexattr_t; prioceiling : int) return int;  -- /usr/include/pthread.h:867
   pragma Import (C, pthread_mutexattr_setprioceiling, "pthread_mutexattr_setprioceiling");

   function pthread_mutexattr_getrobust (attr : access constant pthread_mutexattr_t; robustness : access int) return int;  -- /usr/include/pthread.h:873
   pragma Import (C, pthread_mutexattr_getrobust, "pthread_mutexattr_getrobust");

   function pthread_mutexattr_getrobust_np (attr : access constant pthread_mutexattr_t; robustness : access int) return int;  -- /usr/include/pthread.h:877
   pragma Import (C, pthread_mutexattr_getrobust_np, "pthread_mutexattr_getrobust_np");

   function pthread_mutexattr_setrobust (attr : access pthread_mutexattr_t; robustness : int) return int;  -- /usr/include/pthread.h:883
   pragma Import (C, pthread_mutexattr_setrobust, "pthread_mutexattr_setrobust");

   function pthread_mutexattr_setrobust_np (attr : access pthread_mutexattr_t; robustness : int) return int;  -- /usr/include/pthread.h:887
   pragma Import (C, pthread_mutexattr_setrobust_np, "pthread_mutexattr_setrobust_np");

   function pthread_rwlock_init (rwlock : access pthread_rwlock_t; attr : access constant pthread_rwlockattr_t) return int;  -- /usr/include/pthread.h:899
   pragma Import (C, pthread_rwlock_init, "pthread_rwlock_init");

   function pthread_rwlock_destroy (rwlock : access pthread_rwlock_t) return int;  -- /usr/include/pthread.h:904
   pragma Import (C, pthread_rwlock_destroy, "pthread_rwlock_destroy");

   function pthread_rwlock_rdlock (rwlock : access pthread_rwlock_t) return int;  -- /usr/include/pthread.h:908
   pragma Import (C, pthread_rwlock_rdlock, "pthread_rwlock_rdlock");

   function pthread_rwlock_tryrdlock (rwlock : access pthread_rwlock_t) return int;  -- /usr/include/pthread.h:912
   pragma Import (C, pthread_rwlock_tryrdlock, "pthread_rwlock_tryrdlock");

   function pthread_rwlock_timedrdlock (rwlock : access pthread_rwlock_t; abstime : access constant Libc.Time.timespec) return int;  -- /usr/include/pthread.h:917
   pragma Import (C, pthread_rwlock_timedrdlock, "pthread_rwlock_timedrdlock");

   function pthread_rwlock_wrlock (rwlock : access pthread_rwlock_t) return int;  -- /usr/include/pthread.h:923
   pragma Import (C, pthread_rwlock_wrlock, "pthread_rwlock_wrlock");

   function pthread_rwlock_trywrlock (rwlock : access pthread_rwlock_t) return int;  -- /usr/include/pthread.h:927
   pragma Import (C, pthread_rwlock_trywrlock, "pthread_rwlock_trywrlock");

   function pthread_rwlock_timedwrlock (rwlock : access pthread_rwlock_t; abstime : access constant Libc.Time.timespec) return int;  -- /usr/include/pthread.h:932
   pragma Import (C, pthread_rwlock_timedwrlock, "pthread_rwlock_timedwrlock");

   function pthread_rwlock_unlock (rwlock : access pthread_rwlock_t) return int;  -- /usr/include/pthread.h:938
   pragma Import (C, pthread_rwlock_unlock, "pthread_rwlock_unlock");

   function pthread_rwlockattr_init (attr : access pthread_rwlockattr_t) return int;  -- /usr/include/pthread.h:945
   pragma Import (C, pthread_rwlockattr_init, "pthread_rwlockattr_init");

   function pthread_rwlockattr_destroy (attr : access pthread_rwlockattr_t) return int;  -- /usr/include/pthread.h:949
   pragma Import (C, pthread_rwlockattr_destroy, "pthread_rwlockattr_destroy");

   function pthread_rwlockattr_getpshared (attr : access constant pthread_rwlockattr_t; pshared : access int) return int;  -- /usr/include/pthread.h:953
   pragma Import (C, pthread_rwlockattr_getpshared, "pthread_rwlockattr_getpshared");

   function pthread_rwlockattr_setpshared (attr : access pthread_rwlockattr_t; pshared : int) return int;  -- /usr/include/pthread.h:959
   pragma Import (C, pthread_rwlockattr_setpshared, "pthread_rwlockattr_setpshared");

   function pthread_rwlockattr_getkind_np (attr : access constant pthread_rwlockattr_t; pref : access int) return int;  -- /usr/include/pthread.h:964
   pragma Import (C, pthread_rwlockattr_getkind_np, "pthread_rwlockattr_getkind_np");

   function pthread_rwlockattr_setkind_np (attr : access pthread_rwlockattr_t; pref : int) return int;  -- /usr/include/pthread.h:970
   pragma Import (C, pthread_rwlockattr_setkind_np, "pthread_rwlockattr_setkind_np");

   function pthread_cond_init (cond : access pthread_cond_t; cond_attr : access constant pthread_condattr_t) return int;  -- /usr/include/pthread.h:979
   pragma Import (C, pthread_cond_init, "pthread_cond_init");

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;  -- /usr/include/pthread.h:984
   pragma Import (C, pthread_cond_destroy, "pthread_cond_destroy");

   function pthread_cond_signal (cond : access pthread_cond_t) return int;  -- /usr/include/pthread.h:988
   pragma Import (C, pthread_cond_signal, "pthread_cond_signal");

   function pthread_cond_broadcast (cond : access pthread_cond_t) return int;  -- /usr/include/pthread.h:992
   pragma Import (C, pthread_cond_broadcast, "pthread_cond_broadcast");

   function pthread_cond_wait (cond : access pthread_cond_t; mutex : access pthread_mutex_t) return int;  -- /usr/include/pthread.h:1000
   pragma Import (C, pthread_cond_wait, "pthread_cond_wait");

   function pthread_cond_timedwait
     (cond : access pthread_cond_t;
      mutex : access pthread_mutex_t;
      abstime : access constant Libc.Time.timespec) return int;  -- /usr/include/pthread.h:1011
   pragma Import (C, pthread_cond_timedwait, "pthread_cond_timedwait");

   function pthread_condattr_init (attr : access pthread_condattr_t) return int;  -- /usr/include/pthread.h:1019
   pragma Import (C, pthread_condattr_init, "pthread_condattr_init");

   function pthread_condattr_destroy (attr : access pthread_condattr_t) return int;  -- /usr/include/pthread.h:1023
   pragma Import (C, pthread_condattr_destroy, "pthread_condattr_destroy");

   function pthread_condattr_getpshared (attr : access constant pthread_condattr_t; pshared : access int) return int;  -- /usr/include/pthread.h:1027
   pragma Import (C, pthread_condattr_getpshared, "pthread_condattr_getpshared");

   function pthread_condattr_setpshared (attr : access pthread_condattr_t; pshared : int) return int;  -- /usr/include/pthread.h:1033
   pragma Import (C, pthread_condattr_setpshared, "pthread_condattr_setpshared");

   function pthread_condattr_getclock (attr : access constant pthread_condattr_t; clock_id : access Libc.x86_64_linux_gnu_bits_types_h.clockid_t) return int;  -- /usr/include/pthread.h:1038
   pragma Import (C, pthread_condattr_getclock, "pthread_condattr_getclock");

   function pthread_condattr_setclock (attr : access pthread_condattr_t; clock_id : Libc.x86_64_linux_gnu_bits_types_h.clockid_t) return int;  -- /usr/include/pthread.h:1044
   pragma Import (C, pthread_condattr_setclock, "pthread_condattr_setclock");

   function pthread_spin_init (lock : access pthread_spinlock_t; pshared : int) return int;  -- /usr/include/pthread.h:1055
   pragma Import (C, pthread_spin_init, "pthread_spin_init");

   function pthread_spin_destroy (lock : access pthread_spinlock_t) return int;  -- /usr/include/pthread.h:1059
   pragma Import (C, pthread_spin_destroy, "pthread_spin_destroy");

   function pthread_spin_lock (lock : access pthread_spinlock_t) return int;  -- /usr/include/pthread.h:1063
   pragma Import (C, pthread_spin_lock, "pthread_spin_lock");

   function pthread_spin_trylock (lock : access pthread_spinlock_t) return int;  -- /usr/include/pthread.h:1067
   pragma Import (C, pthread_spin_trylock, "pthread_spin_trylock");

   function pthread_spin_unlock (lock : access pthread_spinlock_t) return int;  -- /usr/include/pthread.h:1071
   pragma Import (C, pthread_spin_unlock, "pthread_spin_unlock");

   function pthread_barrier_init
     (barrier : access pthread_barrier_t;
      attr : access constant pthread_barrierattr_t;
      count : unsigned) return int;  -- /usr/include/pthread.h:1079
   pragma Import (C, pthread_barrier_init, "pthread_barrier_init");

   function pthread_barrier_destroy (barrier : access pthread_barrier_t) return int;  -- /usr/include/pthread.h:1085
   pragma Import (C, pthread_barrier_destroy, "pthread_barrier_destroy");

   function pthread_barrier_wait (barrier : access pthread_barrier_t) return int;  -- /usr/include/pthread.h:1089
   pragma Import (C, pthread_barrier_wait, "pthread_barrier_wait");

   function pthread_barrierattr_init (attr : access pthread_barrierattr_t) return int;  -- /usr/include/pthread.h:1094
   pragma Import (C, pthread_barrierattr_init, "pthread_barrierattr_init");

   function pthread_barrierattr_destroy (attr : access pthread_barrierattr_t) return int;  -- /usr/include/pthread.h:1098
   pragma Import (C, pthread_barrierattr_destroy, "pthread_barrierattr_destroy");

   function pthread_barrierattr_getpshared (attr : access constant pthread_barrierattr_t; pshared : access int) return int;  -- /usr/include/pthread.h:1102
   pragma Import (C, pthread_barrierattr_getpshared, "pthread_barrierattr_getpshared");

   function pthread_barrierattr_setpshared (attr : access pthread_barrierattr_t; pshared : int) return int;  -- /usr/include/pthread.h:1108
   pragma Import (C, pthread_barrierattr_setpshared, "pthread_barrierattr_setpshared");

   function pthread_key_create (key : access pthread_key_t; destr_function : access procedure (arg1 : System.Address)) return int;  -- /usr/include/pthread.h:1122
   pragma Import (C, pthread_key_create, "pthread_key_create");

   function pthread_key_delete (key : pthread_key_t) return int;  -- /usr/include/pthread.h:1127
   pragma Import (C, pthread_key_delete, "pthread_key_delete");

   function pthread_getspecific (key : pthread_key_t) return System.Address;  -- /usr/include/pthread.h:1130
   pragma Import (C, pthread_getspecific, "pthread_getspecific");

   function pthread_setspecific (key : pthread_key_t; pointer : System.Address) return int;  -- /usr/include/pthread.h:1133
   pragma Import (C, pthread_setspecific, "pthread_setspecific");

   function pthread_getcpuclockid (thread_id : pthread_t; clock_id : access Libc.x86_64_linux_gnu_bits_types_h.clockid_t) return int;  -- /usr/include/pthread.h:1139
   pragma Import (C, pthread_getcpuclockid, "pthread_getcpuclockid");

   function pthread_atfork
     (prepare : access procedure;
      parent : access procedure;
      child : access procedure) return int;  -- /usr/include/pthread.h:1156
   pragma Import (C, pthread_atfork, "pthread_atfork");

end Libc.Pthread;
