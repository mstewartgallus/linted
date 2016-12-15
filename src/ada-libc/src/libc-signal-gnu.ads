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
with Interfaces.C.Strings;
with System;
with Libc.Sys.Types;
with Libc.Time;
with Libc.Time.GNU;

package Libc.Signal.GNU with SPARK_Mode => Off is
   pragma Preelaborate;

   --  unsupported macro: si_pid _sifields._kill.si_pid
   --  unsupported macro: si_uid _sifields._kill.si_uid
   --  unsupported macro: si_timerid _sifields._timer.si_tid
   --  unsupported macro: si_overrun _sifields._timer.si_overrun
   --  unsupported macro: si_status _sifields._sigchld.si_status
   --  unsupported macro: si_utime _sifields._sigchld.si_utime
   --  unsupported macro: si_stime _sifields._sigchld.si_stime
   --  unsupported macro: si_value _sifields._rt.si_sigval
   --  unsupported macro: si_int _sifields._rt.si_sigval.sival_int
   --  unsupported macro: si_ptr _sifields._rt.si_sigval.sival_ptr
   --  unsupported macro: si_addr _sifields._sigfault.si_addr
   --  unsupported macro: si_addr_lsb _sifields._sigfault.si_addr_lsb
   --  unsupported macro: si_band _sifields._sigpoll.si_band
   --  unsupported macro: si_fd _sifields._sigpoll.si_fd
   --  unsupported macro: si_call_addr _sifields._sigsys._call_addr
   --  unsupported macro: si_syscall _sifields._sigsys._syscall
   --  unsupported macro: si_arch _sifields._sigsys._arch
   --  unsupported macro: SI_ASYNCNL SI_ASYNCNL
   --  unsupported macro: SI_TKILL SI_TKILL
   --  unsupported macro: SI_SIGIO SI_SIGIO
   --  unsupported macro: SI_ASYNCIO SI_ASYNCIO
   --  unsupported macro: SI_MESGQ SI_MESGQ
   --  unsupported macro: SI_TIMER SI_TIMER
   --  unsupported macro: SI_QUEUE SI_QUEUE
   --  unsupported macro: SI_USER SI_USER
   --  unsupported macro: SI_KERNEL SI_KERNEL
   --  unsupported macro: ILL_ILLOPC ILL_ILLOPC
   --  unsupported macro: ILL_ILLOPN ILL_ILLOPN
   --  unsupported macro: ILL_ILLADR ILL_ILLADR
   --  unsupported macro: ILL_ILLTRP ILL_ILLTRP
   --  unsupported macro: ILL_PRVOPC ILL_PRVOPC
   --  unsupported macro: ILL_PRVREG ILL_PRVREG
   --  unsupported macro: ILL_COPROC ILL_COPROC
   --  unsupported macro: ILL_BADSTK ILL_BADSTK
   --  unsupported macro: FPE_INTDIV FPE_INTDIV
   --  unsupported macro: FPE_INTOVF FPE_INTOVF
   --  unsupported macro: FPE_FLTDIV FPE_FLTDIV
   --  unsupported macro: FPE_FLTOVF FPE_FLTOVF
   --  unsupported macro: FPE_FLTUND FPE_FLTUND
   --  unsupported macro: FPE_FLTRES FPE_FLTRES
   --  unsupported macro: FPE_FLTINV FPE_FLTINV
   --  unsupported macro: FPE_FLTSUB FPE_FLTSUB
   --  unsupported macro: SEGV_MAPERR SEGV_MAPERR
   --  unsupported macro: SEGV_ACCERR SEGV_ACCERR
   --  unsupported macro: BUS_ADRALN BUS_ADRALN
   --  unsupported macro: BUS_ADRERR BUS_ADRERR
   --  unsupported macro: BUS_OBJERR BUS_OBJERR
   --  unsupported macro: BUS_MCEERR_AR BUS_MCEERR_AR
   --  unsupported macro: BUS_MCEERR_AO BUS_MCEERR_AO
   --  unsupported macro: TRAP_BRKPT TRAP_BRKPT
   --  unsupported macro: TRAP_TRACE TRAP_TRACE
   --  unsupported macro: CLD_EXITED CLD_EXITED
   --  unsupported macro: CLD_KILLED CLD_KILLED
   --  unsupported macro: CLD_DUMPED CLD_DUMPED
   --  unsupported macro: CLD_TRAPPED CLD_TRAPPED
   --  unsupported macro: CLD_STOPPED CLD_STOPPED
   --  unsupported macro: CLD_CONTINUED CLD_CONTINUED
   --  unsupported macro: POLL_IN POLL_IN
   --  unsupported macro: POLL_OUT POLL_OUT
   --  unsupported macro: POLL_MSG POLL_MSG
   --  unsupported macro: POLL_ERR POLL_ERR
   --  unsupported macro: POLL_PRI POLL_PRI
   --  unsupported macro: POLL_HUP POLL_HUP
   --  unsupported macro: sigev_notify_function _sigev_un._sigev_thread._function
   --  unsupported macro: sigev_notify_attributes _sigev_un._sigev_thread._attribute
   --  unsupported macro: SIGEV_SIGNAL SIGEV_SIGNAL
   --  unsupported macro: SIGEV_NONE SIGEV_NONE
   --  unsupported macro: SIGEV_THREAD SIGEV_THREAD
   --  unsupported macro: SIGEV_THREAD_ID SIGEV_THREAD_ID
   type sigval (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            sival_int : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:34
         when others =>
            sival_ptr : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:35
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, sigval);
   pragma Unchecked_Union (sigval);  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:32

   subtype sigval_t is sigval;

   type uu_sigset_t_uu_val_array is array (0 .. 15) of aliased unsigned_long;
   type sigset_t is record
      uu_val : aliased uu_sigset_t_uu_val_array;  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:29
   end record;
   pragma Convention (C_Pass_By_Copy, sigset_t);  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:30

   type siginfo_t_u_pad_array is array (0 .. 27) of aliased int;
   type anon_14;
   type anon_15 is record
      si_pid : aliased Libc.Sys.Types.pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:76
      si_uid : aliased Libc.Sys.Types.uid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:77
   end record;
   pragma Convention (C_Pass_By_Copy, anon_15);
   type anon_16 is record
      si_tid : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:83
      si_overrun : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:84
      si_sigval : sigval_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:85
   end record;
   pragma Convention (C_Pass_By_Copy, anon_16);
   type anon_17 is record
      si_pid : aliased Libc.Sys.Types.pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:91
      si_uid : aliased Libc.Sys.Types.uid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:92
      si_sigval : sigval_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:93
   end record;
   pragma Convention (C_Pass_By_Copy, anon_17);
   type anon_18 is record
      si_pid : aliased Libc.Sys.Types.pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:99
      si_uid : aliased Libc.Sys.Types.uid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:100
      si_status : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:101
      si_utime : aliased Libc.Time.clock_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:102
      si_stime : aliased Libc.Time.clock_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:103
   end record;
   pragma Convention (C_Pass_By_Copy, anon_18);
   type anon_19 is record
      si_addr : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:109
      si_addr_lsb : aliased short;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:110
   end record;
   pragma Convention (C_Pass_By_Copy, anon_19);
   type anon_20 is record
      si_band : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:116
      si_fd : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:117
   end record;
   pragma Convention (C_Pass_By_Copy, anon_20);
   type anon_21 is record
      u_call_addr : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:123
      u_syscall : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:124
      u_arch : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:125
   end record;
   pragma Convention (C_Pass_By_Copy, anon_21);
   type anon_14 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            u_pad : aliased siginfo_t_u_pad_array;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:71
         when 1 =>
            u_kill : aliased anon_15;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:78
         when 2 =>
            u_timer : aliased anon_16;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:86
         when 3 =>
            u_rt : aliased anon_17;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:94
         when 4 =>
            u_sigchld : aliased anon_18;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:104
         when 5 =>
            u_sigfault : aliased anon_19;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:111
         when 6 =>
            u_sigpoll : aliased anon_20;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:118
         when others =>
            u_sigsys : aliased anon_21;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:126
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_14);
   pragma Unchecked_Union (anon_14);
   type siginfo_t is record
      si_signo : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:64
      si_errno : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:65
      si_code : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:67
      u_sifields : anon_14;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:127
   end record;
   pragma Convention (C_Pass_By_Copy, siginfo_t);  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:128

   --  skipped anonymous struct anon_13

   --  type sigevent_u_pad_array is array (0 .. 11) of aliased int;
   --  type anon_30;
   --  type anon_31 is record
   --     u_function : access procedure (arg1 : sigval_t);  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:323
   --     u_attribute : access Libc.Pthread.pthread_attr_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:324
   --  end record;
   --  pragma Convention (C_Pass_By_Copy, anon_31);
   --  type anon_30 (discr : unsigned := 0) is record
   --     case discr is
   --        when 0 =>
   --           u_pad : aliased sigevent_u_pad_array;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:315
   --        when 1 =>
   --           u_tid : aliased Libc.Sys.Types.pid_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:319
   --        when others =>
   --           u_sigev_thread : aliased anon_31;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:325
   --     end case;
   --  end record;
   --  pragma Convention (C_Pass_By_Copy, anon_30);
   --  pragma Unchecked_Union (anon_30);
   --  type sigevent is record
   --     sigev_value : sigval_t;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:309
   --     sigev_signo : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:310
   --     sigev_notify : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:311
   --     u_sigev_un : anon_30;  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:326
   --  end record;
   --  pragma Convention (C_Pass_By_Copy, sigevent);  -- /usr/include/x86_64-linux-gnu/bits/siginfo.h:307

   --  subtype sigevent_t is sigevent;

   --  unsupported macro: sa_handler __sigaction_handler.sa_handler
   --  unsupported macro: sa_sigaction __sigaction_handler.sa_sigaction
   --  unsupported macro: SA_NOCLDSTOP 1
   --  unsupported macro: SA_NOCLDWAIT 2
   --  unsupported macro: SA_SIGINFO 4
   --  unsupported macro: SA_ONSTACK 0x08000000
   --  unsupported macro: SA_RESTART 0x10000000
   --  unsupported macro: SA_NODEFER 0x40000000
   --  unsupported macro: SA_RESETHAND 0x80000000
   --  unsupported macro: SA_INTERRUPT 0x20000000
   --  unsupported macro: SA_NOMASK SA_NODEFER
   --  unsupported macro: SA_ONESHOT SA_RESETHAND
   --  unsupported macro: SA_STACK SA_ONSTACK
   --  unsupported macro: SIG_BLOCK 0
   --  unsupported macro: SIG_UNBLOCK 1
   --  unsupported macro: SIG_SETMASK 2
   type anon_13 (discr : unsigned := 0) is record
      case discr is
         when others =>
            sa_sigaction : access procedure
                 (arg1 : int;
                  arg2 : access int;
                  arg3 : System.Address);  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:33
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_13);
   pragma Unchecked_Union (anon_13);
   type sigaction is record
      uu_sigaction_handler : anon_13;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:35
      sa_mask : aliased sigset_t;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:43
      sa_flags : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:46
      sa_restorer : access procedure;  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:49
   end record;
   pragma Convention (C_Pass_By_Copy, sigaction);  -- /usr/include/x86_64-linux-gnu/bits/sigaction.h:24

   --  unsupported macro: FP_XSTATE_MAGIC1 0x46505853U
   --  unsupported macro: FP_XSTATE_MAGIC2 0x46505845U
   --  unsupported macro: FP_XSTATE_MAGIC2_SIZE sizeof(FP_XSTATE_MAGIC2)
   type u_fpx_sw_bytes is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, u_fpx_sw_bytes);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:29

   type u_fpreg_significand_array is array (0 .. 3) of aliased unsigned_short;
   type u_fpreg is record
      significand : aliased u_fpreg_significand_array;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:40
      exponent : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:41
   end record;
   pragma Convention (C_Pass_By_Copy, u_fpreg);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:38

   type u_fpxreg_significand_array is array (0 .. 3) of aliased unsigned_short;
   type u_fpxreg_padding_array is array (0 .. 2) of aliased unsigned_short;
   type u_fpxreg is record
      significand : aliased u_fpxreg_significand_array;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:46
      exponent : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:47
      padding : aliased u_fpxreg_padding_array;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:48
   end record;
   pragma Convention (C_Pass_By_Copy, u_fpxreg);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:44

   type u_xmmreg is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, u_xmmreg);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:51

   type u_fpstate_u_st_array is array (0 .. 7) of aliased u_fpxreg;
   type u_fpstate_u_xmm_array is array (0 .. 15) of aliased u_xmmreg;
   type u_fpstate is record
      u_st : aliased u_fpstate_u_st_array;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:132
      u_xmm : aliased u_fpstate_u_xmm_array;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:133
   end record;
   pragma Convention (C_Pass_By_Copy, u_fpstate);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:121

   type anon_0 (discr : unsigned := 0) is record
      case discr is
         when others =>
            fpstate : access u_fpstate;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:167
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_0);
   pragma Unchecked_Union (anon_0);
   type sigcontext is record
      cs : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:157
      gs : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:158
      fs : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:159
      uu_pad0 : aliased unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:160
      field_5 : aliased anon_0;
   end record;
   pragma Convention (C_Pass_By_Copy, sigcontext);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:137

   type u_xsave_hdr is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, u_xsave_hdr);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:175

   type u_ymmh_state is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, u_ymmh_state);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:182

   type u_xstate is record
      fpstate : aliased u_fpstate;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:189
      xstate_hdr : aliased u_xsave_hdr;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:190
      ymmh : aliased u_ymmh_state;  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:191
   end record;
   pragma Convention (C_Pass_By_Copy, u_xstate);  -- /usr/include/x86_64-linux-gnu/bits/sigcontext.h:187

   --  arg-macro: procedure sigmask (sig)
   --    __sigmask(sig)
   --  unsupported macro: NSIG _NSIG
   --  unsupported macro: sv_onstack sv_flags
   --  unsupported macro: SV_ONSTACK (1 << 0)
   --  unsupported macro: SV_INTERRUPT (1 << 1)
   --  unsupported macro: SV_RESETHAND (1 << 2)

   --  unsupported macro: SS_ONSTACK SS_ONSTACK
   --  unsupported macro: SS_DISABLE SS_DISABLE
   --  unsupported macro: MINSIGSTKSZ 2048
   --  unsupported macro: SIGSTKSZ 8192
   type sigstack is record
      ss_sp : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:27
      ss_onstack : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:28
   end record;
   pragma Convention (C_Pass_By_Copy, sigstack);  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:25

   type sigaltstack is record
      ss_sp : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:51
      ss_flags : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:52
   end record;
   pragma Convention (C_Pass_By_Copy, sigaltstack);  -- /usr/include/x86_64-linux-gnu/bits/sigstack.h:49

   subtype stack_t is sigaltstack;

   function sysv_signal (sig : int; handler : sighandler_t) return sighandler_t;  -- /usr/include/signal.h:93
   pragma Import (C, sysv_signal, "sysv_signal");

   function bsd_signal (sig : int; handler : sighandler_t) return sighandler_t;  -- /usr/include/signal.h:119
   pragma Import (C, bsd_signal, "bsd_signal");

   function kill (pid : Libc.Sys.Types.pid_t; sig : int) return int;  -- /usr/include/signal.h:127
   pragma Import (C, kill, "kill");

   function killpg (pgrp : Libc.Sys.Types.pid_t; sig : int) return int;  -- /usr/include/signal.h:134
   pragma Import (C, killpg, "killpg");

   function ssignal (sig : int; handler : sighandler_t) return sighandler_t;  -- /usr/include/signal.h:144
   pragma Import (C, ssignal, "ssignal");

   function gsignal (sig : int) return int;  -- /usr/include/signal.h:146
   pragma Import (C, gsignal, "gsignal");

   procedure psignal (sig : int; s : Interfaces.C.Strings.chars_ptr);  -- /usr/include/signal.h:151
   pragma Import (C, psignal, "psignal");

   procedure psiginfo (pinfo : access constant siginfo_t; s : Interfaces.C.Strings.chars_ptr);  -- /usr/include/signal.h:156
   pragma Import (C, psiginfo, "psiginfo");

   function sigpause (sig : int) return int;  -- /usr/include/signal.h:171
   pragma Import (C, sigpause, "__xpg_sigpause");

   function sigblock (mask : int) return int;  -- /usr/include/signal.h:189
   pragma Import (C, sigblock, "sigblock");

   function sigsetmask (mask : int) return int;  -- /usr/include/signal.h:192
   pragma Import (C, sigsetmask, "sigsetmask");

   function siggetmask return int;  -- /usr/include/signal.h:195
   pragma Import (C, siggetmask, "siggetmask");

   subtype sig_t is sighandler_t;  -- /usr/include/signal.h:209

   function sigemptyset (set : access sigset_t) return int;  -- /usr/include/signal.h:215
   pragma Import (C, sigemptyset, "sigemptyset");

   function sigfillset (set : access sigset_t) return int;  -- /usr/include/signal.h:218
   pragma Import (C, sigfillset, "sigfillset");

   function sigaddset (set : access sigset_t; signo : int) return int;  -- /usr/include/signal.h:221
   pragma Import (C, sigaddset, "sigaddset");

   function sigdelset (set : access sigset_t; signo : int) return int;  -- /usr/include/signal.h:224
   pragma Import (C, sigdelset, "sigdelset");

   function sigismember (set : System.Address; signo : int) return int;  -- /usr/include/signal.h:227
   pragma Import (C, sigismember, "sigismember");

   function sigisemptyset (set : System.Address) return int;  -- /usr/include/signal.h:232
   pragma Import (C, sigisemptyset, "sigisemptyset");

   function sigandset
     (set : access sigset_t;
      left : System.Address;
      right : System.Address) return int;  -- /usr/include/signal.h:235
   pragma Import (C, sigandset, "sigandset");

   function sigorset
     (set : access sigset_t;
      left : System.Address;
      right : System.Address) return int;  -- /usr/include/signal.h:239
   pragma Import (C, sigorset, "sigorset");

   function sigprocmask
     (how : int;
      set : System.Address;
      oset : access sigset_t) return int;  -- /usr/include/signal.h:248
   pragma Import (C, sigprocmask, "sigprocmask");

   function sigsuspend (set : System.Address) return int;  -- /usr/include/signal.h:256
   pragma Import (C, sigsuspend, "sigsuspend");

   function f_sigaction
     (sig : int;
      act : access constant sigaction;
      oact : access sigaction) return int;  -- /usr/include/signal.h:259
   pragma Import (C, f_sigaction, "sigaction");

   function sigpending (set : access sigset_t) return int;  -- /usr/include/signal.h:263
   pragma Import (C, sigpending, "sigpending");

   function sigwait (set : System.Address; sig : access int) return int;  -- /usr/include/signal.h:270
   pragma Import (C, sigwait, "sigwait");

   function sigwaitinfo (set : System.Address; info : access siginfo_t) return int;  -- /usr/include/signal.h:278
   pragma Import (C, sigwaitinfo, "sigwaitinfo");

   function sigtimedwait
     (set : System.Address;
      info : access siginfo_t;
      timeout : access constant Libc.Time.GNU.timespec) return int;  -- /usr/include/signal.h:286
   pragma Import (C, sigtimedwait, "sigtimedwait");

   function sigqueue
     (pid : Libc.Sys.Types.pid_t;
      sig : int;
      val : sigval) return int;  -- /usr/include/signal.h:293
   pragma Import (C, sigqueue, "sigqueue");

   sys_siglist : aliased array (0 .. 64) of Interfaces.C.Strings.chars_ptr;  -- /usr/include/signal.h:304
   pragma Import (C, sys_siglist, "sys_siglist");

   type sigvec is record
      sv_handler : sighandler_t;  -- /usr/include/signal.h:309
      sv_mask : aliased int;  -- /usr/include/signal.h:310
      sv_flags : aliased int;  -- /usr/include/signal.h:312
   end record;
   pragma Convention (C_Pass_By_Copy, sigvec);  -- /usr/include/signal.h:307

   function f_sigvec
     (sig : int;
      vec : access constant sigvec;
      ovec : access sigvec) return int;  -- /usr/include/signal.h:327
   pragma Import (C, f_sigvec, "f_sigvec");

   function sigreturn (scp : access sigcontext) return int;  -- /usr/include/signal.h:335
   pragma Import (C, sigreturn, "sigreturn");

   function siginterrupt (sig : int; interrupt : int) return int;  -- /usr/include/signal.h:347
   pragma Import (C, siginterrupt, "siginterrupt");

   function f_sigstack (ss : access sigstack; oss : access sigstack) return int;  -- /usr/include/signal.h:358
   pragma Import (C, f_sigstack, "sigstack");

   function f_sigaltstack (ss : access constant sigaltstack; oss : access sigaltstack) return int;  -- /usr/include/signal.h:363
   pragma Import (C, f_sigaltstack, "sigaltstack");

   function sighold (sig : int) return int;  -- /usr/include/signal.h:372
   pragma Import (C, sighold, "sighold");

   function sigrelse (sig : int) return int;  -- /usr/include/signal.h:375
   pragma Import (C, sigrelse, "sigrelse");

   function sigignore (sig : int) return int;  -- /usr/include/signal.h:378
   pragma Import (C, sigignore, "sigignore");

   function sigset (sig : int; disp : sighandler_t) return sighandler_t;  -- /usr/include/signal.h:381
   pragma Import (C, sigset, "sigset");
end Libc.Signal.GNU;
