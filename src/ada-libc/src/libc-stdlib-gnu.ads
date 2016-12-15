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
with Interfaces.C.Extensions;
with Libc.Stddef;

package Libc.Stdlib.GNU with SPARK_Mode => Off is
   --  arg-macro: procedure WEXITSTATUS (status)
   --    __WEXITSTATUS (__WAIT_INT (status))
   --  arg-macro: procedure WTERMSIG (status)
   --    __WTERMSIG (__WAIT_INT (status))
   --  arg-macro: procedure WSTOPSIG (status)
   --    __WSTOPSIG (__WAIT_INT (status))
   --  arg-macro: procedure WIFEXITED (status)
   --    __WIFEXITED (__WAIT_INT (status))
   --  arg-macro: procedure WIFSIGNALED (status)
   --    __WIFSIGNALED (__WAIT_INT (status))
   --  arg-macro: procedure WIFSTOPPED (status)
   --    __WIFSTOPPED (__WAIT_INT (status))
   --  arg-macro: procedure WIFCONTINUED (status)
   --    __WIFCONTINUED (__WAIT_INT (status))

   --  function strtoq
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     base : int) return Long_Long_Integer;  -- /usr/include/stdlib.h:195
   --  pragma Import (C, strtoq, "strtoq");

   function strtouq
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address;
      base : int) return Extensions.unsigned_long_long;  -- /usr/include/stdlib.h:200
   pragma Import (C, strtouq, "strtouq");



   --  function strtol_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     base : int;
   --     loc : Libc.xlocale_h.locale_t) return long;  -- /usr/include/stdlib.h:239
   --  pragma Import (C, strtol_l, "strtol_l");

   --  function strtoul_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     base : int;
   --     loc : Libc.xlocale_h.locale_t) return unsigned_long;  -- /usr/include/stdlib.h:243
   --  pragma Import (C, strtoul_l, "strtoul_l");

   --  function strtoll_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     base : int;
   --     loc : Libc.xlocale_h.locale_t) return Long_Long_Integer;  -- /usr/include/stdlib.h:249
   --  pragma Import (C, strtoll_l, "strtoll_l");

   --  function strtoull_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     base : int;
   --     loc : Libc.xlocale_h.locale_t) return Extensions.unsigned_long_long;  -- /usr/include/stdlib.h:255
   --  pragma Import (C, strtoull_l, "strtoull_l");

   --  function strtod_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     loc : Libc.xlocale_h.locale_t) return double;  -- /usr/include/stdlib.h:260
   --  pragma Import (C, strtod_l, "strtod_l");

   --  function strtof_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     loc : Libc.xlocale_h.locale_t) return float;  -- /usr/include/stdlib.h:264
   --  pragma Import (C, strtof_l, "strtof_l");

   --  function strtold_l
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     loc : Libc.xlocale_h.locale_t) return long_double;  -- /usr/include/stdlib.h:268
   --  pragma Import (C, strtold_l, "strtold_l");

   function l64a (n : long) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:305
   pragma Import (C, l64a, "l64a");

   function a64l (s : Interfaces.C.Strings.chars_ptr) return long;  -- /usr/include/stdlib.h:308
   pragma Import (C, a64l, "a64l");

   function random return long;  -- /usr/include/stdlib.h:321
   pragma Import (C, random, "random");

   procedure srandom (seed : unsigned);  -- /usr/include/stdlib.h:324
   pragma Import (C, srandom, "srandom");

   function initstate
     (seed : unsigned;
      statebuf : Interfaces.C.Strings.chars_ptr;
      statelen : size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:330
   pragma Import (C, initstate, "initstate");

   function setstate (statebuf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:335
   pragma Import (C, setstate, "setstate");

   --  type random_data is record
   --     fptr : access Libc.Stdint.int32_t;  -- /usr/include/stdlib.h:345
   --     rptr : access Libc.Stdint.int32_t;  -- /usr/include/stdlib.h:346
   --     state : access Libc.Stdint.int32_t;  -- /usr/include/stdlib.h:347
   --     rand_type : aliased int;  -- /usr/include/stdlib.h:348
   --     rand_deg : aliased int;  -- /usr/include/stdlib.h:349
   --     rand_sep : aliased int;  -- /usr/include/stdlib.h:350
   --     end_ptr : access Libc.Stdint.int32_t;  -- /usr/include/stdlib.h:351
   --  end record;
   --  pragma Convention (C_Pass_By_Copy, random_data);  -- /usr/include/stdlib.h:343

   --  function random_r (buf : access random_data; result : access Libc.Stdint.int32_t) return int;  -- /usr/include/stdlib.h:354
   --  pragma Import (C, random_r, "random_r");

   --  function srandom_r (seed : unsigned; buf : access random_data) return int;  -- /usr/include/stdlib.h:357
   --  pragma Import (C, srandom_r, "srandom_r");

   --  function initstate_r
   --    (seed : unsigned;
   --     statebuf : Interfaces.C.Strings.chars_ptr;
   --     statelen : size_t;
   --     buf : access random_data) return int;  -- /usr/include/stdlib.h:360
   --  pragma Import (C, initstate_r, "initstate_r");

   --  function setstate_r (statebuf : Interfaces.C.Strings.chars_ptr; buf : access random_data) return int;  -- /usr/include/stdlib.h:365
   --  pragma Import (C, setstate_r, "setstate_r");


   function rand_r (seed : access unsigned) return int;  -- /usr/include/stdlib.h:381
   pragma Import (C, rand_r, "rand_r");

   function drand48 return double;  -- /usr/include/stdlib.h:389
   pragma Import (C, drand48, "drand48");

   function erand48 (xsubi : access unsigned_short) return double;  -- /usr/include/stdlib.h:390
   pragma Import (C, erand48, "erand48");

   function lrand48 return long;  -- /usr/include/stdlib.h:393
   pragma Import (C, lrand48, "lrand48");

   function nrand48 (xsubi : access unsigned_short) return long;  -- /usr/include/stdlib.h:394
   pragma Import (C, nrand48, "nrand48");

   function mrand48 return long;  -- /usr/include/stdlib.h:398
   pragma Import (C, mrand48, "mrand48");

   function jrand48 (xsubi : access unsigned_short) return long;  -- /usr/include/stdlib.h:399
   pragma Import (C, jrand48, "jrand48");

   procedure srand48 (seedval : long);  -- /usr/include/stdlib.h:403
   pragma Import (C, srand48, "srand48");

   function seed48 (seed16v : access unsigned_short) return access unsigned_short;  -- /usr/include/stdlib.h:404
   pragma Import (C, seed48, "seed48");

   procedure lcong48 (param : access unsigned_short);  -- /usr/include/stdlib.h:406
   pragma Import (C, lcong48, "lcong48");

   type drand48_data_x_array is array (0 .. 2) of aliased unsigned_short;
   type drand48_data_old_x_array is array (0 .. 2) of aliased unsigned_short;
   type drand48_data is record
      x : aliased drand48_data_x_array;  -- /usr/include/stdlib.h:414
      old_x : aliased drand48_data_old_x_array;  -- /usr/include/stdlib.h:415
      c : aliased unsigned_short;  -- /usr/include/stdlib.h:416
      init : aliased unsigned_short;  -- /usr/include/stdlib.h:417
      a : aliased Extensions.unsigned_long_long;  -- /usr/include/stdlib.h:418
   end record;
   pragma Convention (C_Pass_By_Copy, drand48_data);  -- /usr/include/stdlib.h:412

   function drand48_r (buffer : access drand48_data; result : access double) return int;  -- /usr/include/stdlib.h:423
   pragma Import (C, drand48_r, "drand48_r");

   function erand48_r
     (xsubi : access unsigned_short;
      buffer : access drand48_data;
      result : access double) return int;  -- /usr/include/stdlib.h:425
   pragma Import (C, erand48_r, "erand48_r");

   function lrand48_r (buffer : access drand48_data; result : access long) return int;  -- /usr/include/stdlib.h:430
   pragma Import (C, lrand48_r, "lrand48_r");

   function nrand48_r
     (xsubi : access unsigned_short;
      buffer : access drand48_data;
      result : access long) return int;  -- /usr/include/stdlib.h:433
   pragma Import (C, nrand48_r, "nrand48_r");

   function mrand48_r (buffer : access drand48_data; result : access long) return int;  -- /usr/include/stdlib.h:439
   pragma Import (C, mrand48_r, "mrand48_r");

   function jrand48_r
     (xsubi : access unsigned_short;
      buffer : access drand48_data;
      result : access long) return int;  -- /usr/include/stdlib.h:442
   pragma Import (C, jrand48_r, "jrand48_r");

   function srand48_r (seedval : long; buffer : access drand48_data) return int;  -- /usr/include/stdlib.h:448
   pragma Import (C, srand48_r, "srand48_r");

   function seed48_r (seed16v : access unsigned_short; buffer : access drand48_data) return int;  -- /usr/include/stdlib.h:451
   pragma Import (C, seed48_r, "seed48_r");

   function lcong48_r (param : access unsigned_short; buffer : access drand48_data) return int;  -- /usr/include/stdlib.h:454
   pragma Import (C, lcong48_r, "lcong48_r");


   procedure cfree (ptr : System.Address);  -- /usr/include/stdlib.h:488
   pragma Import (C, cfree, "cfree");

   function valloc (size : size_t) return System.Address;  -- /usr/include/stdlib.h:498
   pragma Import (C, valloc, "valloc");

   function posix_memalign
     (memptr : System.Address;
      alignment : size_t;
      size : size_t) return int;  -- /usr/include/stdlib.h:503
   pragma Import (C, posix_memalign, "posix_memalign");

   function aligned_alloc (alignment : size_t; size : size_t) return System.Address;  -- /usr/include/stdlib.h:509
   pragma Import (C, aligned_alloc, "aligned_alloc");


   function at_quick_exit (func : access procedure) return int;  -- /usr/include/stdlib.h:524
   pragma Import (C, at_quick_exit, "at_quick_exit");

   function on_exit (func : access procedure (arg1 : int; arg2 : System.Address); arg : System.Address) return int;  -- /usr/include/stdlib.h:535
   pragma Import (C, on_exit, "on_exit");

   procedure quick_exit (status : int);  -- /usr/include/stdlib.h:549
   pragma Import (C, quick_exit, "quick_exit");


   function secure_getenv (name : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:570
   pragma Import (C, secure_getenv, "secure_getenv");

   function putenv (string : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdlib.h:578
   pragma Import (C, putenv, "putenv");

   function setenv
     (name : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr;
      replace : int) return int;  -- /usr/include/stdlib.h:584
   pragma Import (C, setenv, "setenv");

   function unsetenv (name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdlib.h:588
   pragma Import (C, unsetenv, "unsetenv");

   function clearenv return int;  -- /usr/include/stdlib.h:595
   pragma Import (C, clearenv, "clearenv");

   function mktemp (template : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:606
   pragma Import (C, mktemp, "mktemp");

   function mkstemp (template : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdlib.h:620
   pragma Import (C, mkstemp, "mkstemp");

   function mkstemp64 (template : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdlib.h:630
   pragma Import (C, mkstemp64, "mkstemp64");

   function mkstemps (template : Interfaces.C.Strings.chars_ptr; suffixlen : int) return int;  -- /usr/include/stdlib.h:642
   pragma Import (C, mkstemps, "mkstemps");

   function mkstemps64 (template : Interfaces.C.Strings.chars_ptr; suffixlen : int) return int;  -- /usr/include/stdlib.h:652
   pragma Import (C, mkstemps64, "mkstemps64");

   function mkdtemp (template : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:663
   pragma Import (C, mkdtemp, "mkdtemp");

   function mkostemp (template : Interfaces.C.Strings.chars_ptr; flags : int) return int;  -- /usr/include/stdlib.h:674
   pragma Import (C, mkostemp, "mkostemp");

   function mkostemp64 (template : Interfaces.C.Strings.chars_ptr; flags : int) return int;  -- /usr/include/stdlib.h:684
   pragma Import (C, mkostemp64, "mkostemp64");

   function mkostemps
     (template : Interfaces.C.Strings.chars_ptr;
      suffixlen : int;
      flags : int) return int;  -- /usr/include/stdlib.h:694
   pragma Import (C, mkostemps, "mkostemps");

   function mkostemps64
     (template : Interfaces.C.Strings.chars_ptr;
      suffixlen : int;
      flags : int) return int;  -- /usr/include/stdlib.h:706
   pragma Import (C, mkostemps64, "mkostemps64");

   function canonicalize_file_name (name : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:724
   pragma Import (C, canonicalize_file_name, "canonicalize_file_name");

   function realpath (name : Interfaces.C.Strings.chars_ptr; resolved : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:734
   pragma Import (C, realpath, "realpath");

   subtype comparison_fn_t is compar_fn_t;  -- /usr/include/stdlib.h:745

   type compar_d_fn_t is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : System.Address) return int;
   pragma Convention (C, compar_d_fn_t);  -- /usr/include/stdlib.h:749

   procedure qsort_r
     (base : System.Address;
      nmemb : size_t;
      size : size_t;
      compar : compar_d_fn_t;
      arg : System.Address);  -- /usr/include/stdlib.h:768
   pragma Import (C, qsort_r, "qsort_r");


   function ecvt
     (value : double;
      ndigit : int;
      decpt : access int;
      sign : access int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:812
   pragma Import (C, ecvt, "ecvt");

   function fcvt
     (value : double;
      ndigit : int;
      decpt : access int;
      sign : access int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:818
   pragma Import (C, fcvt, "fcvt");

   function gcvt
     (value : double;
      ndigit : int;
      buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:824
   pragma Import (C, gcvt, "gcvt");

   function qecvt
     (value : long_double;
      ndigit : int;
      decpt : access int;
      sign : access int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:830
   pragma Import (C, qecvt, "qecvt");

   function qfcvt
     (value : long_double;
      ndigit : int;
      decpt : access int;
      sign : access int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:833
   pragma Import (C, qfcvt, "qfcvt");

   function qgcvt
     (value : long_double;
      ndigit : int;
      buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:836
   pragma Import (C, qgcvt, "qgcvt");

   function ecvt_r
     (value : double;
      ndigit : int;
      decpt : access int;
      sign : access int;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int;  -- /usr/include/stdlib.h:842
   pragma Import (C, ecvt_r, "ecvt_r");

   function fcvt_r
     (value : double;
      ndigit : int;
      decpt : access int;
      sign : access int;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int;  -- /usr/include/stdlib.h:845
   pragma Import (C, fcvt_r, "fcvt_r");

   function qecvt_r
     (value : long_double;
      ndigit : int;
      decpt : access int;
      sign : access int;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int;  -- /usr/include/stdlib.h:849
   pragma Import (C, qecvt_r, "qecvt_r");

   function qfcvt_r
     (value : long_double;
      ndigit : int;
      decpt : access int;
      sign : access int;
      buf : Interfaces.C.Strings.chars_ptr;
      len : size_t) return int;  -- /usr/include/stdlib.h:853
   pragma Import (C, qfcvt_r, "qfcvt_r");

   
   function rpmatch (response : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/stdlib.h:888
   pragma Import (C, rpmatch, "rpmatch");

   function getsubopt
     (optionp : System.Address;
      tokens : System.Address;
      valuep : System.Address) return int;  -- /usr/include/stdlib.h:899
   pragma Import (C, getsubopt, "getsubopt");

   procedure setkey (key : Interfaces.C.Strings.chars_ptr);  -- /usr/include/stdlib.h:908
   pragma Import (C, setkey, "setkey");

   function posix_openpt (oflag : int) return int;  -- /usr/include/stdlib.h:916
   pragma Import (C, posix_openpt, "posix_openpt");

   function grantpt (fd : int) return int;  -- /usr/include/stdlib.h:924
   pragma Import (C, grantpt, "grantpt");

   function unlockpt (fd : int) return int;  -- /usr/include/stdlib.h:928
   pragma Import (C, unlockpt, "unlockpt");

   function ptsname (fd : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:933
   pragma Import (C, ptsname, "ptsname");

   function ptsname_r
     (fd : int;
      buf : Interfaces.C.Strings.chars_ptr;
      buflen : size_t) return int;  -- /usr/include/stdlib.h:940
   pragma Import (C, ptsname_r, "ptsname_r");

   function getpt return int;  -- /usr/include/stdlib.h:944
   pragma Import (C, getpt, "getpt");

   function getloadavg (loadavg : access double; nelem : int) return int;  -- /usr/include/stdlib.h:951
   pragma Import (C, getloadavg, "getloadavg");
end Libc.Stdlib.GNU;
