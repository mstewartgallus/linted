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

package Libc.Stdlib with
     Spark_Mode => Off is
   pragma Preelaborate;

   subtype size_t is Interfaces.C.size_t;
   subtype wchar_t is int;

   type div_t is record
      quot : aliased int;  -- /usr/include/stdlib.h:99
      c_rem : aliased int;  -- /usr/include/stdlib.h:100
   end record;
   pragma Convention (C_Pass_By_Copy, div_t);  -- /usr/include/stdlib.h:101

   type ldiv_t is record
      quot : aliased long;  -- /usr/include/stdlib.h:107
      c_rem : aliased long;  -- /usr/include/stdlib.h:108
   end record;
   pragma Convention (C_Pass_By_Copy, ldiv_t);  -- /usr/include/stdlib.h:109

   --  type lldiv_t is record
   --     quot : aliased Long_Long_Integer;  -- /usr/include/stdlib.h:119
   --     c_rem : aliased Long_Long_Integer;  -- /usr/include/stdlib.h:120
   --  end record;
--  pragma Convention (C_Pass_By_Copy, lldiv_t);  -- /usr/include/stdlib.h:121

   RAND_MAX : constant := 2147483647;
   EXIT_FAILURE : constant := 1;
   EXIT_SUCCESS : constant := 0;
   --  unsupported macro: MB_CUR_MAX (__ctype_get_mb_cur_max ())

   function atof
     (nptr : Interfaces.C.Strings.chars_ptr)
     return double;  -- /usr/include/stdlib.h:144
   pragma Import (C, atof, "atof");

   function atoi
     (nptr : Interfaces.C.Strings.chars_ptr)
     return int;  -- /usr/include/stdlib.h:147
   pragma Import (C, atoi, "atoi");

   function atol
     (nptr : Interfaces.C.Strings.chars_ptr)
     return long;  -- /usr/include/stdlib.h:150
   pragma Import (C, atol, "atol");

   --  function atoll (nptr : Interfaces.C.Strings.chars_ptr) return Long_Long_Integer;  -- /usr/include/stdlib.h:157
   --  pragma Import (C, atoll, "atoll");

   function strtod
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address) return double;  -- /usr/include/stdlib.h:164
   pragma Import (C, strtod, "strtod");

   function strtof
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address) return Float;  -- /usr/include/stdlib.h:172
   pragma Import (C, strtof, "strtof");

   function strtold
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address)
     return long_double;  -- /usr/include/stdlib.h:175
   pragma Import (C, strtold, "strtold");

   function strtol
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address;
      base : int) return long;  -- /usr/include/stdlib.h:183
   pragma Import (C, strtol, "strtol");

   --  function strtoll
   --    (nptr : Interfaces.C.Strings.chars_ptr;
   --     endptr : System.Address;
   --     base : int) return Long_Long_Integer;  -- /usr/include/stdlib.h:209
   --  pragma Import (C, strtoll, "strtoll");

   function strtoul
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address;
      base : int) return unsigned_long;  -- /usr/include/stdlib.h:187
   pragma Import (C, strtoul, "strtoul");

   function strtoull
     (nptr : Interfaces.C.Strings.chars_ptr;
      endptr : System.Address;
      base : int)
     return Extensions.unsigned_long_long;  -- /usr/include/stdlib.h:214
   pragma Import (C, strtoull, "strtoull");

   function rand return int;  -- /usr/include/stdlib.h:374
   pragma Import (C, rand, "rand");

   procedure srand (seed : unsigned);  -- /usr/include/stdlib.h:376
   pragma Import (C, srand, "srand");

   function calloc
     (nmemb : size_t;
      size : size_t) return System.Address;  -- /usr/include/stdlib.h:468
   pragma Import (C, calloc, "calloc");

   procedure free (ptr : System.Address);  -- /usr/include/stdlib.h:483
   pragma Import (C, free, "free");

   function malloc
     (size : size_t) return System.Address;  -- /usr/include/stdlib.h:466
   pragma Import (C, malloc, "malloc");

   function realloc
     (ptr : System.Address;
      size : size_t) return System.Address;  -- /usr/include/stdlib.h:480
   pragma Import (C, realloc, "realloc");

   procedure c_abort;  -- /usr/include/stdlib.h:515
   pragma Import (C, c_abort, "abort");

   function atexit
     (func : access procedure) return int;  -- /usr/include/stdlib.h:519
   pragma Import (C, atexit, "atexit");

   procedure c_exit (status : int);  -- /usr/include/stdlib.h:543
   pragma Import (C, c_exit, "exit");

   --  skipped func _Exit

   function getenv
     (name : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/stdlib.h:564
   pragma Import (C, getenv, "getenv");

   function c_system
     (command : Interfaces.C.Strings.chars_ptr)
     return int;  -- /usr/include/stdlib.h:717
   pragma Import (C, c_system, "system");

   type compar_fn_t is access function
     (arg1 : System.Address;
      arg2 : System.Address) return int;
   pragma Convention (C, compar_fn_t);  -- /usr/include/stdlib.h:742

   function bsearch
     (key : System.Address;
      base : System.Address;
      nmemb : size_t;
      size : size_t;
      compar : compar_fn_t)
     return System.Address;  -- /usr/include/stdlib.h:755
   pragma Import (C, bsearch, "bsearch");

   procedure qsort
     (base : System.Address;
      nmemb : size_t;
      size : size_t;
      compar : compar_fn_t);  -- /usr/include/stdlib.h:765
   pragma Import (C, qsort, "qsort");

   function c_abs (x : int) return int;  -- /usr/include/stdlib.h:775
   pragma Import (C, c_abs, "abs");

   function labs (x : long) return long;  -- /usr/include/stdlib.h:776
   pragma Import (C, labs, "labs");

   --  function llabs (x : Long_Long_Integer) return Long_Long_Integer;  -- /usr/include/stdlib.h:780
   --  pragma Import (C, llabs, "llabs");

   function div
     (numer : int;
      denom : int) return div_t;  -- /usr/include/stdlib.h:789
   pragma Import (C, div, "div");

   function ldiv
     (numer : long;
      denom : long) return ldiv_t;  -- /usr/include/stdlib.h:791
   pragma Import (C, ldiv, "ldiv");

   --  function lldiv (numer : Long_Long_Integer; denom : Long_Long_Integer) return lldiv_t;  -- /usr/include/stdlib.h:797
   --  pragma Import (C, lldiv, "lldiv");

   function mblen
     (s : Interfaces.C.Strings.chars_ptr;
      n : size_t) return int;  -- /usr/include/stdlib.h:863
   pragma Import (C, mblen, "mblen");

   function mbtowc
     (pwc : access wchar_t;
      s : Interfaces.C.Strings.chars_ptr;
      n : size_t) return int;  -- /usr/include/stdlib.h:866
   pragma Import (C, mbtowc, "mbtowc");

   function wctomb
     (s : Interfaces.C.Strings.chars_ptr;
      wchar : wchar_t) return int;  -- /usr/include/stdlib.h:870
   pragma Import (C, wctomb, "wctomb");

   function mbstowcs
     (pwcs : access wchar_t;
      s : Interfaces.C.Strings.chars_ptr;
      n : size_t) return size_t;  -- /usr/include/stdlib.h:874
   pragma Import (C, mbstowcs, "mbstowcs");

   function wcstombs
     (s : Interfaces.C.Strings.chars_ptr;
      pwcs : access wchar_t;
      n : size_t) return size_t;  -- /usr/include/stdlib.h:877
   pragma Import (C, wcstombs, "wcstombs");
end Libc.Stdlib;
