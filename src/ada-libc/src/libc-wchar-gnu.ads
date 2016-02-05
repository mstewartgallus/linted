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
with Libc.Stddef;
with Libc.xlocale_h;
with System;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package Libc.Wchar.GNU is
   pragma Preelaborate;

   --  skipped empty struct tm

   function wcscasecmp (s1 : access wchar_t; s2 : access wchar_t) return int;  -- /usr/include/wchar.h:172
   pragma Import (C, wcscasecmp, "wcscasecmp");

   function wcsncasecmp
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return int;  -- /usr/include/wchar.h:175
   pragma Import (C, wcsncasecmp, "wcsncasecmp");

   function wcscasecmp_l
     (s1 : access wchar_t;
      s2 : access wchar_t;
      loc : Libc.xlocale_h.locale_t) return int;  -- /usr/include/wchar.h:182
   pragma Import (C, wcscasecmp_l, "wcscasecmp_l");

   function wcsncasecmp_l
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t;
      loc : Libc.xlocale_h.locale_t) return int;  -- /usr/include/wchar.h:185
   pragma Import (C, wcsncasecmp_l, "wcsncasecmp_l");


   function wcscoll_l
     (s1 : access wchar_t;
      s2 : access wchar_t;
      loc : Libc.xlocale_h.locale_t) return int;  -- /usr/include/wchar.h:206
   pragma Import (C, wcscoll_l, "wcscoll_l");

   function wcsxfrm_l
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t;
      loc : Libc.xlocale_h.locale_t) return size_t;  -- /usr/include/wchar.h:212
   pragma Import (C, wcsxfrm_l, "wcsxfrm_l");

   function wcsdup (s : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:216
   pragma Import (C, wcsdup, "wcsdup");

   function wcschrnul (s : access wchar_t; wc : wchar_t) return access wchar_t;  -- /usr/include/wchar.h:245
   pragma Import (C, wcschrnul, "wcschrnul");


   function wcswcs (haystack : access wchar_t; needle : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:293
   pragma Import (C, wcswcs, "wcswcs");

   function wcsnlen (s : access wchar_t; maxlen : size_t) return size_t;  -- /usr/include/wchar.h:306
   pragma Import (C, wcsnlen, "wcsnlen");



   function wmempcpy
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:344
   pragma Import (C, wmempcpy, "wmempcpy");


   function mbsnrtowcs
     (dst : access wchar_t;
      src : System.Address;
      nmc : size_t;
      len : size_t;
      ps : access mbstate_t) return size_t;  -- /usr/include/wchar.h:423
   pragma Import (C, mbsnrtowcs, "mbsnrtowcs");

   function wcsnrtombs
     (dst : Interfaces.C.Strings.chars_ptr;
      src : System.Address;
      nwc : size_t;
      len : size_t;
      ps : access mbstate_t) return size_t;  -- /usr/include/wchar.h:429
   pragma Import (C, wcsnrtombs, "wcsnrtombs");

   function wcwidth (c : wchar_t) return int;  -- /usr/include/wchar.h:439
   pragma Import (C, wcwidth, "wcwidth");

   function wcswidth (s : access wchar_t; n : size_t) return int;  -- /usr/include/wchar.h:443
   pragma Import (C, wcswidth, "wcswidth");


   function wcstoq
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int) return Long_Long_Integer;  -- /usr/include/wchar.h:500
   pragma Import (C, wcstoq, "wcstoq");

   function wcstouq
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int) return Extensions.unsigned_long_long;  -- /usr/include/wchar.h:507
   pragma Import (C, wcstouq, "wcstouq");

   function wcstol_l
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int;
      loc : Libc.xlocale_h.locale_t) return long;  -- /usr/include/wchar.h:530
   pragma Import (C, wcstol_l, "wcstol_l");

   function wcstoul_l
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int;
      loc : Libc.xlocale_h.locale_t) return unsigned_long;  -- /usr/include/wchar.h:534
   pragma Import (C, wcstoul_l, "wcstoul_l");

   function wcstoll_l
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int;
      loc : Libc.xlocale_h.locale_t) return Long_Long_Integer;  -- /usr/include/wchar.h:539
   pragma Import (C, wcstoll_l, "wcstoll_l");

   function wcstoull_l
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int;
      loc : Libc.xlocale_h.locale_t) return Extensions.unsigned_long_long;  -- /usr/include/wchar.h:544
   pragma Import (C, wcstoull_l, "wcstoull_l");

   function wcstod_l
     (nptr : access wchar_t;
      endptr : System.Address;
      loc : Libc.xlocale_h.locale_t) return double;  -- /usr/include/wchar.h:549
   pragma Import (C, wcstod_l, "wcstod_l");

   function wcstof_l
     (nptr : access wchar_t;
      endptr : System.Address;
      loc : Libc.xlocale_h.locale_t) return float;  -- /usr/include/wchar.h:553
   pragma Import (C, wcstof_l, "wcstof_l");

   function wcstold_l
     (nptr : access wchar_t;
      endptr : System.Address;
      loc : Libc.xlocale_h.locale_t) return long_double;  -- /usr/include/wchar.h:557
   pragma Import (C, wcstold_l, "wcstold_l");

   function wcpcpy (dest : access wchar_t; src : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:566
   pragma Import (C, wcpcpy, "wcpcpy");

   function wcpncpy
     (dest : access wchar_t;
      src : access wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:571
   pragma Import (C, wcpncpy, "wcpncpy");

   function open_wmemstream (bufloc : System.Address; sizeloc : access size_t) return System.Address;  -- /usr/include/wchar.h:580
   pragma Import (C, open_wmemstream, "open_wmemstream");


   function vfwscanf
     (s : System.Address;
      format : access wchar_t;
      arg : access System.Address) return int;  -- /usr/include/wchar.h:689
   pragma Import (C, vfwscanf, "vfwscanf");


   function vswscanf
     (s : access wchar_t;
      format : access wchar_t;
      arg : access System.Address) return int;  -- /usr/include/wchar.h:701
   pragma Import (C, vswscanf, "vswscanf");


   function fputws (ws : access wchar_t; stream : System.Address) return int;  -- /usr/include/wchar.h:781
   pragma Import (C, fputws, "fputws");

   function getwc_unlocked (stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:801
   pragma Import (C, getwc_unlocked, "getwc_unlocked");

   function getwchar_unlocked return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:802
   pragma Import (C, getwchar_unlocked, "getwchar_unlocked");

   function fgetwc_unlocked (stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:810
   pragma Import (C, fgetwc_unlocked, "fgetwc_unlocked");

   function fputwc_unlocked (wc : wchar_t; stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:818
   pragma Import (C, fputwc_unlocked, "fputwc_unlocked");

   function putwc_unlocked (wc : wchar_t; stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:827
   pragma Import (C, putwc_unlocked, "putwc_unlocked");

   function putwchar_unlocked (wc : wchar_t) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:828
   pragma Import (C, putwchar_unlocked, "putwchar_unlocked");

   function fgetws_unlocked
     (ws : access wchar_t;
      n : int;
      stream : System.Address) return access wchar_t;  -- /usr/include/wchar.h:837
   pragma Import (C, fgetws_unlocked, "fgetws_unlocked");

   function fputws_unlocked (ws : access wchar_t; stream : System.Address) return int;  -- /usr/include/wchar.h:846
   pragma Import (C, fputws_unlocked, "fputws_unlocked");


   function wcsftime_l
     (s : access wchar_t;
      maxsize : size_t;
      format : access wchar_t;
      tp : System.Address;
      loc : Libc.xlocale_h.locale_t) return size_t;  -- /usr/include/wchar.h:865
   pragma Import (C, wcsftime_l, "wcsftime_l");

end Libc.Wchar.GNU;
