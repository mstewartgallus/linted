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

package Libc.Wchar is
   pragma Preelaborate;

   subtype wchar_t is Libc.Stddef.wchar_t;

   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX
   --  unsupported macro: WEOF (0xffffffffu)
   subtype mbstate_t_wchb_array is Interfaces.C.char_array (0 .. 3);
   type anon_1 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            wch : aliased unsigned;  -- /usr/include/wchar.h:88
         when others =>
            wchb : aliased mbstate_t_wchb_array;  -- /usr/include/wchar.h:92
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_1);
   pragma Unchecked_Union (anon_1);
   type mbstate_t is record
      count : aliased int;  -- /usr/include/wchar.h:84
      value : anon_1;  -- /usr/include/wchar.h:93
   end record;
   pragma Convention (C_Pass_By_Copy, mbstate_t);  -- /usr/include/wchar.h:94

   --  skipped anonymous struct anon_0

   subtype mbstate_t is mbstate_t;

   function fwprintf (stream : System.Address; format : access wchar_t  -- , ...
      ) return int;  -- /usr/include/wchar.h:594
   pragma Import (C, fwprintf, "fwprintf");

   function fwscanf (stream : System.Address; format : access wchar_t  -- , ...
      ) return int;  -- /usr/include/wchar.h:635
   pragma Import (C, fwscanf, "fwscanf");

   function swprintf
     (s : access wchar_t;
      n : size_t;
      format : access wchar_t  -- , ...
      ) return int;  -- /usr/include/wchar.h:604
   pragma Import (C, swprintf, "swprintf");

   function swscanf (s : access wchar_t; format : access wchar_t  -- , ...
      ) return int;  -- /usr/include/wchar.h:645
   pragma Import (C, swscanf, "swscanf");

         function vfwprintf
     (s : System.Address;
      format : access wchar_t;
      arg : access System.Address) return int;  -- /usr/include/wchar.h:612
   pragma Import (C, vfwprintf, "vfwprintf");

   function vswprintf
     (s : access wchar_t;
      n : size_t;
      format : access wchar_t;
      arg : access System.Address) return int;  -- /usr/include/wchar.h:625
   pragma Import (C, vswprintf, "vswprintf");

   function vwprintf (format : access wchar_t; arg : access System.Address) return int;  -- /usr/include/wchar.h:620
   pragma Import (C, vwprintf, "vwprintf");

   function vwscanf (format : access wchar_t; arg : access System.Address) return int;  -- /usr/include/wchar.h:697
   pragma Import (C, vwscanf, "vwscanf");

   function wprintf (format : access wchar_t  -- , ...
      ) return int;  -- /usr/include/wchar.h:601
   pragma Import (C, wprintf, "wprintf");

   function wscanf (format : access wchar_t  -- , ...
      ) return int;  -- /usr/include/wchar.h:642
   pragma Import (C, wscanf, "wscanf");
   function fgetwc (stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:745
   pragma Import (C, fgetwc, "fgetwc");

   function fgetws
     (ws : access wchar_t;
      n : int;
      stream : System.Address) return access wchar_t;  -- /usr/include/wchar.h:774
   pragma Import (C, fgetws, "fgetws");

   function fputwc (wc : wchar_t; stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:759
   pragma Import (C, fputwc, "fputwc");

   function fwide (fp : System.Address; mode : int) return int;  -- /usr/include/wchar.h:587
   pragma Import (C, fwide, "fwide");

   function getwc (stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:746
   pragma Import (C, getwc, "getwc");

   function getwchar return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:752
   pragma Import (C, getwchar, "getwchar");

   function putwc (wc : wchar_t; stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:760
   pragma Import (C, putwc, "putwc");

   function putwchar (wc : wchar_t) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:766
   pragma Import (C, putwchar, "putwchar");

   function ungetwc (wc : Libc.Stddef.wint_t; stream : System.Address) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:789
   pragma Import (C, ungetwc, "ungetwc");

   function wcstod (nptr : access wchar_t; endptr : System.Address) return double;  -- /usr/include/wchar.h:450
   pragma Import (C, wcstod, "wcstod");

   function wcstof (nptr : access wchar_t; endptr : System.Address) return float;  -- /usr/include/wchar.h:457
   pragma Import (C, wcstof, "wcstof");

   function wcstold (nptr : access wchar_t; endptr : System.Address) return long_double;  -- /usr/include/wchar.h:459
   pragma Import (C, wcstold, "wcstold");

   function wcstol
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int) return long;  -- /usr/include/wchar.h:468
   pragma Import (C, wcstol, "wcstol");

   function wcstoul
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int) return unsigned_long;  -- /usr/include/wchar.h:473
   pragma Import (C, wcstoul, "wcstoul");

   function wcstoll
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int) return Long_Long_Integer;  -- /usr/include/wchar.h:483
   pragma Import (C, wcstoll, "wcstoll");

   function wcstoull
     (nptr : access wchar_t;
      endptr : System.Address;
      base : int) return Extensions.unsigned_long_long;  -- /usr/include/wchar.h:490
   pragma Import (C, wcstoull, "wcstoull");

      function wcscpy (dest : access wchar_t; src : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:147
   pragma Import (C, wcscpy, "wcscpy");

   function wcsncpy
     (dest : access wchar_t;
      src : access wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:150
   pragma Import (C, wcsncpy, "wcsncpy");

   function wmemcpy
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:329
   pragma Import (C, wmemcpy, "wmemcpy");

   function wmemmove
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:334
   pragma Import (C, wmemmove, "wmemmove");



   function wcscat (dest : access wchar_t; src : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:155
   pragma Import (C, wcscat, "wcscat");

   function wcsncat
     (dest : access wchar_t;
      src : access wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:158
   pragma Import (C, wcsncat, "wcsncat");

   function wcscmp (s1 : access wchar_t; s2 : access wchar_t) return int;  -- /usr/include/wchar.h:163
   pragma Import (C, wcscmp, "wcscmp");

   function wcscoll (s1 : access wchar_t; s2 : access wchar_t) return int;  -- /usr/include/wchar.h:192
   pragma Import (C, wcscoll, "wcscoll");

   function wcsncmp
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return int;  -- /usr/include/wchar.h:166
   pragma Import (C, wcsncmp, "wcsncmp");

   function wcsxfrm
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return size_t;  -- /usr/include/wchar.h:196
   pragma Import (C, wcsxfrm, "wcsxfrm");

   function wmemcmp
     (s1 : access wchar_t;
      s2 : access wchar_t;
      n : size_t) return int;  -- /usr/include/wchar.h:325
   pragma Import (C, wmemcmp, "wmemcmp");

   function wcspbrk (wcs : access wchar_t; accept : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:260
   pragma Import (C, wcspbrk, "wcspbrk");


   function wcschr (wcs : access wchar_t; wc : wchar_t) return access wchar_t;  -- /usr/include/wchar.h:222
   pragma Import (C, wcschr, "wcschr");


   function wcsspn (wcs : access wchar_t; accept : access wchar_t) return size_t;  -- /usr/include/wchar.h:256
   pragma Import (C, wcsspn, "wcsspn");

   function wcsstr (haystack : access wchar_t; needle : access wchar_t) return access wchar_t;  -- /usr/include/wchar.h:271
   pragma Import (C, wcsstr, "wcsstr");

   function wcstok
     (s : access wchar_t;
      delim : access wchar_t;
      ptr : System.Address) return access wchar_t;  -- /usr/include/wchar.h:282
   pragma Import (C, wcstok, "wcstok");

   function wmemchr
     (s : access wchar_t;
      c : wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:314
   pragma Import (C, wmemchr, "wmemchr");

   function wcslen (s : access wchar_t) return size_t;  -- /usr/include/wchar.h:287
   pragma Import (C, wcslen, "wcslen");
      function wmemset
     (s : access wchar_t;
      c : wchar_t;
      n : size_t) return access wchar_t;  -- /usr/include/wchar.h:338
   pragma Import (C, wmemset, "wmemset");

   function wcsftime
     (s : access wchar_t;
      maxsize : size_t;
      format : access wchar_t;
      tp : System.Address) return size_t;  -- /usr/include/wchar.h:855
   pragma Import (C, wcsftime, "wcsftime");

   function btowc (c : int) return Libc.Stddef.wint_t;  -- /usr/include/wchar.h:353
   pragma Import (C, btowc, "btowc");

   function wctob (c : Libc.Stddef.wint_t) return int;  -- /usr/include/wchar.h:357
   pragma Import (C, wctob, "wctob");

   function mbsinit (ps : System.Address) return int;  -- /usr/include/wchar.h:361
   pragma Import (C, mbsinit, "mbsinit");

      function mbrlen
     (s : Interfaces.C.Strings.chars_ptr;
      n : size_t;
      ps : access mbstate_t) return size_t;  -- /usr/include/wchar.h:376
   pragma Import (C, mbrlen, "mbrlen");

   function mbrtowc
     (pwc : access wchar_t;
      s : Interfaces.C.Strings.chars_ptr;
      n : size_t;
      p : access mbstate_t) return size_t;  -- /usr/include/wchar.h:365
   pragma Import (C, mbrtowc, "mbrtowc");
   function wcrtomb
     (s : Interfaces.C.Strings.chars_ptr;
      wc : wchar_t;
      ps : access mbstate_t) return size_t;  -- /usr/include/wchar.h:370
   pragma Import (C, wcrtomb, "wcrtomb");

   function mbsrtowcs
     (dst : access wchar_t;
      src : System.Address;
      len : size_t;
      ps : access mbstate_t) return size_t;  -- /usr/include/wchar.h:408
   pragma Import (C, mbsrtowcs, "mbsrtowcs");

   function wcsrtombs
     (dst : Interfaces.C.Strings.chars_ptr;
      src : System.Address;
      len : size_t;
      ps : access mbstate_t) return size_t;  -- /usr/include/wchar.h:414
   pragma Import (C, wcsrtombs, "wcsrtombs");
end Libc.Wchar;
