-- Copyright 2016 Steven Stewart-Gallus
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
with Interfaces.C;
with System;
with Libc.Stddef;
with Interfaces.C.Strings;

package Libc.String is
   pragma Preelaborate;

   --  arg-macro: function strdupa (__extension__ ({ const char *__old := (s); size_t __len := strlen (__old) + 1; char *__new := (char *) __builtin_alloca (__len); (char *) memcpy (__new, __old, __len); })
   --    return __extension__ ({ const char *__old := (s); size_t __len := strlen (__old) + 1; char *__new := (char *) __builtin_alloca (__len); (char *) memcpy (__new, __old, __len); });
   --  arg-macro: function strndupa (__extension__ ({ const char *__old := (s); size_t __len := strnlen (__old, (n)); char *__new := (char *) __builtin_alloca (__len + 1); __new(__len) := Character'Val (0); (char *) memcpy (__new, __old, __len); })
   --    return __extension__ ({ const char *__old := (s); size_t __len := strnlen (__old, (n)); char *__new := (char *) __builtin_alloca (__len + 1); __new(__len) := Character'Val (0); (char *) memcpy (__new, __old, __len); });
   -- Copyright (C) 1991-2014 Free Software Foundation, Inc.
   --   This file is part of the GNU C Library.
   --   The GNU C Library is free software; you can redistribute it and/or
   --   modify it under the terms of the GNU Lesser General Public
   --   License as published by the Free Software Foundation; either
   --   version 2.1 of the License, or (at your option) any later version.
   --   The GNU C Library is distributed in the hope that it will be useful,
   --   but WITHOUT ANY WARRANTY; without even the implied warranty of
   --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   --   Lesser General Public License for more details.
   --   You should have received a copy of the GNU Lesser General Public
   --   License along with the GNU C Library; if not, see
   --   <http://www.gnu.org/licenses/>.

   -- * ISO C99 Standard: 7.21 String handling  <string.h>
   --

   -- Get size_t and NULL from <stddef.h>.
   -- Provide correct C++ prototypes, and indicate this to the caller.  This
   --   requires a compatible C++ standard library.  As a heuristic, we provide
   --   these when the compiler indicates full conformance with C++98 or later,
   --   and for older GCC versions that are known to provide a compatible
   --   libstdc++.

   -- Copy N bytes of SRC to DEST.
   function memcpy
     (dest : System.Address;
      src : System.Address;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:46
   pragma Import (C, memcpy, "memcpy");

   -- Copy N bytes of SRC to DEST, guaranteeing
   --   correct behavior for overlapping strings.

   function memmove
     (dest : System.Address;
      src : System.Address;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:50
   pragma Import (C, memmove, "memmove");

   -- Copy no more than N bytes of SRC to DEST, stopping when C is found.
   --   Return the position in DEST one byte past where C was copied,
   --   or NULL if C was not found in the first N bytes of SRC.

   function memccpy
     (dest : System.Address;
      src : System.Address;
      c : Interfaces.C.int;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:58
   pragma Import (C, memccpy, "memccpy");

   -- Set N bytes of S to C.
   function memset
     (s : System.Address;
      c : Interfaces.C.int;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:66
   pragma Import (C, memset, "memset");

   -- Compare N bytes of S1 and S2.
   function memcmp
     (s1 : System.Address;
      s2 : System.Address;
      n : Libc.Stddef.size_t)
     return Interfaces.C.int;  -- /usr/include/string.h:69
   pragma Import (C, memcmp, "memcmp");

   -- Search N bytes of S for C.
   function memchr
     (s : System.Address;
      c : Interfaces.C.int;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:76
   pragma Import (C, memchr, "memchr");

   -- Search in S for C.  This is similar to `memchr' but there is no
   --   length limit.

   function rawmemchr
     (s : System.Address;
      c : Interfaces.C.int)
     return System.Address;  -- /usr/include/string.h:105
   pragma Import (C, rawmemchr, "rawmemchr");

   -- Search N bytes of S for the final occurrence of C.
   function memrchr
     (s : System.Address;
      c : Interfaces.C.int;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:116
   pragma Import (C, memrchr, "memrchr");

   -- Copy SRC to DEST.
   function strcpy
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:129
   pragma Import (C, strcpy, "strcpy");

   -- Copy no more than N characters of SRC to DEST.
   function strncpy
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:132
   pragma Import (C, strncpy, "strncpy");

   -- Append SRC onto DEST.
   function strcat
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:137
   pragma Import (C, strcat, "strcat");

   -- Append no more than N characters from SRC onto DEST.
   function strncat
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:140
   pragma Import (C, strncat, "strncat");

   -- Compare S1 and S2.
   function strcmp
     (s1 : Interfaces.C.Strings.chars_ptr;
      s2 : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int;  -- /usr/include/string.h:144
   pragma Import (C, strcmp, "strcmp");

   -- Compare N characters of S1 and S2.
   function strncmp
     (s1 : Interfaces.C.Strings.chars_ptr;
      s2 : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Interfaces.C.int;  -- /usr/include/string.h:147
   pragma Import (C, strncmp, "strncmp");

   -- Compare the collated forms of S1 and S2.
   function strcoll
     (s1 : Interfaces.C.Strings.chars_ptr;
      s2 : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int;  -- /usr/include/string.h:151
   pragma Import (C, strcoll, "strcoll");

   -- Put a transformation of SRC into no more than N bytes of DEST.
   function strxfrm
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Libc.Stddef.size_t;  -- /usr/include/string.h:154
   pragma Import (C, strxfrm, "strxfrm");

   -- The following functions are equivalent to the both above but they
   --   take the locale they use for the collation as an extra argument.
   --   This is not standardsized but something like will come.

   -- Compare the collated forms of S1 and S2 using rules from L.
   --  function strcoll_l
   --    (s1 : Interfaces.C.Strings.chars_ptr;
   --     s2 : Interfaces.C.Strings.chars_ptr;
   --     l : xlocale_h.locale_t) return Interfaces.C.int;  -- /usr/include/string.h:166
   --  pragma Import (C, strcoll_l, "strcoll_l");

   -- Put a transformation of SRC into no more than N bytes of DEST.
   --  function strxfrm_l
   --    (dest : Interfaces.C.Strings.chars_ptr;
   --     src : Interfaces.C.Strings.chars_ptr;
   --     n : Libc.Stddef.size_t;
   --     l : xlocale_h.locale_t) return Libc.Stddef.size_t;  -- /usr/include/string.h:169
   --  pragma Import (C, strxfrm_l, "strxfrm_l");

   -- Duplicate S, returning an identical malloc'd string.
   function strdup
     (s : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:176
   pragma Import (C, strdup, "strdup");

   -- Return a malloc'd copy of at most N bytes of STRING.  The
   --   resultant string is terminated even if no null terminator
   --   appears before STRING[N].

   function strndup
     (string : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:184
   pragma Import (C, strndup, "strndup");

   -- Duplicate S, returning an identical alloca'd string.
   -- Return an alloca'd copy of at most N bytes of string.
   -- Find the first occurrence of C in S.
   function strchr
     (s : Interfaces.C.Strings.chars_ptr;
      c : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:216
   pragma Import (C, strchr, "strchr");

   -- Find the last occurrence of C in S.
   function strrchr
     (s : Interfaces.C.Strings.chars_ptr;
      c : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:243
   pragma Import (C, strrchr, "strrchr");

   -- This function is similar to `strchr'.  But it returns a pointer to
   --   the closing NUL byte in case C is not found in S.

   function strchrnul
     (s : Interfaces.C.Strings.chars_ptr;
      c : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:272
   pragma Import (C, strchrnul, "strchrnul");

   -- Return the length of the initial segment of S which
   --   consists entirely of characters not in REJECT.

   function strcspn
     (s : Interfaces.C.Strings.chars_ptr;
      reject : Interfaces.C.Strings.chars_ptr)
     return Libc.Stddef.size_t;  -- /usr/include/string.h:285
   pragma Import (C, strcspn, "strcspn");

   -- Return the length of the initial segment of S which
   --   consists entirely of characters in ACCEPT.

   function strspn
     (s : Interfaces.C.Strings.chars_ptr;
      uu_accept : Interfaces.C.Strings.chars_ptr)
     return Libc.Stddef.size_t;  -- /usr/include/string.h:289
   pragma Import (C, strspn, "strspn");

   -- Find the first occurrence in S of any character in ACCEPT.
   function strpbrk
     (s : Interfaces.C.Strings.chars_ptr;
      uu_accept : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:295
   pragma Import (C, strpbrk, "strpbrk");

   -- Find the first occurrence of NEEDLE in HAYSTACK.
   function strstr
     (haystack : Interfaces.C.Strings.chars_ptr;
      needle : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:322
   pragma Import (C, strstr, "strstr");

   -- Divide S into tokens separated by characters in DELIM.
   function strtok
     (s : Interfaces.C.Strings.chars_ptr;
      delim : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:348
   pragma Import (C, strtok, "strtok");

   -- Divide S into tokens separated by characters in DELIM.  Information
   --   passed between calls are stored in SAVE_PTR.

   --  skipped func __strtok_r

   function strtok_r
     (s : Interfaces.C.Strings.chars_ptr;
      delim : Interfaces.C.Strings.chars_ptr;
      save_ptr : System.Address)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:359
   pragma Import (C, strtok_r, "strtok_r");

   -- Similar to `strstr' but this function ignores the case of both strings.
   function strcasestr
     (haystack : Interfaces.C.Strings.chars_ptr;
      needle : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:367
   pragma Import (C, strcasestr, "strcasestr");

   -- Find the first occurrence of NEEDLE in HAYSTACK.
   --   NEEDLE is NEEDLELEN bytes long;
   --   HAYSTACK is HAYSTACKLEN bytes long.

   function memmem
     (haystack : System.Address;
      haystacklen : Libc.Stddef.size_t;
      needle : System.Address;
      needlelen : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:382
   pragma Import (C, memmem, "memmem");

   -- Copy N bytes of SRC to DEST, return pointer to bytes after the
   --   last written byte.

   --  skipped func __mempcpy

   function mempcpy
     (dest : System.Address;
      src : System.Address;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:391
   pragma Import (C, mempcpy, "mempcpy");

   -- Return the length of S.
   function strlen
     (s : Interfaces.C.Strings.chars_ptr)
     return Libc.Stddef.size_t;  -- /usr/include/string.h:399
   pragma Import (C, strlen, "strlen");

   -- Find the length of STRING, but scan at most MAXLEN characters.
   --   If no '\0' terminator is found in that many characters, return MAXLEN.

   function strnlen
     (string : Interfaces.C.Strings.chars_ptr;
      maxlen : Libc.Stddef.size_t)
     return Libc.Stddef.size_t;  -- /usr/include/string.h:406
   pragma Import (C, strnlen, "strnlen");

   -- Return a string describing the meaning of the `errno' code in ERRNUM.
   function strerror
     (errnum : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:413
   pragma Import (C, strerror, "strerror");

   -- Reentrant version of `strerror'.
   --   There are 2 flavors of `strerror_r', GNU which returns the string
   --   and may or may not use the supplied temporary buffer and POSIX one
   --   which fills the string into the buffer.
--   To use the POSIX version, -D_XOPEN_SOURCE=600 or -D_POSIX_C_SOURCE=200112L
   --   without -D_GNU_SOURCE is needed, otherwise the GNU version is
   --   preferred.

   -- Fill BUF with a string describing the meaning of the `errno' code in
   --   ERRNUM.

   -- If a temporary buffer is required, at most BUFLEN bytes of BUF will be
   --   used.

   function strerror_r
     (errnum : Interfaces.C.int;
      buf : in out Interfaces.C.char_array;
      buflen : Libc.Stddef.size_t) return Interfaces.C.int;
   pragma Import (C, strerror_r, "__xpg_strerror_r");

   -- Translate error number to string according to the locale L.
   --  function strerror_l (errnum : Interfaces.C.int; l : xlocale_h.locale_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:445
   --  pragma Import (C, strerror_l, "strerror_l");

   -- We define this function always since `bzero' is sometimes needed when
   --   the namespace rules does not allow this.

   --  skipped func __bzero

   -- Copy N bytes of SRC to DEST (like memmove, but args reversed).
   procedure bcopy
     (src : System.Address;
      dest : System.Address;
      n : Libc.Stddef.size_t);  -- /usr/include/string.h:455
   pragma Import (C, bcopy, "bcopy");

   -- Set N bytes of S to 0.
   procedure bzero
     (s : System.Address;
      n : Libc.Stddef.size_t);  -- /usr/include/string.h:459
   pragma Import (C, bzero, "bzero");

   -- Compare N bytes of S1 and S2 (same as memcmp).
   function bcmp
     (s1 : System.Address;
      s2 : System.Address;
      n : Libc.Stddef.size_t)
     return Interfaces.C.int;  -- /usr/include/string.h:462
   pragma Import (C, bcmp, "bcmp");

   -- Find the first occurrence of C in S (same as strchr).
   function index
     (s : Interfaces.C.Strings.chars_ptr;
      c : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:469
   pragma Import (C, index, "index");

   -- Find the last occurrence of C in S (same as strrchr).
   function rindex
     (s : Interfaces.C.Strings.chars_ptr;
      c : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:497
   pragma Import (C, rindex, "rindex");

   -- Return the position of the first bit set in I, or 0 if none are set.
   --   The least-significant bit is position 1, the most-significant 32.

   function ffs
     (i : Interfaces.C.int)
     return Interfaces.C.int;  -- /usr/include/string.h:523
   pragma Import (C, ffs, "ffs");

   -- The following two functions are non-standard but necessary for non-32 bit
   --   platforms.

   function ffsl
     (l : Interfaces.C.long)
     return Interfaces.C.int;  -- /usr/include/string.h:528
   pragma Import (C, ffsl, "ffsl");

   --  function ffsll (ll : Long_Long_Integer) return Interfaces.C.int;  -- /usr/include/string.h:529
   --  pragma Import (C, ffsll, "ffsll");

   -- Compare S1 and S2, ignoring case.
   function strcasecmp
     (s1 : Interfaces.C.Strings.chars_ptr;
      s2 : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int;  -- /usr/include/string.h:534
   pragma Import (C, strcasecmp, "strcasecmp");

   -- Compare no more than N chars of S1 and S2, ignoring case.
   function strncasecmp
     (s1 : Interfaces.C.Strings.chars_ptr;
      s2 : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Interfaces.C.int;  -- /usr/include/string.h:538
   pragma Import (C, strncasecmp, "strncasecmp");

   -- Again versions of a few functions which use the given locale instead
   --   of the global one.

   --  function strcasecmp_l
   --    (s1 : Interfaces.C.Strings.chars_ptr;
   --     s2 : Interfaces.C.Strings.chars_ptr;
   --     loc : xlocale_h.locale_t) return Interfaces.C.int;  -- /usr/include/string.h:545
   --  pragma Import (C, strcasecmp_l, "strcasecmp_l");

   --  function strncasecmp_l
   --    (s1 : Interfaces.C.Strings.chars_ptr;
   --     s2 : Interfaces.C.Strings.chars_ptr;
   --     n : Libc.Stddef.size_t;
   --     loc : xlocale_h.locale_t) return Interfaces.C.int;  -- /usr/include/string.h:549
   --  pragma Import (C, strncasecmp_l, "strncasecmp_l");

   -- Return the next DELIM-delimited token from *STRINGP,
   --   terminating it with a '\0', and update *STRINGP to point past it.

   function strsep
     (stringp : System.Address;
      delim : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:557
   pragma Import (C, strsep, "strsep");

   -- Return a string describing the meaning of the signal number in SIG.
   function strsignal
     (sig : Interfaces.C.int)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:564
   pragma Import (C, strsignal, "strsignal");

   -- Copy SRC to DEST, returning the address of the terminating '\0' in DEST.
   --  skipped func __stpcpy

   function stpcpy
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:569
   pragma Import (C, stpcpy, "stpcpy");

   -- Copy no more than N characters of SRC to DEST, returning the address of
   --   the last character written into DEST.

   --  skipped func __stpncpy

   function stpncpy
     (dest : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr;
      n : Libc.Stddef.size_t)
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:577
   pragma Import (C, stpncpy, "stpncpy");

   -- Compare S1 and S2 as strings holding name & indices/version numbers.
   function strverscmp
     (s1 : Interfaces.C.Strings.chars_ptr;
      s2 : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int;  -- /usr/include/string.h:584
   pragma Import (C, strverscmp, "strverscmp");

   -- Sautee STRING briskly.
   function strfry
     (string : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:588
   pragma Import (C, strfry, "strfry");

   -- Frobnicate N bytes of S.
   function memfrob
     (s : System.Address;
      n : Libc.Stddef.size_t)
     return System.Address;  -- /usr/include/string.h:591
   pragma Import (C, memfrob, "memfrob");

   -- Return the file name within directory of FILENAME.  We don't
   --   declare the function if the `basename' macro is available (defined
   --   in <libgen.h>) which makes the XPG version of this function
   --   available.

   function basename
     (filename : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /usr/include/string.h:599
   pragma Import (C, basename, "basename");

   -- When using GNU CC we provide some optimized versions of selected
   --   functions from this header.  There are two kinds of optimizations:
   --   - machine-dependent optimizations, most probably using inline
   --     assembler code; these might be quite expensive since the code
   --     size can increase significantly.
   --     These optimizations are not used unless the symbol
   --   __USE_STRING_INLINES
   --     is defined before including this header.
   --   - machine-independent optimizations which do not increase the
   --     code size significantly and which optimize mainly situations
   --     where one or more arguments are compile-time constants.
   --     These optimizations are used always when the compiler is
   --     taught to optimize.
   --   One can inhibit all optimizations by defining __NO_STRING_INLINES.

   -- Get the machine-dependent optimizations (if any).
   -- These are generic optimizations which do not add too much inline code.
   -- Functions with security checks.
end Libc.String;
