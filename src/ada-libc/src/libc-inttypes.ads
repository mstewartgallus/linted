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
with Libc.Stdint;
with Interfaces.C.Strings;
with System;

package Libc.Inttypes is
   pragma Preelaborate;

   --  unsupported macro: PRId8 "d"
   --  unsupported macro: PRId16 "d"
   --  unsupported macro: PRId32 "d"
   --  unsupported macro: PRId64 __PRI64_PREFIX "d"
   --  unsupported macro: PRIdLEAST8 "d"
   --  unsupported macro: PRIdLEAST16 "d"
   --  unsupported macro: PRIdLEAST32 "d"
   --  unsupported macro: PRIdLEAST64 __PRI64_PREFIX "d"
   --  unsupported macro: PRIdFAST8 "d"
   --  unsupported macro: PRIdFAST16 __PRIPTR_PREFIX "d"
   --  unsupported macro: PRIdFAST32 __PRIPTR_PREFIX "d"
   --  unsupported macro: PRIdFAST64 __PRI64_PREFIX "d"
   --  unsupported macro: PRIi8 "i"
   --  unsupported macro: PRIi16 "i"
   --  unsupported macro: PRIi32 "i"
   --  unsupported macro: PRIi64 __PRI64_PREFIX "i"
   --  unsupported macro: PRIiLEAST8 "i"
   --  unsupported macro: PRIiLEAST16 "i"
   --  unsupported macro: PRIiLEAST32 "i"
   --  unsupported macro: PRIiLEAST64 __PRI64_PREFIX "i"
   --  unsupported macro: PRIiFAST8 "i"
   --  unsupported macro: PRIiFAST16 __PRIPTR_PREFIX "i"
   --  unsupported macro: PRIiFAST32 __PRIPTR_PREFIX "i"
   --  unsupported macro: PRIiFAST64 __PRI64_PREFIX "i"
   --  unsupported macro: PRIo8 "o"
   --  unsupported macro: PRIo16 "o"
   --  unsupported macro: PRIo32 "o"
   --  unsupported macro: PRIo64 __PRI64_PREFIX "o"
   --  unsupported macro: PRIoLEAST8 "o"
   --  unsupported macro: PRIoLEAST16 "o"
   --  unsupported macro: PRIoLEAST32 "o"
   --  unsupported macro: PRIoLEAST64 __PRI64_PREFIX "o"
   --  unsupported macro: PRIoFAST8 "o"
   --  unsupported macro: PRIoFAST16 __PRIPTR_PREFIX "o"
   --  unsupported macro: PRIoFAST32 __PRIPTR_PREFIX "o"
   --  unsupported macro: PRIoFAST64 __PRI64_PREFIX "o"
   --  unsupported macro: PRIu8 "u"
   --  unsupported macro: PRIu16 "u"
   --  unsupported macro: PRIu32 "u"
   --  unsupported macro: PRIu64 __PRI64_PREFIX "u"
   --  unsupported macro: PRIuLEAST8 "u"
   --  unsupported macro: PRIuLEAST16 "u"
   --  unsupported macro: PRIuLEAST32 "u"
   --  unsupported macro: PRIuLEAST64 __PRI64_PREFIX "u"
   --  unsupported macro: PRIuFAST8 "u"
   --  unsupported macro: PRIuFAST16 __PRIPTR_PREFIX "u"
   --  unsupported macro: PRIuFAST32 __PRIPTR_PREFIX "u"
   --  unsupported macro: PRIuFAST64 __PRI64_PREFIX "u"
   --  unsupported macro: PRIx8 "x"
   --  unsupported macro: PRIx16 "x"
   --  unsupported macro: PRIx32 "x"
   --  unsupported macro: PRIx64 __PRI64_PREFIX "x"
   --  unsupported macro: PRIxLEAST8 "x"
   --  unsupported macro: PRIxLEAST16 "x"
   --  unsupported macro: PRIxLEAST32 "x"
   --  unsupported macro: PRIxLEAST64 __PRI64_PREFIX "x"
   --  unsupported macro: PRIxFAST8 "x"
   --  unsupported macro: PRIxFAST16 __PRIPTR_PREFIX "x"
   --  unsupported macro: PRIxFAST32 __PRIPTR_PREFIX "x"
   --  unsupported macro: PRIxFAST64 __PRI64_PREFIX "x"
   --  unsupported macro: PRIX8 "X"
   --  unsupported macro: PRIX16 "X"
   --  unsupported macro: PRIX32 "X"
   --  unsupported macro: PRIX64 __PRI64_PREFIX "X"
   --  unsupported macro: PRIXLEAST8 "X"
   --  unsupported macro: PRIXLEAST16 "X"
   --  unsupported macro: PRIXLEAST32 "X"
   --  unsupported macro: PRIXLEAST64 __PRI64_PREFIX "X"
   --  unsupported macro: PRIXFAST8 "X"
   --  unsupported macro: PRIXFAST16 __PRIPTR_PREFIX "X"
   --  unsupported macro: PRIXFAST32 __PRIPTR_PREFIX "X"
   --  unsupported macro: PRIXFAST64 __PRI64_PREFIX "X"
   --  unsupported macro: PRIdMAX __PRI64_PREFIX "d"
   --  unsupported macro: PRIiMAX __PRI64_PREFIX "i"
   --  unsupported macro: PRIoMAX __PRI64_PREFIX "o"
   --  unsupported macro: PRIuMAX __PRI64_PREFIX "u"
   --  unsupported macro: PRIxMAX __PRI64_PREFIX "x"
   --  unsupported macro: PRIXMAX __PRI64_PREFIX "X"
   --  unsupported macro: PRIdPTR __PRIPTR_PREFIX "d"
   --  unsupported macro: PRIiPTR __PRIPTR_PREFIX "i"
   --  unsupported macro: PRIoPTR __PRIPTR_PREFIX "o"
   --  unsupported macro: PRIuPTR __PRIPTR_PREFIX "u"
   --  unsupported macro: PRIxPTR __PRIPTR_PREFIX "x"
   --  unsupported macro: PRIXPTR __PRIPTR_PREFIX "X"
   --  unsupported macro: SCNd8 "hhd"
   --  unsupported macro: SCNd16 "hd"
   --  unsupported macro: SCNd32 "d"
   --  unsupported macro: SCNd64 __PRI64_PREFIX "d"
   --  unsupported macro: SCNdLEAST8 "hhd"
   --  unsupported macro: SCNdLEAST16 "hd"
   --  unsupported macro: SCNdLEAST32 "d"
   --  unsupported macro: SCNdLEAST64 __PRI64_PREFIX "d"
   --  unsupported macro: SCNdFAST8 "hhd"
   --  unsupported macro: SCNdFAST16 __PRIPTR_PREFIX "d"
   --  unsupported macro: SCNdFAST32 __PRIPTR_PREFIX "d"
   --  unsupported macro: SCNdFAST64 __PRI64_PREFIX "d"
   --  unsupported macro: SCNi8 "hhi"
   --  unsupported macro: SCNi16 "hi"
   --  unsupported macro: SCNi32 "i"
   --  unsupported macro: SCNi64 __PRI64_PREFIX "i"
   --  unsupported macro: SCNiLEAST8 "hhi"
   --  unsupported macro: SCNiLEAST16 "hi"
   --  unsupported macro: SCNiLEAST32 "i"
   --  unsupported macro: SCNiLEAST64 __PRI64_PREFIX "i"
   --  unsupported macro: SCNiFAST8 "hhi"
   --  unsupported macro: SCNiFAST16 __PRIPTR_PREFIX "i"
   --  unsupported macro: SCNiFAST32 __PRIPTR_PREFIX "i"
   --  unsupported macro: SCNiFAST64 __PRI64_PREFIX "i"
   --  unsupported macro: SCNu8 "hhu"
   --  unsupported macro: SCNu16 "hu"
   --  unsupported macro: SCNu32 "u"
   --  unsupported macro: SCNu64 __PRI64_PREFIX "u"
   --  unsupported macro: SCNuLEAST8 "hhu"
   --  unsupported macro: SCNuLEAST16 "hu"
   --  unsupported macro: SCNuLEAST32 "u"
   --  unsupported macro: SCNuLEAST64 __PRI64_PREFIX "u"
   --  unsupported macro: SCNuFAST8 "hhu"
   --  unsupported macro: SCNuFAST16 __PRIPTR_PREFIX "u"
   --  unsupported macro: SCNuFAST32 __PRIPTR_PREFIX "u"
   --  unsupported macro: SCNuFAST64 __PRI64_PREFIX "u"
   --  unsupported macro: SCNo8 "hho"
   --  unsupported macro: SCNo16 "ho"
   --  unsupported macro: SCNo32 "o"
   --  unsupported macro: SCNo64 __PRI64_PREFIX "o"
   --  unsupported macro: SCNoLEAST8 "hho"
   --  unsupported macro: SCNoLEAST16 "ho"
   --  unsupported macro: SCNoLEAST32 "o"
   --  unsupported macro: SCNoLEAST64 __PRI64_PREFIX "o"
   --  unsupported macro: SCNoFAST8 "hho"
   --  unsupported macro: SCNoFAST16 __PRIPTR_PREFIX "o"
   --  unsupported macro: SCNoFAST32 __PRIPTR_PREFIX "o"
   --  unsupported macro: SCNoFAST64 __PRI64_PREFIX "o"
   --  unsupported macro: SCNx8 "hhx"
   --  unsupported macro: SCNx16 "hx"
   --  unsupported macro: SCNx32 "x"
   --  unsupported macro: SCNx64 __PRI64_PREFIX "x"
   --  unsupported macro: SCNxLEAST8 "hhx"
   --  unsupported macro: SCNxLEAST16 "hx"
   --  unsupported macro: SCNxLEAST32 "x"
   --  unsupported macro: SCNxLEAST64 __PRI64_PREFIX "x"
   --  unsupported macro: SCNxFAST8 "hhx"
   --  unsupported macro: SCNxFAST16 __PRIPTR_PREFIX "x"
   --  unsupported macro: SCNxFAST32 __PRIPTR_PREFIX "x"
   --  unsupported macro: SCNxFAST64 __PRI64_PREFIX "x"
   --  unsupported macro: SCNdMAX __PRI64_PREFIX "d"
   --  unsupported macro: SCNiMAX __PRI64_PREFIX "i"
   --  unsupported macro: SCNoMAX __PRI64_PREFIX "o"
   --  unsupported macro: SCNuMAX __PRI64_PREFIX "u"
   --  unsupported macro: SCNxMAX __PRI64_PREFIX "x"
   --  unsupported macro: SCNdPTR __PRIPTR_PREFIX "d"
   --  unsupported macro: SCNiPTR __PRIPTR_PREFIX "i"
   --  unsupported macro: SCNoPTR __PRIPTR_PREFIX "o"
   --  unsupported macro: SCNuPTR __PRIPTR_PREFIX "u"
   --  unsupported macro: SCNxPTR __PRIPTR_PREFIX "x"
   type imaxdiv_t is record
      quot : aliased long;  -- /usr/include/inttypes.h:273
      c_rem : aliased long;  -- /usr/include/inttypes.h:274
   end record;
   pragma Convention (C_Pass_By_Copy, imaxdiv_t);  -- /usr/include/inttypes.h:275

   --  skipped anonymous struct anon_0

   function imaxabs (uu_n : Libc.Stdint.intmax_t) return Libc.Stdint.intmax_t;  -- /usr/include/inttypes.h:290
   pragma Import (C, imaxabs, "imaxabs");

   function imaxdiv (uu_numer : Libc.Stdint.intmax_t; uu_denom : Libc.Stdint.intmax_t) return imaxdiv_t;  -- /usr/include/inttypes.h:293
   pragma Import (C, imaxdiv, "imaxdiv");

   function strtoimax
     (uu_nptr : Interfaces.C.Strings.chars_ptr;
      uu_endptr : System.Address;
      uu_base : int) return Libc.Stdint.intmax_t;  -- /usr/include/inttypes.h:297
   pragma Import (C, strtoimax, "strtoimax");

   function strtoumax
     (uu_nptr : Interfaces.C.Strings.chars_ptr;
      uu_endptr : System.Address;
      uu_base : int) return Libc.Stdint.uintmax_t;  -- /usr/include/inttypes.h:301
   pragma Import (C, strtoumax, "strtoumax");

   function wcstoimax
     (uu_nptr : access wchar_t;
      uu_endptr : System.Address;
      uu_base : int) return Libc.Stdint.intmax_t;  -- /usr/include/inttypes.h:305
   pragma Import (C, wcstoimax, "wcstoimax");

   function wcstoumax
     (uu_nptr : access wchar_t;
      uu_endptr : System.Address;
      uu_base : int) return Libc.Stdint.uintmax_t;  -- /usr/include/inttypes.h:310
   pragma Import (C, wcstoumax, "wcstoumax");

end Libc.Inttypes;
