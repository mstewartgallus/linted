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

package Libc.Sys.Types is
   pragma Preelaborate;

   subtype quad_t is long;  -- /usr/include/sys/types.h:37

   type uu_fsid_t_uu_val_array is array (0 .. 1) of aliased int;
   type fsid_t is record
      uu_val : aliased uu_fsid_t_uu_val_array;  -- /usr/include/bits/types.h:134
   end record;
   pragma Convention (C_Pass_By_Copy, fsid_t);  -- /usr/include/bits/types.h:134

   subtype loff_t is long;  -- /usr/include/sys/types.h:44

   subtype ino_t is unsigned_long;  -- /usr/include/sys/types.h:48

   subtype ino64_t is unsigned_long;  -- /usr/include/sys/types.h:55

   subtype dev_t is unsigned_long;  -- /usr/include/sys/types.h:60

   subtype gid_t is unsigned;  -- /usr/include/sys/types.h:65

   subtype mode_t is unsigned;  -- /usr/include/sys/types.h:70

   subtype nlink_t is unsigned_long;  -- /usr/include/sys/types.h:75

   subtype uid_t is unsigned;  -- /usr/include/sys/types.h:80

   subtype off_t is long;  -- /usr/include/sys/types.h:86

   subtype off64_t is long;  -- /usr/include/sys/types.h:93

   subtype pid_t is int;  -- /usr/include/sys/types.h:98

   subtype id_t is unsigned;  -- /usr/include/sys/types.h:104

   subtype ssize_t is long;  -- /usr/include/sys/types.h:109

   subtype daddr_t is int;  -- /usr/include/sys/types.h:115

   subtype caddr_t is Interfaces.C.Strings.chars_ptr;  -- /usr/include/sys/types.h:116

   subtype key_t is int;  -- /usr/include/sys/types.h:122

   subtype useconds_t is unsigned;  -- /usr/include/sys/types.h:136

   subtype suseconds_t is long;  -- /usr/include/sys/types.h:140

   subtype ulong is unsigned_long;  -- /usr/include/sys/types.h:150

   subtype ushort is unsigned_short;  -- /usr/include/sys/types.h:151

   subtype uint is unsigned;  -- /usr/include/sys/types.h:152

   subtype int8_t is signed_char;  -- /usr/include/sys/types.h:194

   subtype int16_t is short;  -- /usr/include/sys/types.h:195

   subtype int32_t is int;  -- /usr/include/sys/types.h:196

   subtype int64_t is long;  -- /usr/include/sys/types.h:197

   subtype u_int8_t is unsigned_char;  -- /usr/include/sys/types.h:200

   subtype u_int16_t is unsigned_short;  -- /usr/include/sys/types.h:201

   subtype u_int32_t is unsigned;  -- /usr/include/sys/types.h:202

   subtype u_int64_t is unsigned_long;  -- /usr/include/sys/types.h:203

   subtype register_t is long;  -- /usr/include/sys/types.h:205

   subtype blksize_t is long;  -- /usr/include/sys/types.h:228

   subtype blkcnt_t is long;  -- /usr/include/sys/types.h:235

   subtype fsblkcnt_t is unsigned_long;  -- /usr/include/sys/types.h:239

   subtype fsfilcnt_t is unsigned_long;  -- /usr/include/sys/types.h:243

   subtype blkcnt64_t is long;  -- /usr/include/sys/types.h:262

   subtype fsblkcnt64_t is unsigned_long;  -- /usr/include/sys/types.h:263

   subtype fsfilcnt64_t is unsigned_long;  -- /usr/include/sys/types.h:264

end Libc.Sys.Types;
