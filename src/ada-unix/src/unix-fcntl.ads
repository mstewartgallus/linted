pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Unix.Sys.Types;

package Unix.Fcntl is
   pragma Preelaborate;

O_ACCMODE : constant := 8#3#;
O_RDONLY : constant := 8#0#;
O_WRONLY : constant := 8#1#;
O_RDWR : constant := 8#2#;
O_CREAT : constant := 8#100#;
O_EXCL : constant := 8#200#;
O_NOCTTY : constant := 8#400#;
O_TRUNC : constant := 8#1000#;
O_APPEND : constant := 8#2000#;
O_NONBLOCK : constant := 8#4000#;
O_NDELAY : constant := O_NONBLOCK;
O_SYNC : constant := 8#4010000#;
O_FSYNC : constant := O_SYNC;
O_ASYNC : constant := 8#20000#;
O_CLOEXEC : constant := 8#2000000#;
F_GETLK : constant := 5;
F_SETLK : constant := 6;
F_SETLKW : constant := 7;
F_GETLK64 : constant := 12;
F_SETLK64 : constant := 13;
F_SETLKW64 : constant := 14;
F_DUPFD : constant := 0;
F_GETFD : constant := 1;
F_SETFD : constant := 2;
F_GETFL : constant := 3;
F_SETFL : constant := 4;
FD_CLOEXEC : constant := 1;
F_RDLCK : constant := 0;
F_WRLCK : constant := 1;
F_UNLCK : constant := 2;
F_EXLCK : constant := 4;
F_SHLCK : constant := 8;

   --  S_IFMT __S_IFMT
   --  S_IFDIR __S_IFDIR
   --  S_IFCHR __S_IFCHR
   --  S_IFBLK __S_IFBLK
   --  S_IFREG __S_IFREG
   --  S_IFIFO __S_IFIFO
   --  S_IFLNK __S_IFLNK
   --  S_IFSOCK __S_IFSOCK
   --  S_ISUID __S_ISUID
   --  S_ISGID __S_ISGID
   --  S_ISVTX __S_ISVTX
   --  S_IRUSR __S_IREAD
   --  S_IWUSR __S_IWRITE
   --  S_IXUSR __S_IEXEC
   --  S_IRWXU (__S_IREAD|__S_IWRITE|__S_IEXEC)
   --  S_IRGRP (S_IRUSR >> 3)
   --  S_IWGRP (S_IWUSR >> 3)
   --  S_IXGRP (S_IXUSR >> 3)
   --  S_IRWXG (S_IRWXU >> 3)
   --  S_IROTH (S_IRGRP >> 3)
   --  S_IWOTH (S_IWGRP >> 3)
   --  S_IXOTH (S_IXGRP >> 3)
   --  S_IRWXO (S_IRWXG >> 3)

   R_OK : constant := 4;
   W_OK : constant := 2;
   X_OK : constant := 1;
   F_OK : constant := 0;

   SEEK_SET : constant := 0;
   SEEK_CUR : constant := 1;
   SEEK_END : constant := 2;

   F_ULOCK : constant := 0;
   F_LOCK : constant := 1;
   F_TLOCK : constant := 2;
   F_TEST : constant := 3;

   function fcntl (fd : int; cmd : int  -- , ...
      ) return int;  -- /usr/include/fcntl.h:137
   pragma Import (C, fcntl, "fcntl");

   function open (file : Interfaces.C.Strings.chars_ptr; oflag : Int; mode : Unix.Sys.Types.mode_t
      ) return int;  -- /usr/include/fcntl.h:146
   pragma Import (C, open, "open");

   function open64 (file : Interfaces.C.Strings.chars_ptr; oflag : Int; mode : Unix.Sys.Types.mode_t
      ) return int;  -- /usr/include/fcntl.h:156
   pragma Import (C, open64, "open64");

   function openat
     (fd : int;
      file : Interfaces.C.Strings.chars_ptr;
      oflag : int  -- , ...
      ) return int;  -- /usr/include/fcntl.h:170
   pragma Import (C, openat, "openat");

   function openat64
     (fd : int;
      file : Interfaces.C.Strings.chars_ptr;
      oflag : int  -- , ...
      ) return int;  -- /usr/include/fcntl.h:181
   pragma Import (C, openat64, "openat64");

   function creat (file : Interfaces.C.Strings.chars_ptr; mode : Unix.Sys.Types.mode_t) return int;  -- /usr/include/fcntl.h:192
   pragma Import (C, creat, "creat");

   function creat64 (file : Interfaces.C.Strings.chars_ptr; mode : Unix.Sys.Types.mode_t) return int;  -- /usr/include/fcntl.h:202
   pragma Import (C, creat64, "creat64");

   function lockf
     (fd : int;
      cmd : int;
      len : Unix.Sys.Types.off_t) return int;  -- /usr/include/fcntl.h:221
   pragma Import (C, lockf, "lockf");

   function lockf64
     (fd : int;
      cmd : int;
      len : Unix.Sys.Types.off64_t) return int;  -- /usr/include/fcntl.h:230
   pragma Import (C, lockf64, "lockf64");

   function posix_fadvise
     (fd : int;
      offset : Unix.Sys.Types.off_t;
      len : Unix.Sys.Types.off_t;
      advise : int) return int;  -- /usr/include/fcntl.h:238
   pragma Import (C, posix_fadvise, "posix_fadvise");

   function posix_fadvise64
     (fd : int;
      offset : Unix.Sys.Types.off64_t;
      len : Unix.Sys.Types.off64_t;
      advise : int) return int;  -- /usr/include/fcntl.h:250
   pragma Import (C, posix_fadvise64, "posix_fadvise64");

   function posix_fallocate
     (fd : int;
      offset : Unix.Sys.Types.off_t;
      len : Unix.Sys.Types.off_t) return int;  -- /usr/include/fcntl.h:260
   pragma Import (C, posix_fallocate, "posix_fallocate");

   function posix_fallocate64
     (fd : int;
      offset : Unix.Sys.Types.off64_t;
      len : Unix.Sys.Types.off64_t) return int;  -- /usr/include/fcntl.h:271
   pragma Import (C, posix_fallocate64, "posix_fallocate64");

end Unix.Fcntl;
