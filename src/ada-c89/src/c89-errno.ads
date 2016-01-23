pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package C89.Errno is
   pragma Pure;

   function Errno return Interfaces.C.int;
   pragma Import (C, Errno, "linted_adarts_c89_errno");

   procedure Errno_Set (Err : Interfaces.C.int);
   pragma Import (C, Errno_Set, "linted_adarts_c89_errno_set");

   EDOM : constant := 33;
   ERANGE : constant := 34;

   --  This is actually a C11 type I think
   -- subtype error_t is int;  -- /usr/include/errno.h:68

   --  TODO: Possibly, include these GNU extensions.

   --  program_invocation_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/errno.h:54
   --  pragma Import (C, program_invocation_name, "program_invocation_name");

   --  program_invocation_short_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/errno.h:54
   --  pragma Import (C, program_invocation_short_name, "program_invocation_short_name");

   --  unsupported macro: EPERM 1
   --  unsupported macro: ENOENT 2
   --  unsupported macro: ESRCH 3
   --  unsupported macro: EINTR 4
   --  unsupported macro: EIO 5
   --  unsupported macro: ENXIO 6
   --  unsupported macro: E2BIG 7
   --  unsupported macro: ENOEXEC 8
   --  unsupported macro: EBADF 9
   --  unsupported macro: ECHILD 10
   --  unsupported macro: EAGAIN 11
   --  unsupported macro: ENOMEM 12
   --  unsupported macro: EACCES 13
   --  unsupported macro: EFAULT 14
   --  unsupported macro: ENOTBLK 15
   --  unsupported macro: EBUSY 16
   --  unsupported macro: EEXIST 17
   --  unsupported macro: EXDEV 18
   --  unsupported macro: ENODEV 19
   --  unsupported macro: ENOTDIR 20
   --  unsupported macro: EISDIR 21
   --  unsupported macro: EINVAL 22
   --  unsupported macro: ENFILE 23
   --  unsupported macro: EMFILE 24
   --  unsupported macro: ENOTTY 25
   --  unsupported macro: ETXTBSY 26
   --  unsupported macro: EFBIG 27
   --  unsupported macro: ENOSPC 28
   --  unsupported macro: ESPIPE 29
   --  unsupported macro: EROFS 30
   --  unsupported macro: EMLINK 31
   --  unsupported macro: EPIPE 32

   --  unsupported macro: EDEADLK 35
   --  unsupported macro: ENAMETOOLONG 36
   --  unsupported macro: ENOLCK 37
   --  unsupported macro: ENOSYS 38
   --  unsupported macro: ENOTEMPTY 39
   --  unsupported macro: ELOOP 40
   --  unsupported macro: EWOULDBLOCK EAGAIN
   --  unsupported macro: ENOMSG 42
   --  unsupported macro: EIDRM 43
   --  unsupported macro: ECHRNG 44
   --  unsupported macro: EL2NSYNC 45
   --  unsupported macro: EL3HLT 46
   --  unsupported macro: EL3RST 47
   --  unsupported macro: ELNRNG 48
   --  unsupported macro: EUNATCH 49
   --  unsupported macro: ENOCSI 50
   --  unsupported macro: EL2HLT 51
   --  unsupported macro: EBADE 52
   --  unsupported macro: EBADR 53
   --  unsupported macro: EXFULL 54
   --  unsupported macro: ENOANO 55
   --  unsupported macro: EBADRQC 56
   --  unsupported macro: EBADSLT 57
   --  unsupported macro: EDEADLOCK EDEADLK
   --  unsupported macro: EBFONT 59
   --  unsupported macro: ENOSTR 60
   --  unsupported macro: ENODATA 61
   --  unsupported macro: ETIME 62
   --  unsupported macro: ENOSR 63
   --  unsupported macro: ENONET 64
   --  unsupported macro: ENOPKG 65
   --  unsupported macro: EREMOTE 66
   --  unsupported macro: ENOLINK 67
   --  unsupported macro: EADV 68
   --  unsupported macro: ESRMNT 69
   --  unsupported macro: ECOMM 70
   --  unsupported macro: EPROTO 71
   --  unsupported macro: EMULTIHOP 72
   --  unsupported macro: EDOTDOT 73
   --  unsupported macro: EBADMSG 74
   --  unsupported macro: EOVERFLOW 75
   --  unsupported macro: ENOTUNIQ 76
   --  unsupported macro: EBADFD 77
   --  unsupported macro: EREMCHG 78
   --  unsupported macro: ELIBACC 79
   --  unsupported macro: ELIBBAD 80
   --  unsupported macro: ELIBSCN 81
   --  unsupported macro: ELIBMAX 82
   --  unsupported macro: ELIBEXEC 83
   --  unsupported macro: EILSEQ 84
   --  unsupported macro: ERESTART 85
   --  unsupported macro: ESTRPIPE 86
   --  unsupported macro: EUSERS 87
   --  unsupported macro: ENOTSOCK 88
   --  unsupported macro: EDESTADDRREQ 89
   --  unsupported macro: EMSGSIZE 90
   --  unsupported macro: EPROTOTYPE 91
   --  unsupported macro: ENOPROTOOPT 92
   --  unsupported macro: EPROTONOSUPPORT 93
   --  unsupported macro: ESOCKTNOSUPPORT 94
   --  unsupported macro: EOPNOTSUPP 95
   --  unsupported macro: EPFNOSUPPORT 96
   --  unsupported macro: EAFNOSUPPORT 97
   --  unsupported macro: EADDRINUSE 98
   --  unsupported macro: EADDRNOTAVAIL 99
   --  unsupported macro: ENETDOWN 100
   --  unsupported macro: ENETUNREACH 101
   --  unsupported macro: ENETRESET 102
   --  unsupported macro: ECONNABORTED 103
   --  unsupported macro: ECONNRESET 104
   --  unsupported macro: ENOBUFS 105
   --  unsupported macro: EISCONN 106
   --  unsupported macro: ENOTCONN 107
   --  unsupported macro: ESHUTDOWN 108
   --  unsupported macro: ETOOMANYREFS 109
   --  unsupported macro: ETIMEDOUT 110
   --  unsupported macro: ECONNREFUSED 111
   --  unsupported macro: EHOSTDOWN 112
   --  unsupported macro: EHOSTUNREACH 113
   --  unsupported macro: EALREADY 114
   --  unsupported macro: EINPROGRESS 115
   --  unsupported macro: ESTALE 116
   --  unsupported macro: EUCLEAN 117
   --  unsupported macro: ENOTNAM 118
   --  unsupported macro: ENAVAIL 119
   --  unsupported macro: EISNAM 120
   --  unsupported macro: EREMOTEIO 121
   --  unsupported macro: EDQUOT 122
   --  unsupported macro: ENOMEDIUM 123
   --  unsupported macro: EMEDIUMTYPE 124
   --  unsupported macro: ECANCELED 125
   --  unsupported macro: ENOKEY 126
   --  unsupported macro: EKEYEXPIRED 127
   --  unsupported macro: EKEYREVOKED 128
   --  unsupported macro: EKEYREJECTED 129
   --  unsupported macro: EOWNERDEAD 130
   --  unsupported macro: ENOTRECOVERABLE 131
   --  unsupported macro: ERFKILL 132
   --  unsupported macro: EHWPOISON 133
end C89.Errno;
