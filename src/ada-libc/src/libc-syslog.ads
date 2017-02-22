with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package Libc.Syslog with
     Spark_Mode => Off is
   pragma Preelaborate;

   LOG_EMERG : constant :=
     0;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:51
   LOG_ALERT : constant :=
     1;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:52
   LOG_CRIT : constant := 2;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:53
   LOG_ERR : constant := 3;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:54
   LOG_WARNING : constant :=
     4;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:55
   LOG_NOTICE : constant :=
     5;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:56
   LOG_INFO : constant := 6;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:57
   LOG_DEBUG : constant :=
     7;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:58

   LOG_PRIMASK : constant :=
     16#07#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:60
   --  arg-macro: function LOG_PRI ((p) and LOG_PRIMASK
   --    return (p) and LOG_PRIMASK;
   --  arg-macro: function LOG_MAKEPRI ((fac) or (pri)
   --    return (fac) or (pri);
   --  unsupported macro: LOG_KERN (0<<3)

   LOG_USER : constant :=
     (2**3);  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:94
   --  unsupported macro: LOG_MAIL (2<<3)
   --  unsupported macro: LOG_DAEMON (3<<3)
   --  unsupported macro: LOG_AUTH (4<<3)
   --  unsupported macro: LOG_SYSLOG (5<<3)
   --  unsupported macro: LOG_LPR (6<<3)
   --  unsupported macro: LOG_NEWS (7<<3)
   --  unsupported macro: LOG_UUCP (8<<3)
   --  unsupported macro: LOG_CRON (9<<3)
   --  unsupported macro: LOG_AUTHPRIV (10<<3)
   --  unsupported macro: LOG_FTP (11<<3)
   --  unsupported macro: LOG_LOCAL0 (16<<3)
   --  unsupported macro: LOG_LOCAL1 (17<<3)
   --  unsupported macro: LOG_LOCAL2 (18<<3)
   --  unsupported macro: LOG_LOCAL3 (19<<3)
   --  unsupported macro: LOG_LOCAL4 (20<<3)
   --  unsupported macro: LOG_LOCAL5 (21<<3)
   --  unsupported macro: LOG_LOCAL6 (22<<3)
   --  unsupported macro: LOG_LOCAL7 (23<<3)

   LOG_NFACILITIES : constant :=
     24;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:116
   LOG_FACMASK : constant :=
     16#03f8#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:117
   --  arg-macro: function LOG_FAC (((p) and LOG_FACMASK) >> 3
   --    return ((p) and LOG_FACMASK) >> 3;
   --  arg-macro: function LOG_MASK (2 ** (pri)
   --    return 2 ** (pri);
   --  arg-macro: function LOG_UPTO ((2 ** ((pri)+1)) - 1
   --    return (2 ** ((pri)+1)) - 1;

   LOG_PID : constant :=
     16#01#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:162
   LOG_CONS : constant :=
     16#02#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:163
   LOG_ODELAY : constant :=
     16#04#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:164
   LOG_NDELAY : constant :=
     16#08#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:165
   LOG_NOWAIT : constant :=
     16#10#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:166
   LOG_PERROR : constant :=
     16#20#;  --  /usr/include/x86_64-linux-gnu/sys/syslog.h:167

   procedure closelog;  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:175
   pragma Import (C, closelog, "closelog");

   procedure openlog
     (ident : char_array;
      option : int;
      facility : int);  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:181
   pragma Import (C, openlog, "openlog");

   function setlogmask
     (mask : int)
     return int;  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:184
   pragma Import (C, setlogmask, "setlogmask");

   procedure syslog (pri : int; fmt : char_array  -- , ...
   );  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:190
   pragma Import (C_Variadic_2, syslog, "syslog");

   procedure vsyslog
     (pri : int;
      fmt : Interfaces.C.Strings.chars_ptr;
      ap : access System
        .Address);  -- /usr/include/x86_64-linux-gnu/sys/syslog.h:200
   pragma Import (C, vsyslog, "vsyslog");

end Libc.Syslog;
