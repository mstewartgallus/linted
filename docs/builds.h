/* Copyright (C) 2014, 2015 Steven Stewart-Gallus
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty provided
 * the copyright notice and this notice are preserved.
 */

/**
 *
 * @file
 *
 * Linted -- Builds
 *
 * A list of types of final build products.
 *
 * @section supported-builds Supported Builds
 *
 * @subsection x86_64-linux-gnu x86_64 Linux GNU with GCC and GNU ld (GNU Binutils)
 *
 * This is the main build.  Other builds may work but are only worked
 * on for the purpose of making the code more modular and better.
 *
 * @subsubsection buildtime Build Time Dependencies
 *
 * - The GNU Compiler Collection
 *   - Main Site - https://www.gnu.org/software/gcc/
 *   - Bug Mailing List - http://gcc.gnu.org/ml/gcc-bugs/
 *   - Bug Database - http://gcc.gnu.org/bugzilla/
 * - The GNU Binary Utilities
 *   - Main Site - https://www.gnu.org/software/binutils/
 *   - Bug Mailing List - <a href="mailto://bug-binutils@gnu.org">&lt;bug-binutils@gnu.org&gt;</a>
 * - Autoconf
 *   - Main Site - https://www.gnu.org/software/autoconf
 *   - Bug Mailing List - <a href="mailto://bug-autoconf@gnu.org">&lt;bug-autoconf@gnu.org&gt;</a>
 * - Automake
 *   - Main Site - https://www.gnu.org/software/automake
 *   - Bug Mailing List - <a href="mailto://bug-automake@gnu.org">&lt;bug-automake@gnu.org&gt;</a>
 * - GNU Make
 *   - Main Site - https://www.gnu.org/software/make
 *   - Bug Mailing List - <a href="mailto://bug-make@gnu.org">&lt;bug-make@gnu.org&gt;</a>
 *
 * @subsection i686-linux-gnu i686 Linux GNU with GCC and GNU ld (GNU Binutils)
 *
 * This is basically the same as for x86_64 but 32 bit.
 *
 * @section unsupported Unsupported Builds
 *
 * This is not a supported final build product.  This is only worked
 * on for a challenge and for making the code more modular and better.
 * In the future it may be supported.
 *
 * - `aarch64-linux-gnu`
 * - `arm-linux-androideabi`
 * - `arm-linux-gnueabi`
 * - `i686-linux-gnu`
 * - `i686-w64-mingw32`
 * - `x86_64-linux-dietlibc`
 * - `x86_64-linux-gnu`
 * - `x86_64-linux-musl`
 * - `x86_64-w64-mingw32`
 *
 * @subsection x86_64-w64-mingw32 x86_64 MinGW-w64
 *
 * Options that don't work well with Windows NT builds:
 * - `-gplit-dwarf`
 * - `-flto`
 * - `-fstack-protector`
 *
 * Functions in the CRT that can be replaced with Win32 functions see
 * https://support.microsoft.com/en-us/kb/99456
 *
 * <table>
 * <tr><td> `isalnum` </td><td> `IsCharAlphaNumeric`</td></tr>
 * <tr><td> `isalpha` </td><td> `IsCharAlpha`, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `__isascii` </td><td> none</td></tr>
 * <tr><td> `iscntrl` </td><td> none, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `__iscsym` </td><td> none</td></tr>
 * <tr><td> `__iscsymf` </td><td> none</td></tr>
 * <tr><td> `isdigit` </td><td> none, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `isgraph` </td><td> none</td></tr>
 * <tr><td> `islower` </td><td> `IsCharLower`, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `isprint` </td><td> none</td></tr>
 * <tr><td> `ispunct` </td><td> none, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `isspace` </td><td> none, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `isupper` </td><td> `IsCharUpper`, `GetStringTypeW (Unicode)`</td></tr>
 * <tr><td> `isxdigit` </td><td> none, `GetStringTypeW (Unicode)`
 * <tr><td> `__toascii` </td><td> none</td></tr>
 * <tr><td> `tolower` </td><td> `CharLower`</td></tr>
 * <tr><td> `_tolower` </td><td> none</td></tr>
 * <tr><td> `toupper` </td><td> `CharUpper`</td></tr>
 * <tr><td> `_toupper` </td><td> none</td></tr>
 * <tr><td> `_chdir` </td><td> `SetCurrentDirectory`</td></tr>
 * <tr><td> `_chdrive` </td><td> `SetCurrentDirectory`</td></tr>
 * <tr><td> `_getcwd` </td><td> `GetCurrentDirectory`</td></tr>
 * <tr><td> `_getdrive` </td><td> GetCurrentDirectory</td></tr>
 * <tr><td> `_mkdir` </td><td> `CreateDirectory`</td></tr>
 * <tr><td> `_rmdir` </td><td> RemoveDirectory</td></tr>
 * <tr><td> `_searchenv` </td><td> `SearchPath`</td></tr>
 * <tr><td> `_access` </td><td> none</td></tr>
 * <tr><td> `_chmod` </td><td> `SetFileAttributes`</td></tr>
 * <tr><td> `_chsize` </td><td> SetEndOfFile</td></tr>
 * <tr><td> `_filelength` </td><td> `GetFileSize`</td></tr>
 * <tr><td> `_fstat` </td><td> See Note 5</td></tr>
 * <tr><td> `_fullpath` </td><td> `GetFullPathName`</td></tr>
 * <tr><td> `_get_osfhandle` </td><td> none</td></tr>
 * <tr><td> `_isatty` </td><td> `GetFileType`</td></tr>
 * <tr><td> `_locking` </td><td> `LockFileEx`</td></tr>
 * <tr><td> `_makepath` </td><td> none</td></tr>
 * <tr><td> `_mktemp` </td><td> `GetTempFileName`</td></tr>
 * <tr><td> `_open_osfhandle` </td><td> none</td></tr>
 * <tr><td> `remove` </td><td> `DeleteFile`</td></tr>
 * <tr><td> `rename` </td><td> `MoveFile`</td></tr>
 * <tr><td> `_setmode` </td><td> none</td></tr>
 * <tr><td> `_splitpath` </td><td> none</td></tr>
 * <tr><td> `_stat` </td><td> none</td></tr>
 * <tr><td> `_umask` </td><td> none</td></tr>
 * <tr><td> `_unlink` </td><td> `DeleteFile`</td></tr>
 * <tr><td> `_displaycursor` </td><td> `SetConsoleCursorInfo`</td></tr>
 * <tr><td> `_gettextcolor` </td><td> `GetConsoleScreenBufferInfo`</td></tr>
 * <tr><td> `_gettextcursor` </td><td> `GetConsoleCursorInfo`</td></tr>
 * <tr><td> `_gettextposition` </td><td> `GetConsoleScreenBufferInfo`</td></tr>
 * <tr><td> `_gettextwindow` </td><td> `GetConsoleWindowInfo`</td></tr>
 * <tr><td> `_outtext` </td><td> `WriteConsole`</td></tr>
 * <tr><td> `_scrolltextwindow` </td><td> `ScrollConsoleScreenBuffer`</td></tr>
 * <tr><td> `_settextcolor` </td><td> `SetConsoleTextAttribute`</td></tr>
 * <tr><td> `_settextcursor` </td><td> `SetConsoleCursorInfo`</td></tr>
 * <tr><td> `_settextposition` </td><td> `SetConsoleCursorPosition`</td></tr>
 * <tr><td> `_settextwindow` </td><td> `SetConsoleWindowInfo`</td></tr>
 * <tr><td> `_wrapon` </td><td> `SetConsoleMode`</td></tr>
 * <tr><td> `clearerr` </td><td> none</td></tr>
 * <tr><td> `fclose` </td><td> `CloseHandle`</td></tr>
 * <tr><td> `_fcloseall` </td><td> none</td></tr>
 * <tr><td> `_fdopen` </td><td> none</td></tr>
 * <tr><td> `feof` </td><td> none</td></tr>
 * <tr><td> `ferror` </td><td> none</td></tr>
 * <tr><td> `fflush` </td><td> `FlushFileBuffers`</td></tr>
 * <tr><td> `fgetc` </td><td> none</td></tr>
 * <tr><td> `_fgetchar` </td><td> none</td></tr>
 * <tr><td> `fgetpos` </td><td> none</td></tr>
 * <tr><td> `fgets` </td><td> none</td></tr>
 * <tr><td> `_fileno` </td><td> none</td></tr>
 * <tr><td> `_flushall` </td><td> none</td></tr>
 * <tr><td> `fopen` </td><td> `CreateFile`</td></tr>
 * <tr><td> `fprintf` </td><td> none</td></tr>
 * <tr><td> `fputc` </td><td> none</td></tr>
 * <tr><td> `_fputchar` </td><td> none</td></tr>
 * <tr><td> `fputs` </td><td> none</td></tr>
 * <tr><td> `fread` </td><td> `ReadFile`</td></tr>
 * <tr><td> `freopen` (std handles) </td><td> `SetStdHandle`</td></tr>
 * <tr><td> `fscanf` </td><td> none</td></tr>
 * <tr><td> `fseek` </td><td> `SetFilePointer`</td></tr>
 * <tr><td> `fsetpos` </td><td> `SetFilePointer`</td></tr>
 * <tr><td> `_fsopen` </td><td> `CreateFile`</td></tr>
 * <tr><td> `ftell` </td><td> `SetFilePointer` (check return value)</td></tr>
 * <tr><td> `fwrite` </td><td> `WriteFile`</td></tr>
 * <tr><td> `getc` </td><td> none</td></tr>
 * <tr><td> `getchar` </td><td> none</td></tr>
 * <tr><td> `gets` </td><td> none</td></tr>
 * <tr><td> `_getw` </td><td> none</td></tr>
 * <tr><td> `printf` </td><td> none</td></tr>
 * <tr><td> `putc` </td><td> none</td></tr>
 * <tr><td> `putchar` </td><td> none</td></tr>
 * <tr><td> `puts` </td><td> none</td></tr>
 * <tr><td> `_putw` </td><td> none</td></tr>
 * <tr><td> `rewind` </td><td> `SetFilePointer`</td></tr>
 * <tr><td> `_rmtmp` </td><td> none</td></tr>
 * <tr><td> `scanf` </td><td> none</td></tr>
 * <tr><td> `setbuf` </td><td> none</td></tr>
 * <tr><td> `setvbuf` </td><td> none</td></tr>
 * <tr><td> `_snprintf` </td><td> none</td></tr>
 * <tr><td> `sprintf` </td><td> `wsprintf`</td></tr>
 * <tr><td> `sscanf` </td><td> none</td></tr>
 * <tr><td> `_tempnam` </td><td> `GetTempFileName`</td></tr>
 * <tr><td> `tmpfile` </td><td> none</td></tr>
 * <tr><td> `tmpnam` </td><td> `GetTempFileName`</td></tr>
 * <tr><td> `ungetc` </td><td> none</td></tr>
 * <tr><td> `vfprintf` </td><td> none</td></tr>
 * <tr><td> `vprintf` </td><td> none</td></tr>
 * <tr><td> `_vsnprintf` </td><td> none</td></tr>
 * <tr><td> `vsprintf` </td><td> `wvsprintf`</td></tr>
 * <tr><td> `_close` </td><td> `_lclose`, `CloseHandle`</td></tr>
 * <tr><td> `_commit` </td><td> `FlushFileBuffers`</td></tr>
 * <tr><td> `_creat` </td><td> `_lcreat`, `CreateFile`</td></tr>
 * <tr><td> `_dup` </td><td> `DuplicateHandle`</td></tr>
 * <tr><td> `_dup2` </td><td> none</td></tr>
 * <tr><td> `_eof` </td><td> none</td></tr>
 * <tr><td> `_lseek` </td><td> `_llseek`, `SetFilePointer`</td></tr>
 * <tr><td> `_open` </td><td> `_lopen`, `CreateFile`</td></tr>
 * <tr><td> `_read` </td><td> `_lread`, `ReadFile`</td></tr>
 * <tr><td> `_sopen` </td><td> `CreateFile`</td></tr>
 * <tr><td> `_tell` </td><td> `SetFilePointer (check return value)`</td></tr>
 * <tr><td> `_write` </td><td> `_lread`</td></tr>
 * <tr><td> `_cgets` </td><td> none</td></tr>
 * <tr><td> `_cprintf` </td><td> none</td></tr>
 * <tr><td> `_cputs` </td><td> none</td></tr>
 * <tr><td> `_cscanf` </td><td> none</td></tr>
 * <tr><td> `_getch` </td><td> `ReadConsoleInput`</td></tr>
 * <tr><td> `_getche` </td><td> `ReadConsoleInput`</td></tr>
 * <tr><td> `_inp` </td><td> none</td></tr>
 * <tr><td> `_inpw` </td><td> none</td></tr>
 * <tr><td> `_kbhit` </td><td> `PeekConsoleInput`</td></tr>
 * <tr><td> `_outp` </td><td> none</td></tr>
 * <tr><td> `_outpw` </td><td> none</td></tr>
 * <tr><td> `_putch` </td><td> `WriteConsoleInput`</td></tr>
 * <tr><td> `_ungetch` </td><td> none</td></tr>
 * <tr><td> `_alloca` </td><td> none</td></tr>
 * <tr><td> `_bfreeseg` </td><td> none</td></tr>
 * <tr><td> `_bheapseg` </td><td> none</td></tr>
 * <tr><td> `calloc` </td><td> `GlobalAlloc`</td></tr>
 * <tr><td> `_expand` </td><td> none</td></tr>
 * <tr><td> `free` </td><td> `GlobalFree`</td></tr>
 * <tr><td> `_freect` </td><td> `GlobalMemoryStatus`</td></tr>
 * <tr><td> `_halloc` </td><td> `GlobalAlloc`</td></tr>
 * <tr><td> `_heapadd` </td><td> none</td></tr>
 * <tr><td> `_heapchk` </td><td> none</td></tr>
 * <tr><td> `_heapmin` </td><td> none</td></tr>
 * <tr><td> `_heapset` </td><td> none</td></tr>
 * <tr><td> `_heapwalk` </td><td> none</td></tr>
 * <tr><td> `_hfree` </td><td> `GlobalFree`</td></tr>
 * <tr><td> `malloc` </td><td> GlobalAlloc</td></tr>
 * <tr><td> `_memavl` </td><td> `GlobalMemoryStatus`</td></tr>
 * <tr><td> `_memmax` </td><td> `GlobalMemoryStatus`</td></tr>
 * <tr><td> `_msize` </td><td> `GlobalSize`</td></tr>
 * <tr><td> `realloc` </td><td> `GlobalReAlloc`</td></tr>
 * <tr><td> `_set_new_handler` </td><td> none</td></tr>
 * <tr><td> `_set_hnew_handler` </td><td> none</td></tr>
 * <tr><td> `_stackavail` </td><td> none</td></tr>
 * <tr><td> `abort` </td><td> none</td></tr>
 * <tr><td> `assert` </td><td> none</td></tr>
 * <tr><td> `atexit` </td><td> none</td></tr>
 * <tr><td> `_cexit` </td><td> none</td></tr>
 * <tr><td> `_c_exit` </td><td> none</td></tr>
 * <tr><td> `_exec` functions </td><td> none</td></tr>
 * <tr><td> `exit` </td><td> `ExitProcess`</td></tr>
 * <tr><td> `_exit` </td><td> `ExitProcess`</td></tr>
 * <tr><td> `getenv` </td><td> `GetEnvironmentVariable`</td></tr>
 * <tr><td> `_getpid` </td><td> `GetCurrentProcessId`</td></tr>
 * <tr><td> `longjmp` </td><td> none</td></tr>
 * <tr><td> `_onexit` </td><td> none</td></tr>
 * <tr><td> `perror` </td><td> `FormatMessage`</td></tr>
 * <tr><td> `_putenv` </td><td> `SetEnvironmentVariable`</td></tr>
 * <tr><td> `raise` </td><td> `RaiseException`</td></tr>
 * <tr><td> `setjmp` </td><td> none</td></tr>
 * <tr><td> `signal` (ctrl-c only) </td><td> `SetConsoleCtrlHandler`</td></tr>
 * <tr><td> `_spawn` functions </td><td> `CreateProcess`</td></tr>
 * <tr><td> `system` </td><td> `CreateProcess`</td></tr>
 * <tr><td> `strcat`, `wcscat` </td><td> `lstrcat`</td></tr>
 * <tr><td> `strchr`, `wcschr` </td><td> none</td></tr>
 * <tr><td> `strcmp`, `wcscmp` </td><td> `lstrcmp`</td></tr>
 * <tr><td> `strcpy`, `wcscpy` </td><td> `lstrcpy`</td></tr>
 * <tr><td> `strcspn`, `wcscspn` </td><td> none</td></tr>
 * <tr><td> `_strdup`, `_wcsdup` </td><td> none</td></tr>
 * <tr><td> `strerror` </td><td> `FormatMessage`</td></tr>
 * <tr><td> `_strerror` </td><td> `FormatMessage`</td></tr>
 * <tr><td> `_stricmp`, `_wcsicmp` </td><td> `lstrcmpi`</td></tr>
 * <tr><td> `strlen`, `wcslen` </td><td> `lstrlen`</td></tr>
 * <tr><td> `_strlwr`, `_wcslwr` </td><td> `CharLower`, `CharLowerBuffer`</td></tr>
 * <tr><td> `strncat`, `wcsncat` </td><td> none</td></tr>
 * <tr><td> `strncmp`, `wcsncmp` </td><td> none</td></tr>
 * <tr><td> `strncpy`, `wcsncpy` </td><td> none</td></tr>
 * <tr><td> `_strnicmp`, `_wcsnicmp` </td><td> none</td></tr>
 * <tr><td> `_strnset`, `_wcsnset` </td><td> `FillMemory`, `ZeroMemory`</td></tr>
 * <tr><td> `strpbrk`, `wcspbrk` </td><td> none</td></tr>
 * <tr><td> `strrchr`, `wcsrchr` </td><td> none</td></tr>
 * <tr><td> `_strrev`, `_wcsrev` </td><td> none</td></tr>
 * <tr><td> `_strset`, `_wcsset` </td><td> `FillMemory, ZeroMemory`</td></tr>
 * <tr><td> `strspn`, `wcsspn` </td><td> none</td></tr>
 * <tr><td> `strstr`, `wcsstr` </td><td> none</td></tr>
 * <tr><td> `strtok`, `wcstok` </td><td> none</td></tr>
 * <tr><td> `_strupr`, `_wcsupr` </td><td> `CharUpper`, `CharUpperBuffer`</td></tr>
 * <tr><td> `_bdos` </td><td> none</td></tr>
 * <tr><td> `_chain_intr` </td><td> none</td></tr>
 * <tr><td> `_disable` </td><td> none</td></tr>
 * <tr><td> `_dos_allocmem` </td><td> `GlobalAlloc`</td></tr>
 * <tr><td> `_dos_close` </td><td> `CloseHandle`</td></tr>
 * <tr><td> `_dos_commit` </td><td> `FlushFileBuffers`</td></tr>
 * <tr><td> `_dos_creat` </td><td> `CreateFile`</td></tr>
 * <tr><td> `_dos_creatnew` </td><td> `CreateFile`</td></tr>
 * <tr><td> `_dos_findfirst` </td><td> `FindFirstFile`</td></tr>
 * <tr><td> `_dos_findnext` </td><td> `FindNextFile`</td></tr>
 * <tr><td> `_dos_freemem` </td><td> `GlobalFree`</td></tr>
 * <tr><td> `_dos_getdate` </td><td> `GetSystemTime`</td></tr>
 * <tr><td> `_dos_getdiskfree` </td><td> `GetDiskFreeSpace`</td></tr>
 * <tr><td> `_dos_getdrive` </td><td> `GetCurrentDirectory`</td></tr>
 * <tr><td> `_dos_getfileattr` </td><td> `GetFileAttributes`</td></tr>
 * <tr><td> `_dos_getftime` </td><td> `GetFileTime`</td></tr>
 * <tr><td> `_dos_gettime` </td><td> `GetSystemTime`</td></tr>
 * <tr><td> `_dos_getvect` </td><td> none</td></tr>
 * <tr><td> `_dos_keep` </td><td> none</td></tr>
 * <tr><td> `_dos_open` </td><td> `OpenFile`</td></tr>
 * <tr><td> `_dos_read` </td><td> `ReadFile`</td></tr>
 * <tr><td> `_dos_setblock` </td><td> `GlobalReAlloc`</td></tr>
 * <tr><td> `_dos_setdate` </td><td> `SetSystemTime`</td></tr>
 * <tr><td> `_dos_setdrive` </td><td> `SetCurrentDirectory`</td></tr>
 * <tr><td> `_dos_setfileattr` </td><td> `SetFileAttributes`</td></tr>
 * <tr><td> `_dos_setftime` </td><td> `SetFileTime`</td></tr>
 * <tr><td> `_dos_settime` </td><td> `SetSystemTime`</td></tr>
 * <tr><td> `_dos_setvect` </td><td> none</td></tr>
 * <tr><td> `_dos_write` </td><td> `WriteFile`</td></tr>
 * <tr><td> `_dosexterr` </td><td> `GetLastError`</td></tr>
 * <tr><td> `_enable` </td><td> none</td></tr>
 * <tr><td> `_FP_OFF` </td><td> none</td></tr>
 * <tr><td> `_FP_SEG` </td><td> none</td></tr>
 * <tr><td> `_harderr` </td><td> See Note 1</td></tr>
 * <tr><td> `_hardresume` </td><td> See Note 1</td></tr>
 * <tr><td> `_hardretn` </td><td> See Note 1</td></tr>
 * <tr><td> `_int86` </td><td> none</td></tr>
 * <tr><td> `_int86x` </td><td> none</td></tr>
 * <tr><td> `_intdos` </td><td> none</td></tr>
 * <tr><td> `_intdosx` </td><td> none</td></tr>
 * <tr><td> `_segread` </td><td> none</td></tr>
 * <tr><td> `asctime` </td><td> See Note 2</td></tr>
 * <tr><td> `clock` </td><td> See Note 2</td></tr>
 * <tr><td> `ctime` </td><td> See Note 2</td></tr>
 * <tr><td> `difftime` </td><td> See Note 2</td></tr>
 * <tr><td> `_ftime` </td><td> See Note 2</td></tr>
 * <tr><td> `_getsystime` </td><td> `GetLocalTime`</td></tr>
 * <tr><td> `gmtime` </td><td> See Note 2</td></tr>
 * <tr><td> `localtime` </td><td> See Note 2</td></tr>
 * <tr><td> `mktime` </td><td> See Note 2</td></tr>
 * <tr><td> `_strdate` </td><td> See Note 2</td></tr>
 * <tr><td> `_strtime` </td><td> See Note 2</td></tr>
 * <tr><td> `time` </td><td> See Note 2</td></tr>
 * <tr><td> `_tzset` </td><td> See Note 2</td></tr>
 * <tr><td> `_utime` </td><td> `SetFileTime`</td></tr>
 * <tr><td> `_vfree` </td><td> See Note 3</td></tr>
 * <tr><td> `_vheapinit` </td><td> See Note 3</td></tr>
 * <tr><td> `_vheapterm` </td><td> See Note 3</td></tr>
 * <tr><td> `_vload` </td><td> See Note 3</td></tr>
 * <tr><td> `_vlock` </td><td> See Note 3</td></tr>
 * <tr><td> `_vlockcnt` </td><td> See Note 3</td></tr>
 * <tr><td> `_vmalloc` </td><td> See Note 3</td></tr>
 * <tr><td> `_vmsize` </td><td> See Note 3</td></tr>
 * <tr><td> `_vrealloc` </td><td> See Note 3</td></tr>
 * <tr><td> `_vunlock` </td><td> See Note 3</td></tr>
 * <tr><td> `_beginthread` </td><td> `CreateThread`</td></tr>
 * <tr><td> `_cwait` </td><td> `WaitForSingleObject` w/ `GetExitCodeProcess`</td></tr>
 * <tr><td> `_endthread` </td><td> `ExitThread`</td></tr>
 * <tr><td> `_findclose` </td><td> `FindClose`</td></tr>
 * <tr><td> `_findfirst` </td><td> `FindFirstFile`</td></tr>
 * <tr><td> `_findnext` </td><td> `FindNextFile`</td></tr>
 * <tr><td> `_futime` </td><td> `SetFileTime`</td></tr>
 * <tr><td> `_get_osfhandle` </td><td> none</td></tr>
 * <tr><td> `_open_osfhandle` </td><td> none</td></tr>
 * <tr><td> `_pclose` </td><td> See Note 4</td></tr>
 * <tr><td> `_pipe` </td><td> `CreatePipe`</td></tr>
 * <tr><td> `_popen` </td><td> See Note 4</td></tr>
 * <tr><td> `_popen` </td><td> `CreatePipe`, `CreateProcess`</td></tr>
 * <tr><td> `_pclose` </td><td> `WaitForSingleObject`, `CloseHandle`</td></tr>
 * </table>
 *
 * @subsection asmjs-unknown-emscripten Emscripten
 *
 * This build is temporarily out of service until I figure out why
 * Emscripten generates broken code (it seems to generate null
 * pointers out of nowhere) or the Emscripten compiler improves.
 */
