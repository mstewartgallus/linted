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
 *    isalnum                 IsCharAlphaNumeric
 *    isalpha                 IsCharAlpha, GetStringTypeW (Unicode)
 *  __isascii                 none
 *    iscntrl                 none, GetStringTypeW (Unicode)
 *  __iscsym                  none
 *  __iscsymf                 none
 *    isdigit                 none, GetStringTypeW (Unicode)
 *    isgraph                 none
 *    islower                 IsCharLower, GetStringTypeW (Unicode)
 *    isprint                 none
 *    ispunct                 none, GetStringTypeW (Unicode)
 *    isspace                 none, GetStringTypeW (Unicode)
 *    isupper                 IsCharUpper, GetStringTypeW (Unicode)
 *    isxdigit                none, GetStringTypeW (Unicode)
 *  __toascii                 none
 *    tolower                 CharLower
 *   _tolower                 none
 *    toupper                 CharUpper
 *   _toupper                 none
 *  _chdir                    SetCurrentDirectory
 *  _chdrive                  SetCurrentDirectory
 *  _getcwd                   GetCurrentDirectory
 *  _getdrive                 GetCurrentDirectory
 *  _mkdir                    CreateDirectory
 *  _rmdir                    RemoveDirectory
 *  _searchenv                SearchPath
 *  _access                   none
 *  _chmod                    SetFileAttributes
 *  _chsize                   SetEndOfFile
 *  _filelength               GetFileSize
 *  _fstat                    See Note 5
 *  _fullpath                 GetFullPathName
 *  _get_osfhandle            none
 *  _isatty                   GetFileType
 *  _locking                  LockFileEx
 *  _makepath                 none
 *  _mktemp                   GetTempFileName
 *  _open_osfhandle           none
 *   remove                   DeleteFile
 *   rename                   MoveFile
 *  _setmode                  none
 *  _splitpath                none
 *  _stat                     none
 *  _umask                    none
 *  _unlink                   DeleteFile
 *  _displaycursor*           SetConsoleCursorInfo
 *  _gettextcolor*            GetConsoleScreenBufferInfo
 *  _gettextcursor*           GetConsoleCursorInfo
 *  _gettextposition*         GetConsoleScreenBufferInfo
 *  _gettextwindow*           GetConsoleWindowInfo
 *  _outtext*                 WriteConsole
 *  _scrolltextwindow*        ScrollConsoleScreenBuffer
 *  _settextcolor*            SetConsoleTextAttribute
 *  _settextcursor*           SetConsoleCursorInfo
 *  _settextposition*         SetConsoleCursorPosition
 *  _settextwindow*           SetConsoleWindowInfo
 *  _wrapon*                  SetConsoleMode
 *   clearerr                 none
 *   fclose                   CloseHandle
 *  _fcloseall                none
 *  _fdopen                   none
 *   feof                     none
 *   ferror                   none
 *   fflush                   FlushFileBuffers
 *   fgetc                    none
 *  _fgetchar                 none
 *   fgetpos                  none
 *   fgets                    none
 *  _fileno                   none
 *  _flushall                 none
 *   fopen                    CreateFile
 *   fprintf                  none
 *   fputc                    none
 *  _fputchar                 none
 *   fputs                    none
 *   fread                    ReadFile
 *   freopen (std handles)    SetStdHandle
 *   fscanf                   none
 *   fseek                    SetFilePointer
 *   fsetpos                  SetFilePointer
 *  _fsopen                   CreateFile
 *   ftell                    SetFilePointer (check return value)
 *   fwrite                   WriteFile
 *   getc                     none
 *   getchar                  none
 *   gets                     none
 *  _getw                     none
 *   printf                   none
 *   putc                     none
 *   putchar                  none
 *   puts                     none
 *  _putw                     none
 *   rewind                   SetFilePointer
 *  _rmtmp                    none
 *   scanf                    none
 *   setbuf                   none
 *   setvbuf                  none
 *  _snprintf                 none
 *   sprintf                  wsprintf
 *   sscanf                   none
 *  _tempnam                  GetTempFileName
 *   tmpfile                  none
 *   tmpnam                   GetTempFileName
 *   ungetc                   none
 *   vfprintf                 none
 *   vprintf                  none
 *  _vsnprintf                none
 *   vsprintf                 wvsprintf
 *  _close                   _lclose, CloseHandle
 *  _commit                   FlushFileBuffers
 *  _creat                   _lcreat, CreateFile
 *  _dup                      DuplicateHandle
 *  _dup2                     none
 *  _eof                      none
 *  _lseek                   _llseek, SetFilePointer
 *  _open                    _lopen, CreateFile
 *  _read                    _lread, ReadFile
 *  _sopen                    CreateFile
 *  _tell                     SetFilePointer (check return value)
 *  _write                   _lread
 *  _cgets                    none
 *  _cprintf                  none
 *  _cputs                    none
 *  _cscanf                   none
 *  _getch                    ReadConsoleInput
 *  _getche                   ReadConsoleInput
 *  _inp                      none
 *  _inpw                     none
 *  _kbhit                    PeekConsoleInput
 *  _outp                     none
 *  _outpw                    none
 *  _putch                    WriteConsoleInput
 *  _ungetch                  none
 *  _alloca                   none
 *  _bfreeseg*                none
 *  _bheapseg*                none
 *   calloc                   GlobalAlloc
 *  _expand                   none
 *   free                     GlobalFree
 *  _freect*                  GlobalMemoryStatus
 *  _halloc*                  GlobalAlloc
 *  _heapadd                  none
 *  _heapchk                  none
 *  _heapmin                  none
 *  _heapset                  none
 *  _heapwalk                 none
 *  _hfree*                   GlobalFree
 *   malloc                   GlobalAlloc
 *  _memavl                   GlobalMemoryStatus
 *  _memmax                   GlobalMemoryStatus
 *  _msize*                   GlobalSize
 *   realloc                  GlobalReAlloc
 *  _set_new_handler          none
 *  _set_hnew_handler*        none
 *  _stackavail*              none
 *   abort                    none
 *   assert                   none
 *   atexit                   none
 *  _cexit                    none
 *  _c_exit                   none
 *  _exec functions           none
 *   exit                     ExitProcess
 *  _exit                     ExitProcess
 *   getenv                   GetEnvironmentVariable
 *  _getpid                   GetCurrentProcessId
 *   longjmp                  none
 *  _onexit                   none
 *   perror                   FormatMessage
 *  _putenv                   SetEnvironmentVariable
 *   raise                    RaiseException
 *   setjmp                   none
 *   signal (ctrl-c only)     SetConsoleCtrlHandler
 *  _spawn functions          CreateProcess
 *   system                   CreateProcess
 *  strcat, wcscat            lstrcat
 *  strchr, wcschr            none
 *  strcmp, wcscmp            lstrcmp
 *  strcpy, wcscpy            lstrcpy
 *  strcspn, wcscspn          none
 * _strdup, _wcsdup           none
 *  strerror                  FormatMessage
 * _strerror                  FormatMessage
 * _stricmp, _wcsicmp         lstrcmpi
 *  strlen, wcslen            lstrlen
 * _strlwr, _wcslwr           CharLower, CharLowerBuffer
 *  strncat, wcsncat          none
 *  strncmp, wcsncmp          none
 *  strncpy, wcsncpy          none
 * _strnicmp, _wcsnicmp       none
 * _strnset, _wcsnset         FillMemory, ZeroMemory
 *  strpbrk, wcspbrk          none
 *  strrchr, wcsrchr          none
 * _strrev, _wcsrev           none
 * _strset, _wcsset           FillMemory, ZeroMemory
 *  strspn, wcsspn            none
 *  strstr, wcsstr            none
 *  strtok, wcstok            none
 * _strupr, _wcsupr           CharUpper, CharUpperBuffer
 * _bdos*                     none
 * _chain_intr*               none
 * _disable*                  none
 * _dos_allocmem*             GlobalAlloc
 * _dos_close*                CloseHandle
 * _dos_commit*               FlushFileBuffers
 * _dos_creat*                CreateFile
 * _dos_creatnew*             CreateFile
 * _dos_findfirst*            FindFirstFile
 * _dos_findnext*             FindNextFile
 * _dos_freemem*              GlobalFree
 * _dos_getdate*              GetSystemTime
 * _dos_getdiskfree*          GetDiskFreeSpace
 * _dos_getdrive*             GetCurrentDirectory
 * _dos_getfileattr*          GetFileAttributes
 * _dos_getftime*             GetFileTime
 * _dos_gettime*              GetSystemTime
 * _dos_getvect*              none
 * _dos_keep*                 none
 * _dos_open*                 OpenFile
 * _dos_read*                 ReadFile
 * _dos_setblock*             GlobalReAlloc
 * _dos_setdate*              SetSystemTime
 * _dos_setdrive*             SetCurrentDirectory
 * _dos_setfileattr*          SetFileAttributes
 * _dos_setftime*             SetFileTime
 * _dos_settime*              SetSystemTime
 * _dos_setvect*              none
 * _dos_write*                WriteFile
 * _dosexterr*                GetLastError
 * _enable*                   none
 * _FP_OFF*                   none
 * _FP_SEG*                   none
 * _harderr*                  See Note 1
 * _hardresume*               See Note 1
 * _hardretn*                 See Note 1
 * _int86*                    none
 * _int86x*                   none
 * _intdos*                   none
 * _intdosx*                  none
 * _segread*                  none
 *  asctime                   See Note 2
 *  clock                     See Note 2
 *  ctime                     See Note 2
 *  difftime                  See Note 2
 *  _ftime                    See Note 2
 *  _getsystime               GetLocalTime
 *  gmtime                    See Note 2
 *  localtime                 See Note 2
 *  mktime                    See Note 2
 *  _strdate                  See Note 2
 *  _strtime                  See Note 2
 *  time                      See Note 2
 *  _tzset                    See Note 2
 *  _utime                    SetFileTime
 *  _vfree*                   See Note 3
 *  _vheapinit*               See Note 3
 *  _vheapterm*               See Note 3
 *  _vload*                   See Note 3
 *  _vlock*                   See Note 3
 *  _vlockcnt*                See Note 3
 *  _vmalloc*                 See Note 3
 *  _vmsize*                  See Note 3
 *  _vrealloc*                See Note 3
 *  _vunlock*                 See Note 3
 *  _beginthread              CreateThread
 *  _cwait                    WaitForSingleObject w/ GetExitCodeProcess
 *  _endthread                ExitThread
 *  _findclose                FindClose
 *  _findfirst                FindFirstFile
 *  _findnext                 FindNextFile
 *  _futime                   SetFileTime
 *  _get_osfhandle            none
 *  _open_osfhandle           none
 *  _pclose                   See Note 4
 *  _pipe                     CreatePipe
 *  _popen                    See Note 4
 *  _popen                     CreatePipe, CreateProcess
 *  _pclose                    WaitForSingleObject, CloseHandle
 *
 * @subsection asmjs-unknown-emscripten Emscripten
 *
 * This build is temporarily out of service until I figure out why
 * Emscripten generates broken code (it seems to generate null
 * pointers out of nowhere) or the Emscripten compiler improves.
 */
