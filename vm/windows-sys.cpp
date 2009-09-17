/* windows-sys.cpp
 * June 2nd, 2007
 *
 * Windows version of system access routines.
 */

#include "scan.h"

#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <tchar.h>
#include <process.h>

#ifdef SCAN_WINDOWS
#  pragma warning (push)
#    pragma warning (disable: 4668 4826)
#    include <windows.h>
#  pragma warning (pop)
#endif

#include <WinNT.h>

#include "shlwapi.h"


#define SECONDS_PER_MINUTE (60)

namespace scan {

  struct sys_thread_context_t
  {
    CONTEXT _context;
  };

  sys_retcode_t rc_to_sys_retcode_t(DWORD rc); // forward decl

  _TCHAR **sys_get_env_vars()
  {
    return environ;
  }

  sys_retcode_t sys_setenv(_TCHAR *varname, _TCHAR *value)
  {
    if (!SetEnvironmentVariable(varname, value))
      return rc_to_sys_retcode_t(GetLastError());

    return SYS_OK;
  }


#if !defined(FILE_ATTRIBUTE_DEVICE)
#   define FILE_ATTRIBUTE_DEVICE 0x0040
#endif

  // !! find-files should be able to take lists of filename specifiers in addition to single specifiers.

  struct sys_dir_t
  {
    HANDLE          _fh;
    bool            _fd_valid;
    WIN32_FIND_DATA _fd;
  };

  sys_retcode_t sys_opendir(const char *path, sys_dir_t **dir)
  {
    /* fix up path to include the universal wildcard. POSIX
     * opendir opens a directory and requires the path to the directory.
     * Win32 FindFirstFile starts searching for files within a directory
     * that match a glob. This makes up for that difference.
     */
    _TCHAR path_buf[SYS_PATH_MAX];

    _tcsncpy(path_buf, path, SYS_PATH_MAX);

    size_t path_len = strlen(path_buf);

    if (path_len && ((path_buf[path_len - 1] == '/')
                     || (path_buf[path_len - 1] == '\\')))
      {
        _tcsncat(path_buf, "*.*", SYS_PATH_MAX - path_len);
      }

    *dir = (sys_dir_t *)safe_malloc(sizeof(sys_dir_t));

    if (*dir == NULL)
      return SYS_ENOMEM;


    (*dir)->_fh = FindFirstFile(path_buf, &((*dir)->_fd));

    if ((*dir)->_fh == INVALID_HANDLE_VALUE)
      {
        DWORD error_code = GetLastError();

        safe_free(*dir);
        *dir = NULL;

        return rc_to_sys_retcode_t(error_code);
      }

    (*dir)->_fd_valid = true;

    return SYS_OK;
  }

  sys_filetype_t filetype_from_attributes(DWORD file_attributes)
  {
    if (file_attributes & FILE_ATTRIBUTE_DIRECTORY)
      return SYS_FT_DIR;
    else if (file_attributes & FILE_ATTRIBUTE_DEVICE)
      return SYS_FT_BLK; // REVISIT: Can we do better than assuming every dev is block?
    else if (file_attributes & FILE_ATTRIBUTE_REPARSE_POINT)
      return SYS_FT_LNK;
    else
      return SYS_FT_REG;
  }

  sys_retcode_t sys_readdir(sys_dir_t *dir, sys_dirent_t *ent, bool *done_p)
  {
    /* If the current find data isn't valid, we're at the end. */
    if (!dir->_fd_valid)
      {
        *done_p = true;
        return SYS_OK;
      }

    /* We have valid find data, marahsll it to the correct format. */
    ent->_type = filetype_from_attributes(dir->_fd.dwFileAttributes);

    _tcsncpy(ent->_name, dir->_fd.cFileName, SYS_NAME_MAX);

    /* Look for the next file, which we'll store away for the next call to
     * sys_readdir. . */
    if (FindNextFile(dir->_fh, &(dir->_fd)))
      {
        *done_p        = false;
        dir->_fd_valid = true;
        return SYS_OK;
      }

    *done_p        = false;
    dir->_fd_valid = false;

    DWORD error_code = GetLastError();

    if (error_code != ERROR_NO_MORE_FILES)
      {
        *done_p = true;
        return rc_to_sys_retcode_t(error_code);
      }

    return SYS_OK;
  }

  sys_retcode_t sys_closedir(sys_dir_t *dir)
  {
    sys_retcode_t retcode = SYS_OK;

    if (!FindClose(dir->_fh))
      retcode = rc_to_sys_retcode_t(GetLastError());

    dir->_fh = NULL;

    safe_free(dir);

    return retcode;
  }

  /* FILETIME units (FT_UNITS) are 100ns units measured from an
   * epoch beginning January 1st, 1601. These are the conversion
   * factors necessary to convert them into time_t units, which
   * are seconds from January 1st, 1970. The offset is itself
   * measured in FILETIME units.
   */
  const u64 FT_UNITS_PER_SECOND = U64(10000000);
  const u64 FT_UNITS_OFS        = U64(116444736000000000);

  static sys_time_t filetime_to_time_t(FILETIME ft)
  {
    u64 ftlinear = make_u64(ft.dwHighDateTime, ft.dwLowDateTime);

    return sys_time_t((ftlinear - FT_UNITS_OFS) / FT_UNITS_PER_SECOND);
  }

  sys_retcode_t sys_stat(const char *path, sys_stat_t *buf)
  {
    WIN32_FIND_DATA fd;

    HANDLE find_handle = FindFirstFile(path, &fd);

    if (find_handle == INVALID_HANDLE_VALUE)
      {
        DWORD error_code = GetLastError();

        return rc_to_sys_retcode_t(error_code);
      }

    FindClose(find_handle);

    buf->_filetype = filetype_from_attributes(fd.dwFileAttributes);

    if (fd.dwFileAttributes & FILE_ATTRIBUTE_TEMPORARY)
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs & SYS_FATTR_TEMPORARY);
    if (fd.dwFileAttributes & FILE_ATTRIBUTE_ARCHIVE)
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs & SYS_FATTR_ARCHIVE);
    if (fd.dwFileAttributes & FILE_ATTRIBUTE_OFFLINE)
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs & SYS_FATTR_OFFLINE);
    if (fd.dwFileAttributes & FILE_ATTRIBUTE_COMPRESSED)
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs & SYS_FATTR_COMPRESSED);
    if (fd.dwFileAttributes & FILE_ATTRIBUTE_ENCRYPTED)
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs & SYS_FATTR_ENCRYPTED);
    if (fd.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN)
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs & SYS_FATTR_HIDDEN);

    // REVISIT: add alternate filename

    // REVISIT: Better way to get mode info?
    buf->_mode    = (fd.dwFileAttributes & FILE_ATTRIBUTE_READONLY) ? 0555 : 0777;

    buf->_size    = make_i64(fd.nFileSizeHigh, fd.nFileSizeLow);
    buf->_atime   = filetime_to_time_t(fd.ftLastAccessTime);
    buf->_mtime   = filetime_to_time_t(fd.ftLastWriteTime);
    buf->_ctime   = filetime_to_time_t(fd.ftCreationTime);


    return SYS_OK;
  }

  sys_retcode_t sys_temporary_filename(_TCHAR *prefix, _TCHAR *buf, size_t buflen)
  {
    _TCHAR pathBuf[MAX_PATH];
    _TCHAR nameBuf[MAX_PATH];

    if (GetTempPath(MAX_PATH, pathBuf) == 0)
      return rc_to_sys_retcode_t(GetLastError());

    if (GetTempFileName(pathBuf, prefix, 0, nameBuf) == 0)
      return rc_to_sys_retcode_t(GetLastError());

    strncpy(buf, nameBuf, buflen);

    return SYS_OK;
  }

  sys_retcode_t sys_delete_file(_TCHAR *filename)
  {
    if (!DeleteFile(filename))
      return rc_to_sys_retcode_t(GetLastError());

    return SYS_OK;
  }


  static __int64 runtime_ticks_per_sec = 0;
  static bool have_highres_timebase = false;

  static flonum_t realtime_offset = 0.0; // timebase offset to epoch
  static flonum_t runtime_offset = 0.0;  // timebase offset to interp start

  static flonum_t sys_timebase_time(void);

  static void sys_init_time()
  {
    LARGE_INTEGER temp;

    // Determine the number of high resolution ticks per second
    if (QueryPerformanceFrequency(&temp) == 0) // 0 => FAIL
      {
        have_highres_timebase = false;
        runtime_ticks_per_sec = 1000;
      }
    else
      {
        have_highres_timebase = true;
        runtime_ticks_per_sec = temp.QuadPart;
      }

    // Get the current time so that we can accurately return a high resolution
    // time relative to the Unix epoch.
    FILETIME ft_runtime;
    GetSystemTimeAsFileTime(&ft_runtime);

    realtime_offset = (flonum_t)filetime_to_time_t(ft_runtime) - sys_timebase_time();

    // Record the current time so that we can get a measure of uptime
    runtime_offset = sys_timebase_time();
  }

  static flonum_t sys_timebase_time(void)
  {
    LARGE_INTEGER temp;
    __int64 counterValue;

    if (!have_highres_timebase)
      return ((flonum_t)GetTickCount() / runtime_ticks_per_sec);

    QueryPerformanceCounter(&temp);
    counterValue = temp.QuadPart;

    return ((flonum_t)counterValue / (flonum_t)runtime_ticks_per_sec);
  }

  flonum_t sys_realtime(void)
  {
    return sys_timebase_time() + realtime_offset;
  }

  flonum_t sys_runtime(void)
  {
    return sys_timebase_time() - runtime_offset;
  }

  flonum_t sys_time_resolution()
  {
    sys_runtime();

    return (flonum_t)runtime_ticks_per_sec;
  }

  flonum_t sys_timezone_offset()
  {
    TIME_ZONE_INFORMATION tz_info;

    DWORD rc = GetTimeZoneInformation(&tz_info);

    LONG bias = tz_info.Bias;

    switch(rc)
      {
      case TIME_ZONE_ID_DAYLIGHT:
        bias += tz_info.DaylightBias;
        break;

      case TIME_ZONE_ID_STANDARD:
        bias += tz_info.StandardBias;
        break;

        // The TIME_ZONE_ID_UNKNOWN case gets no additional bias
      }

    return bias * SECONDS_PER_MINUTE;
  }

  sys_retcode_t sys_gethostname(_TCHAR *buf, size_t len)
  {
    DWORD buf_len = len;

    if (::GetComputerName(buf, &buf_len))
      return SYS_OK;

    return rc_to_sys_retcode_t(GetLastError());
  }


  void sys_get_info(sys_info_t *info)
  {
    info->_machine_bits            = 32;
    info->_eoln                    = SYS_EOLN_CRLF;
    info->_fs_names_case_sensitive = false;
    info->_platform_name           = _T("win32");
  }

  /**************************************************************
   * int debug_printf(_TCHAR *, ...)
   *
   * Debugging print statement. Sends debug messages to the
   * standard debugging output.
   */

  enum { MESSAGE_BUF_SIZE = 256 };

  extern "C" int debug_printf(_TCHAR *format, ...)
  {
    int i;
    va_list args;
    _TCHAR buf[MESSAGE_BUF_SIZE];

    va_start(args, format);
    i = _vsntprintf(buf, MESSAGE_BUF_SIZE, format, args);
    va_end(args);

    OutputDebugString(buf);

    return i;
  }

  static panic_handler_t current_panic_handler = NULL;

  panic_handler_t set_panic_handler(panic_handler_t new_handler)
  {
    panic_handler_t old_handler = current_panic_handler;

    current_panic_handler = new_handler;

    return old_handler;
  }

  void debug_break(); // REVISIT: where does this prototype really go?

  void _panic(const _TCHAR *str, const _TCHAR *filename, long lineno)
  {
    _TCHAR buf[MESSAGE_BUF_SIZE];
    _sntprintf(buf, MESSAGE_BUF_SIZE, "Fatal Error: %s @ (%s:%d)\n", str, filename, lineno);

    fprintf(stderr, "%s", buf);
    fflush(stderr);

    OutputDebugString(buf);

    if (current_panic_handler)
      current_panic_handler();

    assert(false);
  }

  extern "C" char *strchrnul(const char *s, int c)
  {
    char *loc = (char *)s;

    while((*loc != '\0') && ((int)*loc != c))
      loc++;

    return loc;
  }

  void *sys_get_stack_start()
  {
    SYSTEM_INFO sysinfo;
    MEMORY_BASIC_INFORMATION mem_info;

    GetSystemInfo(&sysinfo);

    VirtualQuery(((u8 *)&mem_info) - sysinfo.dwPageSize, &mem_info, sizeof(mem_info));

    return (char *)mem_info.BaseAddress + mem_info.RegionSize;
  }

  void output_debug_string(const _TCHAR *str)
  {
    OutputDebugString(str);
  }

  void debug_break()
  {
#if defined(_MSC_VER)
    __debugbreak();
#elif defined(__GNUC__)
    __asm__ __volatile__ ("int3");
#else
#error debug_break not supported.
#endif
  }

  sys_retcode_t rc_to_sys_retcode_t(DWORD rc)
  {
    _TCHAR msg_buf[STACK_STRBUF_LEN];

    switch(rc) {
    case NO_ERROR:                                     return SYS_OK;
    case ERROR_INVALID_FUNCTION:                       return SYS_ENOSYS;
    case ERROR_FILE_NOT_FOUND:                         return SYS_ENOENT;
    case ERROR_PATH_NOT_FOUND:                         return SYS_ENOENT;
    case ERROR_TOO_MANY_OPEN_FILES:                    return SYS_EMFILE;
    case ERROR_ACCESS_DENIED:                          return SYS_EACCES;
    case ERROR_INVALID_HANDLE:                         return SYS_EBADF;
    case ERROR_NOT_ENOUGH_MEMORY:                      return SYS_ENOMEM;
    case ERROR_INVALID_BLOCK:                          return SYS_EBADF;
    case ERROR_OUTOFMEMORY:                            return SYS_ENOMEM;
    case ERROR_INVALID_DRIVE:                          return SYS_ENODEV;
    case ERROR_CURRENT_DIRECTORY:                      return SYS_EPERM;
    case ERROR_NO_MORE_FILES:                          return SYS_EMFILE;
    case ERROR_WRITE_PROTECT:                          return SYS_EROFS;
    case ERROR_BAD_UNIT:                               return SYS_ENODEV;
    case ERROR_SEEK:                                   return SYS_ESPIPE;
    case ERROR_WRITE_FAULT:                            return SYS_EIO;
    case ERROR_READ_FAULT:                             return SYS_EIO;
    case ERROR_GEN_FAILURE:                            return SYS_EIO;
    case ERROR_HANDLE_DISK_FULL:                       return SYS_ENOSPC;
    case ERROR_DEV_NOT_EXIST:                          return SYS_ENODEV;
    case ERROR_BUFFER_OVERFLOW:                        return SYS_ENOSPC;
    case ERROR_BUSY:                                   return SYS_EBUSY;
    case ERROR_INVALID_EXE_SIGNATURE:                  return SYS_ENOEXEC;
    case ERROR_EXE_MARKED_INVALID:                     return SYS_ENOEXEC;
    case ERROR_BAD_EXE_FORMAT:                         return SYS_ENOEXEC;
    case ERROR_SERIAL_NO_DEVICE:                       return SYS_ENODEV;
    case ERROR_BAD_DEVICE:                             return SYS_ENODEV;
    case ERROR_NETWORK_UNREACHABLE:                    return SYS_ENETUNREACH;
    case ERROR_PROTOCOL_UNREACHABLE:                   return SYS_EPFNOSUPPORT;
    case ERROR_CONNECTION_ABORTED:                     return SYS_ECONNABORTED;

    default:
      debug_printf(_T("Unknown return value from Windows API: 0x%08x"), rc);

      if (::FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
                          NULL,
                          rc,
                          0,
                          (_TCHAR *)msg_buf,
                          STACK_STRBUF_LEN,
                          NULL))
        {
          debug_printf(_T("Additional details: %s"), msg_buf);
        }

      return SYS_EWIERD;
    }
  }


  static SCAN_THREAD_LOCAL sys_thread_t current_thread;
  static SCAN_THREAD_LOCAL uptr max_stack_size;
  static SCAN_THREAD_LOCAL sys_thread_context_t current_thread_context_buffer;

  /* External, thanks to the fact that it's used in the stack limit checking
   * macro. */
  SCAN_THREAD_LOCAL u8 *stack_limit_obj;


  static void setup_current_thread()
  {
    HANDLE thread;

    BOOL success = DuplicateHandle(GetCurrentProcess(),
                                   GetCurrentThread(),
                                   GetCurrentProcess(),
                                   &thread,
                                   NULL,
                                   TRUE,
                                   DUPLICATE_SAME_ACCESS);

    assert(success && "Error finding current thread handle!");

    current_thread = (sys_thread_t)thread;

    sys_set_thread_stack_limit(max_stack_size);
  }

  static bool sys_initialized = false;

  void sys_init()
  {
    assert(!sys_initialized);

    sys_initialized = true;

    max_stack_size = THREAD_DEFAULT_STACK_SIZE;

    sys_init_time();

    setup_current_thread();
  }

  struct sys_thread_args_t
  {
    thread_entry_t entry;
    uptr max_stack_size;
    void *arglist;
  };

  void sys_thread_main(void *arglist)
  {
    thread_entry_t entry    = ((sys_thread_args_t *)arglist)->entry;
    void *actual_arglist    = ((sys_thread_args_t *)arglist)->arglist;

    max_stack_size = ((sys_thread_args_t *)arglist)->max_stack_size;

    safe_free(arglist);

    setup_current_thread();

    entry(actual_arglist);
  }


  sys_thread_t sys_create_thread(thread_entry_t entry, uptr max_stack_size,  void *arglist)
  {
    sys_thread_args_t *args = (sys_thread_args_t *)safe_malloc(sizeof(sys_thread_args_t));

    args->entry          = entry;
    args->arglist        = arglist;
    args->max_stack_size = max_stack_size;

    sys_thread_t thread_handle = _beginthread(sys_thread_main, max_stack_size, args);

    return thread_handle;
  }

  sys_thread_t sys_current_thread()
  {
    return (sys_thread_t)current_thread;
  }

  enum {
    MAX_THREAD_SUSPEND_ATTEMPTS   = 6,   // The maximum number of times to try to suspend a thread
    THREAD_SUSPEND_SLEEP_DURATION = 100 ,// The duration (in ms.) of the sleep between suspend attempts.
  };

  void *sys_set_thread_stack_limit(size_t new_size_limit) // new_size_limit of 0 disables limit checking
  {
    u8 *stack_start = (u8 *)sys_get_stack_start();

    if (new_size_limit > max_stack_size)
      new_size_limit = max_stack_size;

    /* If the size limit is greater than the address, the computation of
     * stack_limit_obj would wrap around the address space, put the limit
     * at the very top of the address space, and therefore immediately trigger
     * a stack limit violation at the next check. This clamp keeps that
     * from happening.
     */
    if (new_size_limit > (uptr)stack_start)
      new_size_limit = 0;

    if (new_size_limit == 0)
      stack_limit_obj = (u8 *)0;
    else
      stack_limit_obj = (u8 *)(stack_start - new_size_limit);

    return stack_limit_obj;
  }

  sys_retcode_t sys_suspend_thread(sys_thread_t thread)
  {
    /* Retry thread suspension if it initially fails, to catch the scenario of a
     * failed suspension due to a thread in system code.
     */
    for(size_t attempts = 0; attempts < MAX_THREAD_SUSPEND_ATTEMPTS; attempts++)
      {
        if (SuspendThread((HANDLE)thread) == 0)
          return SYS_OK;

        Sleep(THREAD_SUSPEND_SLEEP_DURATION);
      }

    return SYS_EWIERD; // TODO: Return something more descriptive?
  }

  sys_retcode_t sys_resume_thread(sys_thread_t thread)
  {
    if (ResumeThread((HANDLE)thread) == 0)
      return SYS_OK;
    else
      return SYS_EWIERD; // TODO: Parse GetLastError?
  }

  struct sys_critical_section_t // REVISIT: Add magic number?
  {
    CRITICAL_SECTION _crit_sec;
  };

  sys_critical_section_t *sys_create_critical_section() // REVISIT: Add section name?
  {
    sys_critical_section_t *crit_sec =
      (sys_critical_section_t *)safe_malloc(sizeof(sys_critical_section_t));

    if (crit_sec == NULL)
      return NULL;

    InitializeCriticalSection(&(crit_sec->_crit_sec));

    return crit_sec;
  }

  void sys_enter_critical_section(sys_critical_section_t *crit_sec)
  {
    assert(crit_sec != NULL);

    EnterCriticalSection(&(crit_sec->_crit_sec));
  }

  void sys_leave_critical_section(sys_critical_section_t *crit_sec)
  {
    assert(crit_sec != NULL);

    LeaveCriticalSection(&(crit_sec->_crit_sec));
  }

  void sys_delete_critical_section(sys_critical_section_t *crit_sec)
  {
    assert(crit_sec != NULL);

    DeleteCriticalSection(&(crit_sec->_crit_sec));

    safe_free(crit_sec);
  }

  void sys_sleep(uintptr_t duration_ms)
  {
    Sleep(duration_ms);
  }


  sys_thread_context_t *sys_get_thread_context(sys_thread_t thread)
  {
    current_thread_context_buffer._context.ContextFlags
      = CONTEXT_SEGMENTS | CONTEXT_INTEGER | CONTEXT_CONTROL;

    if (GetThreadContext((HANDLE)thread, &(current_thread_context_buffer._context)) != 0)
      return NULL;

    return &current_thread_context_buffer;
  }

  void sys_thread_context_get_state_region(sys_thread_context_t *context, void **low, void **high)
  {
    *low  = &(context->_context.SegGs);
    *high = &(context->_context.ExtendedRegisters[0]);
  }

  void *sys_thread_context_get_stack_pointer(sys_thread_context_t *context)
  {
    return (void *)(context->_context.Esp);
  }

} // end namespace scan
