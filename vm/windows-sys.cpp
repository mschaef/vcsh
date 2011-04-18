
/*
 * windows-sys.cpp --
 *
 * Windows specific system functions. This implements the API
 * defined in scan-sys.h.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <tchar.h>
#include <process.h>

#ifdef SCAN_WINDOWS
#  pragma warning (push)
#  pragma warning (disable: 4668 4826)
#    include <windows.h>
#  pragma warning (pop)
#endif

#include <WinNT.h>

#include "shlwapi.h"

#include "scan-private.h"

BEGIN_NAMESPACE(scan)

  struct _sys_thread_context_t
  {
    CONTEXT _context;
  };

sys_retcode_t rc_to_sys_retcode_t(DWORD rc); /*  forward decl */

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

    *dir = (sys_dir_t *)gc_malloc(sizeof(sys_dir_t));

    if (*dir == NULL)
      return SYS_E_OUT_OF_MEMORY;


    (*dir)->_fh = FindFirstFile(path_buf, &((*dir)->_fd));

    if ((*dir)->_fh == INVALID_HANDLE_VALUE)
      {
        DWORD error_code = GetLastError();

        gc_free(*dir);
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
         return SYS_FT_BLK; /*  REVISIT: Can we do better than assuming every dev is block? */
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

    gc_free(dir);

    return retcode;
  }

/* FILETIME units (FT_UNITS) are 100ns units measured from an
 * epoch beginning January 1st, 1601. These are the conversion
 * factors necessary to convert them into time_t units, which
 * are seconds from January 1st, 1970. The offset is itself
 * measured in FILETIME units.
 */
  const uint64_t FT_UNITS_PER_SECOND = UINT64_C(10000000);
  const uint64_t FT_UNITS_OFS        = UINT64_C(116444736000000000);
  static sys_time_t filetime_to_time_t(FILETIME ft)
  {
    uint64_t ftlinear = make_uint64_t(ft.dwHighDateTime, ft.dwLowDateTime);

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

    /*  REVISIT: add alternate filename */

    /*  REVISIT: Better way to get mode info? */
    buf->_mode    = (fd.dwFileAttributes & FILE_ATTRIBUTE_READONLY) ? 0555 : 0777;

    buf->_size    = make_int64_t(fd.nFileSizeHigh, fd.nFileSizeLow);
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

static double realtime_offset = 0.0; /*  timebase offset to epoch */
static double runtime_offset = 0.0;  /*  timebase offset to interp start */

  static double sys_timebase_time(void);

  static sys_retcode_t sys_init_time()
  {
    LARGE_INTEGER temp;

    /*  Determine the number of high resolution ticks per second */
    if (QueryPerformanceFrequency(&temp) == 0) /*  0 => FAIL */
      {
        have_highres_timebase = false;
        runtime_ticks_per_sec = 1000;
      }
    else
      {
        have_highres_timebase = true;
        runtime_ticks_per_sec = temp.QuadPart;
      }

    /*  Get the current time so that we can accurately return a high resolution
     *  time relative to the Unix epoch. */
    FILETIME ft_runtime;
    GetSystemTimeAsFileTime(&ft_runtime);

    realtime_offset = (double)filetime_to_time_t(ft_runtime) - sys_timebase_time();

    /*  Record the current time so that we can get a measure of uptime */
    runtime_offset = sys_timebase_time();

    return SYS_OK;
  }

  static double sys_timebase_time(void)
  {
    LARGE_INTEGER temp;
    __int64 counterValue;

    if (!have_highres_timebase)
      return ((double)GetTickCount() / runtime_ticks_per_sec);

    QueryPerformanceCounter(&temp);
    counterValue = temp.QuadPart;

    return ((double)counterValue / (double)runtime_ticks_per_sec);
  }

  double sys_realtime(void)
  {
    return sys_timebase_time() + realtime_offset;
  }

  double sys_runtime(void)
  {
    return sys_timebase_time() - runtime_offset;
  }

  double sys_time_resolution()
  {
    sys_runtime();

    return (double)runtime_ticks_per_sec;
  }

  double sys_timezone_offset()
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

        /*  The TIME_ZONE_ID_UNKNOWN case gets no additional bias */
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

  extern "C" int debug_printf(const _TCHAR *format, ...)
  {
    int i;
    va_list args;
    _TCHAR buf[DEBUG_MESSAGE_BUF_SIZE];

    va_start(args, format);
    i = _vsntprintf(buf, DEBUG_MESSAGE_BUF_SIZE, format, args);
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

void sys_debug_break(); /*  REVISIT: where does this prototype really go? */

void sys_abnormally_terminate_vm(int rc)
{
     exit(rc);
}

  void sys_output_debug_string(const _TCHAR *str)
  {
    fprintf(stderr, "%s", buf);
    fflush(stderr);

    OutputDebugString(str);
  }

  void sys_debug_break()
  {
#if defined(_MSC_VER)
    __debugbreak();
#elif defined(__GNUC__)
    __asm__ __volatile__ ("int3");
#else
#error sys_debug_break not supported.
#endif
  }

  sys_retcode_t rc_to_sys_retcode_t(DWORD rc)
  {
    _TCHAR msg_buf[STACK_STRBUF_LEN];

    switch(rc) {
    case NO_ERROR:                                     return SYS_OK;
    case ERROR_INVALID_FUNCTION:                       return SYS_E_BAD_ARGUMENT;
    case ERROR_FILE_NOT_FOUND:                         return SYS_E_NO_FILE;
    case ERROR_PATH_NOT_FOUND:                         return SYS_E_NO_FILE;
    case ERROR_TOO_MANY_OPEN_FILES:                    return SYS_E_OUT_OF_MEMORY;
    case ERROR_ACCESS_DENIED:                          return SYS_E_NOT_PERMITTED;
    case ERROR_INVALID_HANDLE:                         return SYS_E_BAD_ARGUMENT;
    case ERROR_NOT_ENOUGH_MEMORY:                      return SYS_E_OUT_OF_MEMORY;
    case ERROR_INVALID_BLOCK:                          return SYS_E_BAD_ARGUMENT;
    case ERROR_OUTOFMEMORY:                            return SYS_E_OUT_OF_MEMORY;
    case ERROR_INVALID_DRIVE:                          return SYS_E_NOT_DIRECTORY;
    case ERROR_CURRENT_DIRECTORY:                      return SYS_E_NOT_PERMITTED;
    case ERROR_WRITE_PROTECT:                          return SYS_E_NOT_PERMITTED;
    case ERROR_BAD_UNIT:                               return SYS_E_NO_FILE;
    case ERROR_SEEK:                                   return SYS_E_BAD_ARGUMENT;
    case ERROR_WRITE_FAULT:                            return SYS_E_IO_ERROR;
    case ERROR_READ_FAULT:                             return SYS_E_IO_ERROR;
    case ERROR_GEN_FAILURE:                            return SYS_E_IO_ERROR;
    case ERROR_HANDLE_DISK_FULL:                       return SYS_E_NO_SPACE;
    case ERROR_DEV_NOT_EXIST:                          return SYS_E_NO_FILE;
    case ERROR_BUFFER_OVERFLOW:                        return SYS_E_NAME_TOO_LONG;
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

      return SYS_E_FAIL;
    }
  }


  static uint8_t *sys_stack_start;
  uint8_t *stack_limit_obj;

  static bool sys_initialized = false;

  sys_retcode_t sys_init()
  {
       /*  REVISIT: Can this be done more efficiently with inline assembly? */
    int stack_location;

    sys_stack_start = (uint8_t *)&stack_location;

    if (sys_init_time() != SYS_OK)
      return SYS_E_FAIL;

    sys_set_stack_limit(DEFAULT_STACK_SIZE);
	
    return SYS_OK;
  }

  void *sys_get_stack_start()
  {
    return (void *)sys_stack_start;
  }

     void *sys_set_stack_limit(size_t new_size_limit) /*  new_size_limit of 0 disables limit checking */
  {
    /* If the size limit is greater than the address, the computation of
     * stack_limit_obj would wrap around the address space, put the limit
     * at the very top of the address space, and therefore immediately trigger
     * a stack limit violation at the next check. This clamp keeps that
     * from happening.
     */
    if (new_size_limit > (uintptr_t)sys_stack_start)
      new_size_limit = 0;

    if (new_size_limit == 0)
      stack_limit_obj = (uint8_t *)0;
    else
      stack_limit_obj = (uint8_t *)(sys_stack_start - new_size_limit);

    return stack_limit_obj;
  }

  void sys_sleep(uintptr_t duration_ms)
  {
    Sleep(duration_ms);
  }


END_NAMESPACE
