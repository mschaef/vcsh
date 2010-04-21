/* unix-sys.cpp
 * June 2nd, 2007
 *
 * Unix (Linux/MacOS) version of system access routines.
 */

#define _XOPEN_SOURCE // Necessary for the ucontext functions we shouldn't be using.

#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <dirent.h>
#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <ucontext.h>

#include "scan.h"

namespace scan {

  static sys_retcode_t rc_to_sys_retcode_t(int rc);
  static sys_retcode_t sys_init_threads();
  static sys_retcode_t sys_init_time();

  sys_retcode_t sys_init()
  {
    if (sys_init_threads() != SYS_OK)
      return SYS_ENOTRECOVERABLE;

    if (sys_init_time() != SYS_OK)
      return SYS_ENOTRECOVERABLE;

    return SYS_OK;
  }

  /****************************************************************
   * Environment Variable Access
   */

  _TCHAR **sys_get_env_vars()
  {
    return environ;
  }

  sys_retcode_t sys_setenv(_TCHAR *varname, _TCHAR *value)
  {
    return rc_to_sys_retcode_t(setenv(varname, value, 1)); // 1 == always overwrite
  }

  struct sys_dir_t
  {
    DIR *_dir;
  };

  sys_retcode_t sys_opendir(const char *path, sys_dir_t **dir)
  {
    *dir = (sys_dir_t *)safe_malloc(sizeof(sys_dir_t));

    if (*dir == NULL)
      return SYS_ENOMEM;

    (*dir)->_dir = opendir(path);

    if ((*dir)->_dir == NULL)
      {
        safe_free(*dir);
        *dir = NULL;

        return rc_to_sys_retcode_t(errno);
      }

    return SYS_OK;
  }


  sys_retcode_t sys_readdir(sys_dir_t *dir, sys_dirent_t *ent, bool *done)
  {
    struct dirent *sent;

    errno = 0;
    sent = readdir(dir->_dir);

    if (sent == NULL)
      {
        *done = true;
        return rc_to_sys_retcode_t(errno);
      }
    else
      *done = false;

    ent->_ino  = sent->d_ino;

#if !defined(__CYGWIN__)
    switch(sent->d_type) {
    case DT_FIFO: ent->_type = SYS_FT_FIFO; break;
    case DT_CHR:  ent->_type = SYS_FT_CHR;  break;
    case DT_DIR:  ent->_type = SYS_FT_DIR;  break;
    case DT_BLK:  ent->_type = SYS_FT_BLK;  break;
    case DT_REG:  ent->_type = SYS_FT_REG;  break;
    case DT_LNK:  ent->_type = SYS_FT_LNK;  break;
    case DT_SOCK: ent->_type = SYS_FT_SOCK; break;
    case DT_WHT:  ent->_type = SYS_FT_FIFO; break;
    case DT_UNKNOWN:
      /* fall-through */
    default:
      ent->_type = SYS_FT_UNKNOWN;
      break;
    }
#else
    ent->_type = SYS_FT_UNKNOWN;
#endif

    strncpy(ent->_name, sent->d_name, SYS_NAME_MAX);

    return SYS_OK;
  }

  sys_retcode_t sys_closedir(sys_dir_t *dir)
  {
    DIR *d = dir->_dir;

    dir->_dir = NULL;
    safe_free(dir);

    return rc_to_sys_retcode_t(closedir(d));
  }

  const char *filename_beginning(const char *path)
  {
    const char *beg = path;

    for(; *path != '\0'; path++)
      if (*path != '/')
        beg = path;

    return beg;
  }

  sys_retcode_t sys_stat(const char *path, sys_stat_t *buf)
  {
    struct stat sbuf;

    int rc = stat(path, &sbuf);

    if (rc)
      return rc_to_sys_retcode_t(rc);


    if (S_ISREG(sbuf.st_mode))       buf->_filetype = SYS_FT_REG;
    else if (S_ISDIR(sbuf.st_mode))  buf->_filetype = SYS_FT_DIR;
    else if (S_ISCHR(sbuf.st_mode))  buf->_filetype = SYS_FT_CHR;
    else if (S_ISBLK(sbuf.st_mode))  buf->_filetype = SYS_FT_BLK;
    else if (S_ISFIFO(sbuf.st_mode)) buf->_filetype = SYS_FT_FIFO;
    else if (S_ISLNK(sbuf.st_mode))  buf->_filetype = SYS_FT_LNK;
    else if (S_ISSOCK(sbuf.st_mode)) buf->_filetype = SYS_FT_SOCK;

    buf->_attrs   = SYS_FATTR_NONE;

    if (*filename_beginning(path) == '.')
      buf->_attrs = (sys_file_attrs_t)(buf->_attrs | SYS_FATTR_HIDDEN);

    buf->_mode    = sbuf.st_mode & ~S_IFMT;

    buf->_size    = sbuf.st_size;
    buf->_atime   = sbuf.st_atime;
    buf->_mtime   = sbuf.st_mtime;
    buf->_ctime   = sbuf.st_ctime;

    return SYS_OK;
  }

  sys_retcode_t sys_temporary_filename(_TCHAR *prefix, _TCHAR *buf, size_t buflen)
  {
    char *name = tempnam(NULL, prefix);

    if (name)
      {
        strncpy(buf, name, buflen);
        free(name);

        return SYS_OK;
      }

    switch(errno) {
    case ENOMEM: return SYS_ENOMEM;
    case EEXIST: return SYS_EEXIST;
    default: return SYS_EWIERD;
    }
  }

  sys_retcode_t sys_delete_file(_TCHAR *filename)
  {
    return rc_to_sys_retcode_t(unlink(filename));
  }


  /****************************************************************
   * Time and Date
   */

  static flonum_t runtime_offset = 0.0;  // timebase offset to interp start

  static flonum_t sys_timebase_time(void);

  static sys_retcode_t sys_init_time()
  {
    // Record the current time so that we can get a measure of uptime
    runtime_offset = sys_timebase_time();
    
    return SYS_OK;
  }

  static flonum_t sys_timebase_time(void)
  {
    struct timeval tv;

    gettimeofday(&tv, NULL);

    return tv.tv_sec + tv.tv_usec / 1000000.0;
  }

  flonum_t sys_realtime(void)
  {
    return sys_timebase_time();
  }

  flonum_t sys_runtime(void)
  {
    return sys_timebase_time() - runtime_offset;
  }

  flonum_t sys_time_resolution()
  {
    return 1000000.0;
  }

  flonum_t sys_timezone_offset() // XXX: This does not accurately capture DST on MacOS X
  {
    struct timezone tz;

    gettimeofday(NULL, &tz);

    return (flonum_t)tz.tz_minuteswest * SECONDS_PER_MINUTE;
  }

  /****************************************************************
   * System Information
   */

  sys_retcode_t sys_gethostname(_TCHAR *buf, size_t len)
  {
    return rc_to_sys_retcode_t(gethostname(buf, len));
  }

  void sys_get_info(sys_info_t *info)
  {
    info->_machine_bits            = 32;
    info->_eoln                    = SYS_EOLN_LF;
    info->_fs_names_case_sensitive = true;
    info->_platform_name           = _T("linux");
  }

  /****************************************************************
   * Error Code Mapping
   */

  static sys_retcode_t rc_to_sys_retcode_t(int rc)
  {
    switch(rc) {
    case 0               : return SYS_OK;

    case EPERM           : return SYS_EPERM;
    case ENOENT          : return SYS_ENOENT;
    case ESRCH           : return SYS_ESRCH;
    case EINTR           : return SYS_EINTR;
    case EIO             : return SYS_EIO;
    case ENXIO           : return SYS_ENXIO;
    case E2BIG           : return SYS_E2BIG;
    case ENOEXEC         : return SYS_ENOEXEC;
    case EBADF           : return SYS_EBADF;
    case ECHILD          : return SYS_ECHILD;
    case EAGAIN          : return SYS_EAGAIN;
    case ENOMEM          : return SYS_ENOMEM;
    case EACCES          : return SYS_EACCES;
    case EFAULT          : return SYS_EFAULT;
    case ENOTBLK         : return SYS_ENOTBLK;
    case EBUSY           : return SYS_EBUSY;
    case EEXIST          : return SYS_EEXIST;
    case EXDEV           : return SYS_EXDEV;
    case ENODEV          : return SYS_ENODEV;
    case ENOTDIR         : return SYS_ENOTDIR;
    case EISDIR          : return SYS_EISDIR;
    case EINVAL          : return SYS_EINVAL;
    case ENFILE          : return SYS_ENFILE;
    case EMFILE          : return SYS_EMFILE;
    case ENOTTY          : return SYS_ENOTTY;
    case ETXTBSY         : return SYS_ETXTBSY;
    case EFBIG           : return SYS_EFBIG;
    case ENOSPC          : return SYS_ENOSPC;
    case ESPIPE          : return SYS_ESPIPE;
    case EROFS           : return SYS_EROFS;
    case EMLINK          : return SYS_EMLINK;
    case EPIPE           : return SYS_EPIPE;
    case EDOM            : return SYS_EDOM;
    case ERANGE          : return SYS_ERANGE;
    case EDEADLK         : return SYS_EDEADLK;
    case ENAMETOOLONG    : return SYS_ENAMETOOLONG;
    case ENOLCK          : return SYS_ENOLCK;
    case ENOSYS          : return SYS_ENOSYS;
    case ENOTEMPTY       : return SYS_ENOTEMPTY;
    case ELOOP           : return SYS_ELOOP;
    case ENOMSG          : return SYS_ENOMSG;
      //case EL2NSYNC        : return SYS_EL2NSYNC;
      //case EL3HLT          : return SYS_EL3HLT;
      //case EL3RST          : return SYS_EL3RST;
      //case ELNRNG          : return SYS_ELNRNG;
      //case EUNATCH         : return SYS_EUNATCH;
      //case ENOCSI          : return SYS_ENOCSI;
      //case EL2HLT          : return SYS_EL2HLT;
      //case EBADE           : return SYS_EBADE;
      //case EBADR           : return SYS_EBADR;
      //case EXFULL          : return SYS_EXFULL;
      //case ENOANO          : return SYS_ENOANO;
      //case EBADRQC         : return SYS_EBADRQC;
      //case EBADSLT         : return SYS_EBADSLT;
      //case EBFONT          : return SYS_EBFONT;
    case ENOSTR          : return SYS_ENOSTR;
    case ETIME           : return SYS_ETIME;
      //case ENONET          : return SYS_ENONET;
      //case ENOPKG          : return SYS_ENOPKG;
    case EREMOTE         : return SYS_EREMOTE;
    case ENOLINK         : return SYS_ENOLINK;
      //case EADV            : return SYS_EADV;
      //case ESRMNT          : return SYS_ESRMNT;
      //case ECOMM           : return SYS_ECOMM;
    case EPROTO          : return SYS_EPROTO;
    case EMULTIHOP       : return SYS_EMULTIHOP;
      //case EDOTDOT         : return SYS_EDOTDOT;
    case EBADMSG         : return SYS_EBADMSG;
    case EOVERFLOW       : return SYS_EOVERFLOW;
      //case ENOTUNIQ        : return SYS_ENOTUNIQ;
      //case EBADFD          : return SYS_EBADFD;
      //case EREMCHG         : return SYS_EREMCHG;
      //case ELIBACC         : return SYS_ELIBACC;
      //case ELIBBAD         : return SYS_ELIBBAD;
      //case ELIBSCN         : return SYS_ELIBSCN;
      //case ELIBMAX         : return SYS_ELIBMAX;
      //case ELIBEXEC        : return SYS_ELIBEXEC;
    case EILSEQ          : return SYS_EILSEQ;
      // case ERESTART        : return SYS_ERESTART;
      // case ESTRPIPE        : return SYS_ESTRPIPE;
    case EUSERS          : return SYS_EUSERS;
    case ENOTSOCK        : return SYS_ENOTSOCK;
    case EDESTADDRREQ    : return SYS_EDESTADDRREQ;
    case EMSGSIZE        : return SYS_EMSGSIZE;
    case EPROTOTYPE      : return SYS_EPROTOTYPE;
    case ENOPROTOOPT     : return SYS_ENOPROTOOPT;
    case EPROTONOSUPPORT : return SYS_EPROTONOSUPPORT;
    case ESOCKTNOSUPPORT : return SYS_ESOCKTNOSUPPORT;
    case EOPNOTSUPP      : return SYS_EOPNOTSUPP;
    case EPFNOSUPPORT    : return SYS_EPFNOSUPPORT;
    case EAFNOSUPPORT    : return SYS_EAFNOSUPPORT;
    case EADDRINUSE      : return SYS_EADDRINUSE;
    case EADDRNOTAVAIL   : return SYS_EADDRNOTAVAIL;
    case ENETDOWN        : return SYS_ENETDOWN;
    case ENETUNREACH     : return SYS_ENETUNREACH;
    case ENETRESET       : return SYS_ENETRESET;
    case ECONNABORTED    : return SYS_ECONNABORTED;
    case ECONNRESET      : return SYS_ECONNRESET;
    case ENOBUFS         : return SYS_ENOBUFS;
    case EISCONN         : return SYS_EISCONN;
    case ENOTCONN        : return SYS_ENOTCONN;
    case ESHUTDOWN       : return SYS_ESHUTDOWN;
    case ETOOMANYREFS    : return SYS_ETOOMANYREFS;
    case ETIMEDOUT       : return SYS_ETIMEDOUT;
    case ECONNREFUSED    : return SYS_ECONNREFUSED;
    case EHOSTDOWN       : return SYS_EHOSTDOWN;
    case EHOSTUNREACH    : return SYS_EHOSTUNREACH;
    case EALREADY        : return SYS_EALREADY;
    case EINPROGRESS     : return SYS_EINPROGRESS;
    case ESTALE          : return SYS_ESTALE;
      // case EUCLEAN         : return SYS_EUCLEAN;
      // case ENOTNAM         : return SYS_ENOTNAM;
      // case ENAVAIL         : return SYS_ENAVAIL;
      // case EISNAM          : return SYS_EISNAM;
      // case EREMOTEIO       : return SYS_EREMOTEIO;
    case EDQUOT          : return SYS_EDQUOT;
      //case ENOMEDIUM       : return SYS_ENOMEDIUM;
      // case EMEDIUMTYPE     : return SYS_EMEDIUMTYPE;
      // case ECANCELED       : return SYS_ECANCELED;
      // case ENOKEY          : return SYS_ENOKEY;
      // case EKEYEXPIRED     : return SYS_EKEYEXPIRED;
      // case EKEYREVOKED     : return SYS_EKEYREVOKED;
      // case EKEYREJECTED    : return SYS_EKEYREJECTED;
      // case EOWNERDEAD      : return SYS_EOWNERDEAD;
      // case ENOTRECOVERABLE : return SYS_ENOTRECOVERABLE;
    default              : return SYS_EWIERD;
    }
  }

  /****************************************************************
   * Debug I/O
   */

  void output_debug_string(const _TCHAR *str)
  {
    fputs(str, stderr);
  }

  enum { MESSAGE_BUF_SIZE = 256 };

  int debug_printf(const _TCHAR *format, ...)
  {
    int i;
    va_list args;
    _TCHAR buf[MESSAGE_BUF_SIZE];

    va_start(args, format);
    i = _vsntprintf(buf, MESSAGE_BUF_SIZE, format, args);
    va_end(args);

    output_debug_string(buf);

    return i;
  }

  /****************************************************************
   * Panic Handling
   */

  static panic_handler_t current_panic_handler = NULL;

  panic_handler_t set_panic_handler(panic_handler_t new_handler)
  {
    panic_handler_t old_handler = current_panic_handler;

    current_panic_handler = new_handler;

    return old_handler;
  }


  void _panic(const _TCHAR *str, const _TCHAR *filename, long lineno)
  {
    _TCHAR buf[MESSAGE_BUF_SIZE];
    _sntprintf(buf, MESSAGE_BUF_SIZE, "Fatal Error: %s @ (%s:%ld)\n", str, filename, lineno);

    output_debug_string(buf);

    if (current_panic_handler)
      current_panic_handler();

    exit(1);
  }

  void debug_break()
  {
    __asm__ __volatile__ ("int3"); // !!! Is this the gdb way to simulate a breakpoint?
  }

  /****************************************************************
   * Threads
   */

  struct _sys_thread_t
  {
    pthread_t pt;

    thread_entry_t entry;

    void *userdata;

    ucontext_t ucontext;
    jmp_buf jb;
  };

  // This is the thread we started the process on
  static struct _sys_thread_t initial_thread;

  // All threads carry a single user pointer around that's a pointer to a thread local block (defined above)
  static pthread_key_t ptkey_info;

  sys_retcode_t sys_init_threads()
  {
    if (pthread_key_create(&ptkey_info, NULL) != 0)
      return SYS_ENOMEM;

    initial_thread.pt       = pthread_self();
    initial_thread.entry    = NULL;
    initial_thread.userdata = NULL;
    getcontext(&initial_thread.ucontext);

    pthread_setspecific(ptkey_info, &initial_thread);

    return SYS_OK;
  }

  sys_thread_t sys_current_thread()
  {
    return (sys_thread_t)pthread_getspecific(ptkey_info);
  }

  static void *thread_main(void *userdata)
  {
    sys_thread_t thinfo = (sys_thread_t)userdata;

    pthread_setspecific(ptkey_info, thinfo);

    getcontext(&(thinfo->ucontext));

    thinfo->entry(thinfo->userdata);

    safe_free(thinfo);

    return NULL;
  }

  sys_thread_t sys_create_thread(thread_entry_t entry, uptr max_stack_size,  void *userdata)
  { 
    sys_thread_t thinfo = (sys_thread_t)safe_malloc(sizeof(_sys_thread_t));

    if (thinfo == NULL)
      return NULL;

    thinfo->entry          = entry;
    thinfo->userdata       = userdata;
    
    int rc = pthread_create(&(thinfo->pt), NULL, thread_main, thinfo);
    
    if (rc != 0)
      {
        safe_free(thinfo);

        return NULL;
      }

    return thinfo;
  }

  void *sys_get_stack_start()
  {
    sys_thread_t thinfo = sys_current_thread();

    assert(thinfo != NULL);

  return thinfo->ucontext.uc_stack.ss_sp;
  }

  void *sys_current_thread_userdata()
  {
    sys_thread_t thinfo = sys_current_thread();

    assert(thinfo != NULL);

    return thinfo->userdata;
  }

  void sys_set_current_thread_userdata(void *userdata)
  {
    sys_thread_t thinfo = sys_current_thread();

    assert(thinfo != NULL);

    thinfo->userdata = userdata;
  }

  void *sys_set_thread_stack_limit(size_t new_size_limit) // XXX: Unimplemented
  {
    return NULL;
  }

  sys_retcode_t sys_suspend_thread(sys_thread_t thread) // XXX: Unimplemented
  {
    return SYS_OK;
  }

  sys_retcode_t sys_resume_thread(sys_thread_t thread) // XXX: Unimplemented
  {
    return SYS_OK;
  }

  struct _sys_critical_section_t // REVISIT: Add magic number?
  {
    pthread_mutex_t mutex;
  };

  sys_critical_section_t sys_create_critical_section()
  {
    sys_critical_section_t crit_sec =
      (sys_critical_section_t)safe_malloc(sizeof(_sys_critical_section_t));

    if (crit_sec == NULL)
      return NULL;

    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);

    int rc = pthread_mutex_init(&(crit_sec->mutex), &attr);

    pthread_mutexattr_destroy(&attr);

    if (rc != 0)
      {
        safe_free(crit_sec);

        return NULL;
      }

    return crit_sec;
  }

  void sys_enter_critical_section(sys_critical_section_t crit_sec)
  {
    assert(crit_sec != NULL);

    pthread_mutex_lock(&(crit_sec->mutex));
  }

  void sys_leave_critical_section(sys_critical_section_t crit_sec)
  {
    assert(crit_sec != NULL);

    pthread_mutex_unlock(&(crit_sec->mutex));
  }

  void sys_delete_critical_section(sys_critical_section_t crit_sec)
  {
    assert(crit_sec != NULL);

    pthread_mutex_destroy(&(crit_sec->mutex));

    safe_free(crit_sec);
  }

  #define MSEC_PER_USEC 1000

  void sys_sleep(uintptr_t duration_ms)
  {
    usleep(duration_ms * MSEC_PER_USEC);
  }

  struct _sys_thread_context_t
  {
    int unused;
  };

  sys_thread_context_t sys_get_thread_context(sys_thread_t thread)
  {
    assert(thread == sys_current_thread());
   
    return NULL;
  }

  void sys_thread_context_get_state_region(sys_thread_context_t context, void **low, void **high)
  {
    *low  = NULL;
    *high = NULL;
  }

  void *sys_thread_context_get_stack_pointer(sys_thread_context_t context)
  {
    return NULL;
  }

  /****************************************************************
   * String utilities
   */

#if defined(__CYGWIN__) || defined (SCAN_UNIX)
  char *strchrnul(char *string, int c) // REVISIT: Also implmented in windows-sys.cpp... consolidate
  {
    for(; *string; string++)
      if (*string == c)
        break;

    return string;
  }
#endif

} // end namespace scan

