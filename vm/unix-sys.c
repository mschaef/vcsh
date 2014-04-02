/*
 * unix-sys.c --
 *
 * Unix specific system functions. This implements the API
 * defined in scan-sys.h.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

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
#include <errno.h>
#include <time.h>

#include "scan-sys.h"
#include "scan-private.h"

static enum sys_retcode_t rc_to_sys_retcode_t(int rc);
static enum sys_retcode_t sys_init_time();

static uint8_t *sys_stack_start;

uint8_t *stack_limit_obj;

enum sys_retcode_t sys_init()
{
     int stack_location;

     sys_stack_start = (uint8_t *) & stack_location;

     if (sys_init_time() != SYS_OK)
          return SYS_E_FAIL;

     sys_set_stack_limit(DEFAULT_STACK_SIZE);

     return SYS_OK;
}

/****************************************************************
 * Environment Variable Access
 */

_TCHAR **sys_get_env_vars()
{
     return environ;
}

enum sys_retcode_t sys_setenv(_TCHAR * varname, _TCHAR * value)
{
     if (setenv(varname, value, 1)) /* 1 == always overwrite */
          return rc_to_sys_retcode_t(errno);

     return SYS_OK;
}

struct sys_dir_t
{
     DIR *_dir;
};

enum sys_retcode_t sys_opendir(const char *path, struct sys_dir_t ** dir)
{
     *dir = gc_malloc(sizeof(**dir));

     if (*dir == NULL)
          return SYS_E_OUT_OF_MEMORY;

     (*dir)->_dir = opendir(path);

     if ((*dir)->_dir == NULL)
     {
          gc_free(*dir);
          *dir = NULL;

          return rc_to_sys_retcode_t(errno);
     }

     return SYS_OK;
}


enum sys_retcode_t sys_readdir(struct sys_dir_t * dir,
                               struct sys_dirent_t * ent,
                               bool * done)
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

     ent->_ino = sent->d_ino;

#if !defined(__CYGWIN__) && !defined(__APPLE__)
     switch (sent->d_type)
     {
     case DT_FIFO:
          ent->_type = SYS_FT_FIFO;
          break;
     case DT_CHR:
          ent->_type = SYS_FT_CHR;
          break;
     case DT_DIR:
          ent->_type = SYS_FT_DIR;
          break;
     case DT_BLK:
          ent->_type = SYS_FT_BLK;
          break;
     case DT_REG:
          ent->_type = SYS_FT_REG;
          break;
     case DT_LNK:
          ent->_type = SYS_FT_LNK;
          break;
     case DT_SOCK:
          ent->_type = SYS_FT_SOCK;
          break;
     case DT_WHT:
          ent->_type = SYS_FT_FIFO;
          break;
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

enum sys_retcode_t sys_closedir(struct sys_dir_t * dir)
{
     DIR *d = dir->_dir;

     dir->_dir = NULL;
     gc_free(dir);

     return rc_to_sys_retcode_t(closedir(d));
}

const char *filename_beginning(const char *path)
{
     const char *beg = path;

     for (; *path != '\0'; path++)
          if (*path != '/')
               beg = path;

     return beg;
}

enum sys_retcode_t sys_stat(const char *path, struct sys_stat_t * buf)
{
     struct stat sbuf;

     if (stat(path, &sbuf))
          return rc_to_sys_retcode_t(errno);

     if (S_ISREG(sbuf.st_mode))
          buf->_filetype = SYS_FT_REG;
     else if (S_ISDIR(sbuf.st_mode))
          buf->_filetype = SYS_FT_DIR;
     else if (S_ISCHR(sbuf.st_mode))
          buf->_filetype = SYS_FT_CHR;
     else if (S_ISBLK(sbuf.st_mode))
          buf->_filetype = SYS_FT_BLK;
     else if (S_ISFIFO(sbuf.st_mode))
          buf->_filetype = SYS_FT_FIFO;
     else if (S_ISLNK(sbuf.st_mode))
          buf->_filetype = SYS_FT_LNK;
     else if (S_ISSOCK(sbuf.st_mode))
          buf->_filetype = SYS_FT_SOCK;

     buf->_attrs = SYS_FATTR_NONE;

     if (*filename_beginning(path) == '.')
          buf->_attrs = (enum sys_file_attrs_t) (buf->_attrs | SYS_FATTR_HIDDEN);

     buf->_mode = sbuf.st_mode & ~S_IFMT;

     buf->_size = sbuf.st_size;
     buf->_atime = sbuf.st_atime;
     buf->_mtime = sbuf.st_mtime;
     buf->_ctime = sbuf.st_ctime;

     return SYS_OK;
}

enum sys_retcode_t sys_temporary_filename(_TCHAR * prefix,
                                          _TCHAR * buf,
                                          size_t buflen)
{
     char *name = tempnam(NULL, prefix);

     if (name)
     {
          strncpy(buf, name, buflen);
          free(name);

          return SYS_OK;
     }

     switch (errno)
     {
     case ENOMEM:
          return SYS_E_OUT_OF_MEMORY;
     case EEXIST:
          return SYS_E_FILE_EXISTS;
     default:
          return SYS_E_FAIL;
     }
}

enum sys_retcode_t sys_delete_file(_TCHAR * filename)
{
     if (unlink(filename))
          return rc_to_sys_retcode_t(errno);

     return SYS_OK;
}


/****************************************************************
 * Time and Date
 */

static double runtime_offset = 0.0;   /* timebase offset to interp start */

static double sys_timebase_time(void);

static enum sys_retcode_t sys_init_time()
{
     tzset();

     /*  Record the current time so that we can get a measure of uptime */
     runtime_offset = sys_timebase_time();

     return SYS_OK;
}

static double sys_timebase_time(void)
{
     struct timeval tv;

     gettimeofday(&tv, NULL);

     return tv.tv_sec + tv.tv_usec / 1000000.0;
}

double sys_realtime(void)
{
     return sys_timebase_time();
}

double sys_runtime(void)
{
     return sys_timebase_time() - runtime_offset;
}

double sys_time_resolution()
{
     return 1000000.0;
}

double sys_timezone_offset()
{
     time_t current_time = time(NULL);
     struct tm ltbuf;

     localtime_r(&current_time, &ltbuf);

     return (double) -ltbuf.tm_gmtoff;
}

/****************************************************************
 * System Information
 */

enum sys_retcode_t sys_gethostname(_TCHAR * buf, size_t len)
{
     if (gethostname(buf, len))
          return rc_to_sys_retcode_t(errno);

     return SYS_OK;
}

enum sys_eoln_convention_t sys_get_eoln_convention()
{
     return SYS_EOLN_LF;
}

bool sys_get_fs_names_case_sensitive()
{
     return true;
}

const _TCHAR *sys_get_platform_name()
{
     return _T("linux");
}


/****************************************************************
 * Error Code Mapping
 */

static enum sys_retcode_t rc_to_sys_retcode_t(int rc)
{
     switch (rc)
     {
     case 0:
          return SYS_OK;

     case EPERM:
     case EACCES:
          return SYS_E_NOT_PERMITTED;

     case ENOENT:
     case ENXIO:
     case ENODEV:
          return SYS_E_NO_FILE;

     case EIO:
          return SYS_E_IO_ERROR;

     case E2BIG:
          /*
     case ENOTBLK:
          */
          return SYS_E_BAD_ARGUMENT;

     case ENOMEM:
          return SYS_E_OUT_OF_MEMORY;

     case EFAULT:
          return SYS_E_BAD_ADDRESS;

     case EEXIST:
          return SYS_E_FILE_EXISTS;

     case EINVAL:
          return SYS_E_BAD_ARGUMENT;;

     case ENOTDIR:
          return SYS_E_NOT_DIRECTORY;

     case EISDIR:
          return SYS_E_IS_DIRECTORY;

     case ENOSPC:
          return SYS_E_NO_SPACE;

     default:
          return SYS_E_FAIL;
     }
}

/****************************************************************
 * Debug I/O
 */

void sys_output_debug_string(const _TCHAR * str)
{
     fputs(str, stderr);
}

int debug_printf(const _TCHAR * format, ...)
{
     int i;
     va_list args;
     _TCHAR buf[DEBUG_MESSAGE_BUF_SIZE];

     va_start(args, format);
     i = _vsntprintf(buf, DEBUG_MESSAGE_BUF_SIZE, format, args);
     va_end(args);

     sys_output_debug_string(buf);

     return i;
}

/****************************************************************
 * Panic Handling
 */

void sys_abnormally_terminate_vm(int rc)
{
     exit(rc);
}

void sys_debug_break()
{
     __asm__ __volatile__("int3");
}

void *sys_get_stack_start()
{
     return (void *) sys_stack_start;
}

void *sys_set_stack_limit(size_t new_size_limit)
{
     /* If the size limit is greater than the address, the computation of
      * stack_limit_obj would wrap around the address space, put the limit
      * at the very top of the address space, and therefore immediately trigger
      * a stack limit violation at the next check. This clamp keeps that
      * from happening.
      */
     if (new_size_limit > (uintptr_t) sys_stack_start)
          new_size_limit = 0;

     if (new_size_limit == 0)
          stack_limit_obj = (uint8_t *) 0;
     else
          stack_limit_obj = (uint8_t *) (sys_stack_start - new_size_limit);

     return stack_limit_obj;
}

#define MSEC_PER_USEC 1000

void sys_sleep(uintptr_t duration_ms)
{
     usleep(duration_ms * MSEC_PER_USEC);
}



