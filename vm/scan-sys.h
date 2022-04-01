
/*
 * scan-sys.h --
 *
 * The system abstraction layer's API.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __SCAN_SYS_H
#define __SCAN_SYS_H

#include <assert.h>

#ifdef SCAN_UNIX
#  include <sys/time.h>
#  include <limits.h>
#endif

#ifdef SCAN_WINDOWS
#  include <time.h>
#  include <windows.h>
#endif

#include <limits.h>

#include "scan-base.h"

#include "scan-constants.h"

#ifdef SCAN_UNIX
#  define SYS_PATH_MAX PATH_MAX
#  define SYS_NAME_MAX NAME_MAX
#endif

#ifdef SCAN_WINDOWS
#  define SYS_PATH_MAX MAX_PATH
#  define SYS_NAME_MAX MAX_PATH
#endif

void sys_abnormally_terminate_vm(int rc);

#ifdef CHECKED
#	define checked_assert(exp) assert(exp)
#else
#	define checked_assert(exp)
#endif

enum
{
     DEFAULT_STACK_SIZE = 1024 * 1024,   /* The default stack size for a newly created thread */
     SECONDS_PER_MINUTE = 60
};

typedef time_t sys_time_t;

enum sys_filetype_t
{
     SYS_FT_REG = 0,
     SYS_FT_DIR = 1,
     SYS_FT_CHR = 2,
     SYS_FT_BLK = 3,
     SYS_FT_FIFO = 4,
     SYS_FT_LNK = 5,
     SYS_FT_SOCK = 6,
     SYS_FT_UNKNOWN = 7
};

enum sys_file_attrs_t
{
     SYS_FATTR_NONE = 0x00,
     SYS_FATTR_TEMPORARY = 0x01,
     SYS_FATTR_ARCHIVE = 0x02,
     SYS_FATTR_OFFLINE = 0x04,
     SYS_FATTR_COMPRESSED = 0x08,
     SYS_FATTR_ENCRYPTED = 0x10,
     SYS_FATTR_HIDDEN = 0x20
};

// const uint64_t SYS_BLKSIZE_UNKNOWN = 0;

struct sys_stat_t
{
     enum sys_filetype_t _filetype;  /* type of the file */
     enum sys_file_attrs_t _attrs;   /* FAT-style file attributes. */
     uint64_t _mode;            /* unix-style permissions bits */

     uint64_t _size;            /* total size, in bytes */
     sys_time_t _atime;         /* time of last access */
     sys_time_t _mtime;         /* time of last modification */
     sys_time_t _ctime;         /* time of last status change */
};

struct sys_dirent_t
{
     uint64_t _ino;             /* inode number */
     enum sys_filetype_t _type;      /* type of file */
     char _name[SYS_NAME_MAX];  /* filename */
};


int debug_printf(const _TCHAR *, ...);

void sys_output_debug_string(const _TCHAR * str);
void sys_debug_break();
void *sys_get_stack_start();

#ifndef MIN2
#  define MIN2(x, y) ((x) < (y) ? (x) : (y))
#endif

#ifndef MAX2
#  define MAX2(x, y) ((x) > (y) ? (x) : (y))
#endif

double sys_realtime(void);
double sys_runtime(void);
double sys_time_resolution();
double sys_timezone_offset();

enum sys_retcode_t sys_init();

_TCHAR **sys_get_env_vars();
enum sys_retcode_t sys_setenv(_TCHAR * varname, _TCHAR * value);

enum sys_retcode_t sys_gethostname(_TCHAR * buf, size_t len);

enum sys_retcode_t sys_delete_file(_TCHAR * filename);
enum sys_retcode_t sys_temporary_filename(_TCHAR * prefix, _TCHAR * buf, size_t buflen);

enum sys_retcode_t sys_stat(const char *path, struct sys_stat_t * buf);

struct sys_dir_t;

enum sys_retcode_t sys_opendir(const char *path, struct sys_dir_t ** dir);
enum sys_retcode_t sys_readdir(struct sys_dir_t * dir, struct sys_dirent_t * ent, bool * done_p);
enum sys_retcode_t sys_closedir(struct sys_dir_t * dir);

enum sys_eoln_convention_t
{
     SYS_EOLN_CRLF = 0,         /* dos/windows */
     SYS_EOLN_CR = 1,           /* macintosh */
     SYS_EOLN_LF = 2            /* unix */
};

enum sys_eoln_convention_t sys_get_eoln_convention();

bool sys_get_fs_names_case_sensitive();

const _TCHAR *sys_get_platform_name();

extern uint8_t *stack_limit_obj;

# define STACK_CHECK(_obj)  if (((uint8_t *)_obj) < stack_limit_obj) vmerror_stack_overflow((uint8_t *) _obj);

void *sys_set_stack_limit(size_t new_size_limit);

/*** Timing ***/

void sys_sleep(uintptr_t duration_ms);

/*** String Utilities ***/
const _TCHAR *strchrnul(const _TCHAR * s, int c);

extern char **environ;

#endif                          /*  __SCAN_SYS_H */
