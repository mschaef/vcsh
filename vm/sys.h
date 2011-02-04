
#ifndef __SYS_H
#define __SYS_H

#include <sys/time.h>
#include <limits.h>

#include "../util/base-types.h"
#include "../util/base-assert.h"
#include "../util/base-tchar.h"

#include "constants.h"

#ifdef SCAN_UNIX                /*  REVISIT: Can these ifdef's be removed? */
#  define SYS_PATH_MAX PATH_MAX
#  define SYS_NAME_MAX NAME_MAX
#endif

#ifdef SCAN_WINDOWS
#  define SYS_PATH_MAX _MAX_PATH
#  define SYS_NAME_MAX _MAX_PATH
#endif

BEGIN_NAMESPACE(scan)

void *safe_malloc(size_t size);
void safe_free(void *block);

enum
{

     DEFAULT_STACK_SIZE = 1024 * 1024,   /* The default stack size for a newly created thread */

     SECONDS_PER_MINUTE = 60    /* bar */
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

const u64_t SYS_BLKSIZE_UNKNOWN = 0;

struct sys_stat_t
{
     sys_filetype_t _filetype;  /* type of the file */
     sys_file_attrs_t _attrs;   /* FAT-style file attributes. */
     i64_t _mode;               /* unix-style permissions bits */

     i64_t _size;               /* total size, in bytes */
     sys_time_t _atime;         /* time of last access */
     sys_time_t _mtime;         /* time of last modification */
     sys_time_t _ctime;         /* time of last status change */
};

struct sys_dirent_t
{
     fixnum_t _ino;             /* inode number */
     sys_filetype_t _type;      /* type of file */
     char _name[SYS_NAME_MAX];  /* filename */
};

void output_debug_string(const _TCHAR * str);
void debug_break();
void *sys_get_stack_start();

#ifndef MIN2
#  define MIN2(x, y) ((x) < (y) ? (x) : (y))
#endif

#ifndef MAX2
#  define MAX2(x, y) ((x) > (y) ? (x) : (y))
#endif

flonum_t sys_realtime(void);
flonum_t sys_runtime(void);
flonum_t sys_time_resolution();
flonum_t sys_timezone_offset();
flonum_t sys_time_since_launch();

sys_retcode_t sys_init();

_TCHAR **sys_get_env_vars();
sys_retcode_t sys_setenv(_TCHAR * varname, _TCHAR * value);

sys_retcode_t sys_gethostname(_TCHAR * buf, size_t len);

sys_retcode_t sys_delete_file(_TCHAR * filename);
sys_retcode_t sys_temporary_filename(_TCHAR * prefix, _TCHAR * buf, size_t buflen);

sys_retcode_t sys_stat(const char *path, sys_stat_t * buf);

struct sys_dir_t;

sys_retcode_t sys_opendir(const char *path, sys_dir_t ** dir);
sys_retcode_t sys_readdir(sys_dir_t * dir, sys_dirent_t * ent, bool * done_p);
sys_retcode_t sys_closedir(sys_dir_t * dir);

enum sys_eoln_convention_t
{
     SYS_EOLN_CRLF = 0,         /* dos/windows */
     SYS_EOLN_CR = 1,           /* macintosh */
     SYS_EOLN_LF = 2            /* unix */
};

struct sys_info_t
{
     size_t _machine_bits;
     sys_eoln_convention_t _eoln;
     bool _fs_names_case_sensitive;
     const _TCHAR *_platform_name;
};

void sys_get_info(sys_info_t * info);

extern i64_t malloc_blocks;
extern i64_t malloc_bytes;

extern u8_t *stack_limit_obj;

# define STACK_CHECK(_obj)  if (((u8_t *)_obj) < stack_limit_obj) vmerror_stack_overflow((u8_t *) _obj);

void *sys_set_stack_limit(size_t new_size_limit);

/*** Timing ***/

void sys_sleep(uintptr_t duration_ms);

/*** String Utilities ***/
extern "C" const _TCHAR *strchrnul(const _TCHAR * s, int c);

END_NAMESPACE;

extern char **environ;

#endif                          /*  __SYS_H */
