
/* sys.h
 * Sometime in 2007
 *
 * System access routines
 */


#ifndef __SYS_H
#define __SYS_H

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdlib.h>

#include "../util/base-tchar.h"


#ifdef SCAN_UNIX                /*  REVISIT: Can these ifdef's be removed? */
#  define SYS_PATH_MAX PATH_MAX
#  define SYS_NAME_MAX NAME_MAX
#endif

#ifdef SCAN_WINDOWS
#  define SYS_PATH_MAX _MAX_PATH
#  define SYS_NAME_MAX _MAX_PATH
#endif

BEGIN_NAMESPACE(scan)
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

enum sys_retcode_t
{
     SYS_OK = 0,                /* Success */
     SYS_EPERM = 1,             /* Operation not permitted */
     SYS_ENOENT = 2,            /* No such file or directory */
     SYS_ESRCH = 3,             /* No such process */
     SYS_EINTR = 4,             /* Interrupted system call */
     SYS_EIO = 5,               /* I/O error */
     SYS_ENXIO = 6,             /* No such device or address */
     SYS_E2BIG = 7,             /* Argument list too long */
     SYS_ENOEXEC = 8,           /* Exec format error */
     SYS_EBADF = 9,             /* Bad file number */
     SYS_ECHILD = 10,           /* No child processes */
     SYS_EAGAIN = 11,           /* Try again */
     SYS_ENOMEM = 12,           /* Out of memory */
     SYS_EACCES = 13,           /* Permission denied */
     SYS_EFAULT = 14,           /* Bad address */
     SYS_ENOTBLK = 15,          /* Block device required */
     SYS_EBUSY = 16,            /* Device or resource busy */
     SYS_EEXIST = 17,           /* File exists */
     SYS_EXDEV = 18,            /* Cross-device link */
     SYS_ENODEV = 19,           /* No such device */
     SYS_ENOTDIR = 20,          /* Not a directory */
     SYS_EISDIR = 21,           /* Is a directory */
     SYS_EINVAL = 22,           /* Invalid argument */
     SYS_ENFILE = 23,           /* File table overflow */
     SYS_EMFILE = 24,           /* Too many open files */
     SYS_ENOTTY = 25,           /* Not a typewriter */
     SYS_ETXTBSY = 26,          /* Text file busy */
     SYS_EFBIG = 27,            /* File too large */
     SYS_ENOSPC = 28,           /* No space left on device */
     SYS_ESPIPE = 29,           /* Illegal seek */
     SYS_EROFS = 30,            /* Read-only file system */
     SYS_EMLINK = 31,           /* Too many links */
     SYS_EPIPE = 32,            /* Broken pipe */
     SYS_EDOM = 33,             /* Math argument out of domain of func */
     SYS_ERANGE = 34,           /* Math result not representable */
     SYS_EDEADLK = 35,          /* Resource deadlock would occur */
     SYS_ENAMETOOLONG = 36,     /* File name too long */
     SYS_ENOLCK = 37,           /* No record locks available */
     SYS_ENOSYS = 38,           /* Function not implemented */
     SYS_ENOTEMPTY = 39,        /* Directory not empty */
     SYS_ELOOP = 40,            /* Too many symbolic links encountered */
     SYS_EWOULDBLOCK = SYS_EAGAIN,      /* Operation would block */
     SYS_ENOMSG = 42,           /* No message of desired type */
     SYS_EIDRM = 43,            /* Identifier removed */
     SYS_ECHRNG = 44,           /* Channel number out of range */
     SYS_EL2NSYNC = 45,         /* Level 2 not synchronized */
     SYS_EL3HLT = 46,           /* Level 3 halted */
     SYS_EL3RST = 47,           /* Level 3 reset */
     SYS_ELNRNG = 48,           /* Link number out of range */
     SYS_EUNATCH = 49,          /* Protocol driver not attached */
     SYS_ENOCSI = 50,           /* No CSI structure available */
     SYS_EL2HLT = 51,           /* Level 2 halted */
     SYS_EBADE = 52,            /* Invalid exchange */
     SYS_EBADR = 53,            /* Invalid request descriptor */
     SYS_EXFULL = 54,           /* Exchange full */
     SYS_ENOANO = 55,           /* No anode */
     SYS_EBADRQC = 56,          /* Invalid request code */
     SYS_EBADSLT = 57,          /* Invalid slot */
     SYS_EDEADLOCK = SYS_EDEADLK,
     SYS_EBFONT = 59,           /* Bad font file format */
     SYS_ENOSTR = 60,           /* Device not a stream */
     SYS_ENODATA = 61,          /* No data available */
     SYS_ETIME = 62,            /* Timer expired */
     SYS_ENOSR = 63,            /* Out of streams resources */
     SYS_ENONET = 64,           /* Machine is not on the network */
     SYS_ENOPKG = 65,           /* Package not installed */
     SYS_EREMOTE = 66,          /* Object is remote */
     SYS_ENOLINK = 67,          /* Link has been severed */
     SYS_EADV = 68,             /* Advertise error */
     SYS_ESRMNT = 69,           /* Srmount error */
     SYS_ECOMM = 70,            /* Communication error on send */
     SYS_EPROTO = 71,           /* Protocol error */
     SYS_EMULTIHOP = 72,        /* Multihop attempted */
     SYS_EDOTDOT = 73,          /* RFS specific error */
     SYS_EBADMSG = 74,          /* Not a data message */
     SYS_EOVERFLOW = 75,        /* Value too large for defined data type */
     SYS_ENOTUNIQ = 76,         /* Name not unique on network */
     SYS_EBADFD = 77,           /* File descriptor in bad state */
     SYS_EREMCHG = 78,          /* Remote address changed */
     SYS_ELIBACC = 79,          /* Can not access a needed shared library */
     SYS_ELIBBAD = 80,          /* Accessing a corrupted shared library */
     SYS_ELIBSCN = 81,          /* .lib section in a.out corrupted */
     SYS_ELIBMAX = 82,          /* Attempting to link in too many shared libraries */
     SYS_ELIBEXEC = 83,         /* Cannot exec a shared library directly */
     SYS_EILSEQ = 84,           /* Illegal byte sequence */
     SYS_ERESTART = 85,         /* Interrupted system call should be restarted */
     SYS_ESTRPIPE = 86,         /* Streams pipe error */
     SYS_EUSERS = 87,           /* Too many users */
     SYS_ENOTSOCK = 88,         /* Socket operation on non-socket */
     SYS_EDESTADDRREQ = 89,     /* Destination address required */
     SYS_EMSGSIZE = 90,         /* Message too long */
     SYS_EPROTOTYPE = 91,       /* Protocol wrong type for socket */
     SYS_ENOPROTOOPT = 92,      /* Protocol not available */
     SYS_EPROTONOSUPPORT = 93,  /* Protocol not supported */
     SYS_ESOCKTNOSUPPORT = 94,  /* Socket type not supported */
     SYS_EOPNOTSUPP = 95,       /* Operation not supported on transport endpoint */
     SYS_EPFNOSUPPORT = 96,     /* Protocol family not supported */
     SYS_EAFNOSUPPORT = 97,     /* Address family not supported by protocol */
     SYS_EADDRINUSE = 98,       /* Address already in use */
     SYS_EADDRNOTAVAIL = 99,    /* Cannot assign requested address */
     SYS_ENETDOWN = 100,        /* Network is down */
     SYS_ENETUNREACH = 101,     /* Network is unreachable */
     SYS_ENETRESET = 102,       /* Network dropped connection because of reset */
     SYS_ECONNABORTED = 103,    /* Software caused connection abort */
     SYS_ECONNRESET = 104,      /* Connection reset by peer */
     SYS_ENOBUFS = 105,         /* No buffer space available */
     SYS_EISCONN = 106,         /* Transport endpoint is already connected */
     SYS_ENOTCONN = 107,        /* Transport endpoint is not connected */
     SYS_ESHUTDOWN = 108,       /* Cannot send after transport endpoint shutdown */
     SYS_ETOOMANYREFS = 109,    /* Too many references: cannot splice */
     SYS_ETIMEDOUT = 110,       /* Connection timed out */
     SYS_ECONNREFUSED = 111,    /* Connection refused */
     SYS_EHOSTDOWN = 112,       /* Host is down */
     SYS_EHOSTUNREACH = 113,    /* No route to host */
     SYS_EALREADY = 114,        /* Operation already in progress */
     SYS_EINPROGRESS = 115,     /* Operation now in progress */
     SYS_ESTALE = 116,          /* Stale NFS file handle */
     SYS_EUCLEAN = 117,         /* Structure needs cleaning */
     SYS_ENOTNAM = 118,         /* Not a XENIX named type file */
     SYS_ENAVAIL = 119,         /* No XENIX semaphores available */
     SYS_EISNAM = 120,          /* Is a named type file */
     SYS_EREMOTEIO = 121,       /* Remote I/O error */
     SYS_EDQUOT = 122,          /* Quota exceeded */
     SYS_ENOMEDIUM = 123,       /* No medium found */
     SYS_EMEDIUMTYPE = 124,     /* Wrong medium type */
     SYS_ECANCELED = 125,       /* Operation Canceled */
     SYS_ENOKEY = 126,          /* Required key not available */
     SYS_EKEYEXPIRED = 127,     /* Key has expired */
     SYS_EKEYREVOKED = 128,     /* Key has been revoked */
     SYS_EKEYREJECTED = 129,    /* Key was rejected by service */

     /* for robust mutexes */
     SYS_EOWNERDEAD = 130,      /* Owner died */
     SYS_ENOTRECOVERABLE = 131, /* State not recoverable */

     SYS_EWIERD = 0xFFFF        /* Unexpected return code. */
};

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
