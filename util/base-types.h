
/* base-types.h
 * August 14th, 2006
 * 
 * Standard types
 */

#ifndef __BASE_TYPES_H
#define __BASE_TYPES_H

#include <stddef.h>
#include <limits.h>

#include "base-stdint.h"
#include "base-tchar.h"

/*** Definitions for inlining ***/

#if defined(__GNUC__)
#  define INLINE inline __attribute__((always_inline))
#elif defined(_MSC_VER)
#  define INLINE __forceinline
#endif

/*** Standard, useful types ***/

/* *INDENT-OFF* */
#if defined(__GNUC__)
typedef unsigned long long int u64_t;
#elif defined(_MSC_VER)
typedef unsigned __int64       u64_t;
#endif
typedef unsigned int           u32_t;
typedef unsigned short         u16_t;
typedef unsigned char          u8_t;
typedef uintptr_t              uptr_t;
typedef u32_t                  usys_t;  /* The 'natural' size for integers on the platform */

#if defined(__GNUC__)
typedef long long int          i64_t;
#elif defined(_MSC_VER)
typedef __int64                i64_t;
#endif
typedef int                    i32_t;
typedef short                  i16_t;
typedef signed char            i8_t;
typedef intptr_t               iptr_t;
typedef i32_t                  isys_t;  /* The 'natural' size for integers on the platform */
typedef float                  f32_t;
typedef double                 f64_t;
/* *INDENT-ON* */

#ifndef TRUE
#   define TRUE   (1==1)
#endif

#ifndef FALSE
#   define FALSE (!TRUE)
#endif


#define I8_MIN SCHAR_MIN
#define I8_MAX SCHAR_MAX
#define U8_MIN 0
#define U8_MAX UCHAR_MAX

#define I16_MIN SHRT_MIN
#define I16_MAX SHRT_MAX
#define U16_MIN 0
#define U16_MAX USHRT_MAX

#define I32_MIN INT_MIN
#define I32_MAX INT_MAX
#define U32_MIN 0
#define U32_MAX UINT_MAX

#if defined(__GNUC__)
#   define I64_MAX LONG_LONG_MAX
#   define I64_MIN LONG_LONG_MIN
#   define I64(val) (val##LL)
#   define U64_MAX ULONG_LONG_MAX
#   define U64_MIN (0ULL)
#   define U64(val) (val##ULL)
#   define INT64_PRINTF_PREFIX "%ll"
#   define SIZE_T_PRINTF_PREFIX "%zi"
#elif defined(_MSC_VER)
#   define I64_MAX _I64_MAX
#   define I64_MIN _I64_MIN
#   define I64(val) (val##i64)
#   define U64_MAX _UI64_MAX
#   define U64_MIN (0ui64)
#   define U64(val) (val##ui64)
#   define INT64_PRINTF_PREFIX "%I64"
#   define SIZE_T_PRINTF_PREFIX "%u"
#endif

/* Couldn't find a better definition for these in the standard header files... */
#ifdef _UNICODE
#   define _TCHAR_MIN WCHAR_MIN
#   define _TCHAR_MAX WCHAR_MAX
#else
#   define _TCHAR_MIN CHAR_MIN
#   define _TCHAR_MAX CHAR_MAX
#endif

#if !defined(__GNUC__)
#   define strtoll _strtoi64
#endif


/* Declare a variable otherwise unreferenced... */
#define UNREFERENCED(x) ((void)x)

#ifdef _MSC_VER
#  pragma warning (disable : 4127)      /* ...warning about the constants used to configure the build. */
#  pragma warning (disable : 4820)      /* ...warning about structure padding */
#  pragma warning (disable : 4061)      /* ...warning about enumerations unhandled by explicit case */
#endif

#ifdef _DEBUG
enum
{ DEBUGGING_BUILD = TRUE };
#   define REFERENCED_BY_DEBUG_BUILD(x)
#else
enum
{ DEBUGGING_BUILD = FALSE };
#   define REFERENCED_BY_DEBUG_BUILD(x) UNREFERENCED(x)
#endif


#ifdef CHECKED
enum
{ CHECKED_BUILD = TRUE };
#   define REFERENCED_BY_CHECKED_BUILD(x)
#else
enum
{ CHECKED_BUILD = FALSE };
#   define REFERENCED_BY_CHECKED_BUILD(x) UNREFERENCED(x)
#endif

/*** Minimum and Maximum ***/

#ifndef MIN2
#   define MIN2(x, y) ((x) < (y) ? (x) : (y))
#endif

#ifndef MAX2
#   define MAX2(x, y) ((x) > (y) ? (x) : (y))
#endif

#if defined(_MSC_VER)
#  include <float.h>

#  define finite _finite
#  define isnan _isnan
#  define strncasecmp _strnicmp
#  define ecvt _ecvt
#endif

/*** Interpreter specific types ***/

#define FIXNUM_64BIT            /* Support for MSC style 64-bit integers */

/*** Global data types ***/

#ifdef FIXNUM_64BIT
typedef i64_t fixnum_t;
typedef u64_t unsigned_fixnum_t;

#   define FIXNUM_BITS (64)
#   define FIXNUM_MAX           I64_MAX
#   define FIXNUM_MIN           I64_MIN
#   define FIXNUM_UNSIGNED_MAX  U64_MAX
#   define FIXNUM_UNSIGNED_MIN  U64_MIN
#   define FIXNUM_PRINTF_PREFIX  INT64_PRINTF_PREFIX

#else
typedef i32 fixnum_t;
typedef u32 unsigned_fixnum_t;

#   define FIXNUM_BITS (32)
#   define FIXNUM_MAX           I32_MAX
#   define FIXNUM_MIN           I32_MIN
#   define FIXNUM_UNSIGNED_MAX  U32_MAX
#   define FIXNUM_UNSIGNED_MIN  U32_MIN
#   define FIXNUM_PRINTF_PREFIX    "%"
#endif

typedef double flonum_t;

#define FLONUM_MAX DBL_MAX
#define FLONUM_MIN -DBL_MAX
#define FLONUM_EPSILON DBL_EPSILON

/* Add macros for beginning and ending C++ namespaces. These are used to keep
 * namespace blocks from confusing indent. If the namespace declarations are
 * there, indent doesn't recognize function declarations as function declarations. */
#define BEGIN_NAMESPACE(name) namespace name {
#define END_NAMESPACE }

extern "C" int debug_printf(const _TCHAR *, ...);

extern "C" INLINE i64_t make_i64(i64_t high, i64_t low)
{
     return ((i64_t) high << 32) + (i64_t) low;
}

extern "C" INLINE u64_t make_u64_t(u64_t high, u64_t low)
{
     return ((u64_t) high << 32) + (u64_t) low;
}

/* Microsoft C and gcc appear to have differing opinions on how to
 * initialize a structure with an indefinate sized array at the end. */

#if defined(_MSC_VER)
typedef u8_t data_block_data_t[];
#define DATA_BLOCK_DATA_CAST
#else
typedef u8_t *data_block_data_t;
#define DATA_BLOCK_DATA_CAST (u8_t [])
typedef unsigned long          size_t;
#endif

struct data_block_t
{
     size_t _length;    
     data_block_data_t _bytes;
};

#endif
