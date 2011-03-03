
/*
 * base-types.h --
 *
 * Standard types.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __BASE_TYPES_H
#define __BASE_TYPES_H

#include <stddef.h>
#include <limits.h>

#ifdef SCAN_UNIX
#  include <stdint.h>
#endif
#ifdef SCAN_WINDOWS
#  if defined(__MSC_VER)
#    include "chemeris-stdint.h"
#  endif
#  if defined(__GNUC__)
#    include <stdint.h>
#  endif
#endif


#include "base-tchar.h"

/*** Definitions for inlining ***/

#if defined(__GNUC__)
#  define INLINE inline __attribute__((always_inline))
#elif defined(_MSC_VER)
#  define INLINE __forceinline
#endif

/*** Standard, useful types ***/

#ifndef TRUE
#   define TRUE   (1==1)
#endif

#ifndef FALSE
#   define FALSE (!TRUE)
#endif

#if defined(__GNUC__)
#   define INT64_PRINTF_PREFIX "ll"
#   define SIZE_T_PRINTF_PREFIX "z"
#elif defined(_MSC_VER)
#   define INT64_PRINTF_PREFIX "I64"
#   define SIZE_T_PRINTF_PREFIX "I"
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

#ifdef SCAN_UNIX
#  include <strings.h>
#endif

/*** Interpreter specific types ***/

#define FIXNUM_64BIT            /* Support for MSC style 64-bit integers */

/*** Global data types ***/

#ifdef FIXNUM_64BIT
typedef int64_t fixnum_t;
typedef uint64_t unsigned_fixnum_t;

#   define FIXNUM_BITS (64)
#   define FIXNUM_MAX           INT64_MAX
#   define FIXNUM_MIN           INT64_MIN
#   define FIXNUM_UNSIGNED_MAX  UINT64_MAX
#   define FIXNUM_UNSIGNED_MIN  UINT64_MIN
#   define FIXNUM_PRINTF_PREFIX  INT64_PRINTF_PREFIX

#else
typedef int32_t fixnum_t;
typedef uint32_t unsigned_fixnum_t;

#   define FIXNUM_BITS (32)
#   define FIXNUM_MAX           INT32_MAX
#   define FIXNUM_MIN           INT32_MIN
#   define FIXNUM_UNSIGNED_MAX  UINT32_MAX
#   define FIXNUM_UNSIGNED_MIN  UINT32_MIN
#   define FIXNUM_PRINTF_PREFIX    ""
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

extern "C" INLINE int64_t make_i64(int64_t high, int64_t low)
{
     return ((int64_t) high << 32) + (int64_t) low;
}

extern "C" INLINE uint64_t make_uint64_t(uint64_t high, uint64_t low)
{
     return ((uint64_t) high << 32) + (uint64_t) low;
}

/* Microsoft C and gcc appear to have differing opinions on how to
 * initialize a structure with an indefinate sized array at the end. */

#if defined(_MSC_VER)

typedef uint8_t data_block_data_t[];
#  define DATA_BLOCK_DATA_CAST

#else

typedef uint8_t *data_block_data_t;
#  define DATA_BLOCK_DATA_CAST (uint8_t [])

#endif

#ifdef SCAN_WINDOWS
#  pragma warning (push)
#  pragma warning (disable: 4200)
#endif

struct data_block_t
{
     size_t _length;
     data_block_data_t _bytes;
};

#ifdef SCAN_WINDOWS
#  pragma warning (pop)
#endif

#endif
