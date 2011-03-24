
/*
 * scan-base.h --
 *
 * Standard types and definitions.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __SCAN_BASE_H
#define __SCAN_BASE_H

#include <stddef.h>
#include <limits.h>
#include <ctype.h>

#if defined(SCAN_UNIX)
#  include <stdint.h>
#elif defined(SCAN_WINDOWS)
#  if defined(_MSC_VER)
#    define __STDC_LIMIT_MACROS
#    define __STDC_CONSTANT_MACROS
#    include "chemeris-stdint.h"
#  endif
#  if defined(__GNUC__)
#    include <stdint.h>
#  endif
#else
#  error Either SCAN_WINDOWS or SCAN_UNIX must be defined to pick a platform.
#endif

#if (defined(__GNUC__) && defined(__LP64__)) || (defined(_MSC_VER) && (_INTEGRAL_MAX_BITS == 64))
#   define SCAN_WORDSIZE 64
#else
#   define SCAN_WORDSIZE 32
#endif

/* Macros for denoting C++ namespaces
 *
 * Add macros for beginning and ending C++ namespaces. These are used to keep
 * namespace blocks from confusing indent. If the namespace declarations are
 * there, indent doesn't recognize function declarations as function
 * declarations.
 */
#define BEGIN_NAMESPACE(name) namespace name {
#define END_NAMESPACE }

/*** A macro that allow a variable to be denoted as unreferenced. ***/

#define UNREFERENCED(x) ((void)x)

/*** Definitions for inlining ***/

#if defined(__GNUC__)
#  define INLINE inline __attribute__((always_inline))
#elif defined(_MSC_VER)
#  define INLINE __forceinline
#endif

/*** TRUE and FALSE ***/

#ifndef TRUE
#   define TRUE   (1==1)
#endif

#ifndef FALSE
#   define FALSE (!TRUE)
#endif

/*** Build type flags ***/

#ifdef _MSC_VER
#  pragma warning (disable : 4127)      /* ...warning about the constants used to configure the build. */
#  pragma warning (disable : 4820)      /* ...warning about structure padding */
#  pragma warning (disable : 4061)      /* ...warning about enumerations unhandled by explicit case */
#endif

enum {
#ifdef _DEBUG
     DEBUGGING_BUILD = TRUE,
#else
     DEBUGGING_BUILD = FALSE,
#endif

#ifdef CHECKED
     CHECKED_BUILD = TRUE,
#else
     CHECKED_BUILD = FALSE,
#endif
};

/*** 64-bit integer support ***/

#if defined(__GNUC__)
#   define PRINTF_PREFIX_INT64 "ll"
#   define PRINTF_PREFIX_SIZE_T "z"
#elif defined(_MSC_VER)
#   define PRINTF_PREFIX_INT64 "I64"
#   define PRINTF_PREFIX_SIZE_T "I"
#endif

extern "C" INLINE int64_t make_int64_t(int64_t high, int64_t low)
{
     return ((int64_t) high << 32) + (int64_t) low;
}

extern "C" INLINE uint64_t make_uint64_t(uint64_t high, uint64_t low)
{
     return ((uint64_t) high << 32) + (uint64_t) low;
}

#if !defined(__GNUC__)
#   define strtoll _strtoi64
#endif

/*** Minimum and Maximum ***/

#ifndef MIN2
#   define MIN2(x, y) ((x) < (y) ? (x) : (y))
#endif

#ifndef MAX2
#   define MAX2(x, y) ((x) > (y) ? (x) : (y))
#endif

/*** Floating point aliases needed in MSVC ***/

#if defined(_MSC_VER)
#  include <float.h>

#  define finite _finite
#  define isnan _isnan
#  define ecvt _ecvt
#endif

/*** strncasecmp ***/

#if defined(_MSC_VER)
#  define strncasecmp _strnicmp
#endif


#ifdef SCAN_UNIX
#  include <strings.h>
#endif


/*** TCHAR ***/

/* Couldn't find a better definition for these in the standard header
 * files... */
#ifdef _UNICODE
#   define _TCHAR_MIN WCHAR_MIN
#   define _TCHAR_MAX WCHAR_MAX
#else
#   define _TCHAR_MIN CHAR_MIN
#   define _TCHAR_MAX CHAR_MAX
#endif


#ifdef SCAN_UNIX

#  ifdef _UNICODE
#    error Unicode unsupported on GNU C
#  endif

typedef char _TCHAR;

#  define _T(x)     x

#  define _vsntprintf  vsnprintf
#  define _sntprintf   snprintf

#  define _tcslen strlen
#  define _tcscmp strcmp
#  define _tcsncpy strncpy
#  define _tcsncat strncat

#  define _istupper isupper
#  define _istlower islower
#  define _istdigit isdigit
#  define _totlower tolower
#  define _istspace isspace
#  define _istalpha isalpha
#  define _istpunct ispunct
#  define _totupper toupper
#  define _stprintf sprintf
#  define _tprintf  printf

#  define _tmain main
#endif                          /* SCAN_UNIX */

#ifdef SCAN_WINDOWS
#  if defined(_MSC_VER)
#    include <tchar.h>
#  if defined(__GNUC__)
#  endif
#    include "tchar.h"
#    if !defined( __TEXT)
#      if defined(_UNICODE)
#        define __TEXT(string) L##string
#      else
#        define __TEXT(string) string
#      endif
#    endif
#  endif
#endif                          /* SCAN_WINDOWS */


#endif
