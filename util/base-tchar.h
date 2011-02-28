
/*
 * base-tchar.h --
 *
 * _TCHAR and associated definitions.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __BASE_TCHAR_H
#define __BASE_TCHAR_H

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
#  endif
#  if defined(__GNUC__)
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

#endif                          /* __BASE_TCHAR_H */
