/* base-stdint.h
 * September 14th, 2006
 */

#ifndef __BASE_STDINT_H
#define __BASE_STDINT_H

#ifdef SCAN_UNIX
#  include <stdint.h>
#endif // SCAN_UNIX

#ifdef SCAN_WINDOWS

#  if defined(__MSC_VER)
#    include "chemeris-stdint.h"
#  endif

#  if defined(__GNUC__)
#    include <stdint.h>
#  endif


#endif // SCAN_WINDOWS

#endif // __UTIL_TCHAR
