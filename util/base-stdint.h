
/*
 * base-stdint.h --
 *
 * A standard way to get access to stdint.h.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __BASE_STDINT_H
#define __BASE_STDINT_H

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

#endif                          /* __BASE_STDINT_H */
