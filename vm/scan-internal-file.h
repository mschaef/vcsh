
/*
 * scan-internal-file.h --
 *
 * The declarations needed for internal files.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-base.h"

#ifndef __SCAN_INTERNAL_FILE_H
#define __SCAN_INTERNAL_FILE_H

BEGIN_NAMESPACE(scan)

/* Microsoft C and gcc appear to have differing opinions on how to
 * initialize a structure with an indefinate sized array at the end. */

#if defined(_MSC_VER)

typedef uint8_t internal_file_data_t[];
#  define INTERNAL_FILE_DATA_CAST

#else

typedef uint8_t *internal_file_data_t;
#  define INTERNAL_FILE_DATA_CAST (uint8_t [])

#endif

#ifdef SCAN_WINDOWS
#  pragma warning (push)
#  pragma warning (disable: 4200)
#endif

struct internal_file_t
{
     _TCHAR *_name;
     size_t _length;
     internal_file_data_t _bytes;
};

#define DECL_INTERNAL_FILE struct scan::internal_file_t

#ifdef SCAN_WINDOWS
#  pragma warning (pop)
#endif

END_NAMESPACE

#endif
