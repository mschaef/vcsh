
/*
 * scan-internal-file.h --
 *
 * The declarations needed for internal files.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-base.h"

#ifndef __SCAN_INTERNAL_FILE_H
#define __SCAN_INTERNAL_FILE_H

/* An internal file is a block of read-only data linked into the
 * executable, usually a compiled scheme image. to-c-source generates
 * one as a separate const byte array plus an internal_file_t that
 * points at it. */

struct internal_file_t
{
     const _TCHAR *_name;
     size_t _length;
     const uint8_t *_bytes;
};

#define DECL_INTERNAL_FILE struct internal_file_t

#endif
