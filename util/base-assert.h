
/*
 * base-assert.h --
 *
 * Standard assertations.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __BASE_ASSERT_H
#define __BASE_ASSERT_H

#include <assert.h>
#include "base-tchar.h"

BEGIN_NAMESPACE(scan)
typedef void (*panic_handler_t) (void);

panic_handler_t set_panic_handler(panic_handler_t new_handler);

void _panic(const _TCHAR * str, const _TCHAR * filename, long lineno);

#define panic(str) scan::_panic(str, __FILE__, __LINE__)

#ifdef CHECKED
#	define checked_assert(exp) assert(exp)
#else
#	define checked_assert(exp)
#endif

END_NAMESPACE
#endif                          /* __BASE_ASSERT_H */
