/* base-assert.h
 * October 17th, 2003
 * 
 * Standard assertations
 */

#ifndef __BASE_ASSERT_H
#define __BASE_ASSERT_H

#include <assert.h>
#include "base-tchar.h"

BEGIN_NAMESPACE(scan)

typedef void (*panic_handler_t)(void);

panic_handler_t set_panic_handler(panic_handler_t new_handler);

void _panic(const _TCHAR *str, const _TCHAR *filename, long lineno);

#define panic(str) scan::_panic(str, __FILE__, __LINE__)

#ifdef CHECKED
#	define checked_assert(exp) assert(exp)
#else
#	define checked_assert(exp) 
#endif

END_NAMESPACE

#endif /* __ASSERT_H */
