
/*
 * malloc.cpp --
 *
 * A wrapper around malloc that adds some keeping and error checking.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>

#include "scan.h"

BEGIN_NAMESPACE(scan)

/**** The C Heap
 *
 * These functions wrap the C malloc/free allocator, to allow
 * for some optional detailed logging, and to guarantee they always
 * return. (This guarantee is made by terminating the process if the
 * allocation fails.)
 */
void *safe_malloc(size_t size)
{
     void *block = malloc(size ? size : 1);

     if (block == NULL)
     {
          _TCHAR buf[STACK_STRBUF_LEN];

          _sntprintf(buf, STACK_STRBUF_LEN, "Failed to allocate %zd bytes from system", size);
          panic(buf);
     }

     if (DEBUGGING_BUILD && DETAILED_MEMORY_LOG)
          debug_printf("\"a\", %d, , %d, %d\n", interp.gc_malloc_blocks, block, size);

     return block;
}

void safe_free(void *block)
{
     if (block == NULL)
          return;

     if (DEBUGGING_BUILD && DETAILED_MEMORY_LOG)
          debug_printf("\"d\", , , %d, \n", block);

     free(block);
}


END_NAMESPACE
