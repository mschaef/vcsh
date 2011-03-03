
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

/* REVISIT: Is there not a better place to store malloc_blocks and malloc_bytes? */
int64_t malloc_blocks = 0;
int64_t malloc_bytes = 0;         /*  REVISIT: have malloc set flag based on user defined limit? use that to trigger GC? */

/**** The C Heap
 *
 * These functions wrap the C malloc/free allocator, and
 * do a couple bits of additional accounting in debug builds.
 * Each block is prefixed with a u32 containing the size of
 * the block. This is used to track blocks allocated and freed
 * and bytes allocated and freed.
 */
void *safe_malloc(size_t size)
{
     void *block;

     if (DEBUGGING_BUILD)
     {
          size += sizeof(size_t);
     }

     malloc_blocks++;
     malloc_bytes += size;

     block = (_TCHAR *) malloc((size) ? size : 1);

     if (block == (void *) NULL)
     {
          _TCHAR buf[STACK_STRBUF_LEN];

          _sntprintf(buf, STACK_STRBUF_LEN, "failed to allocate %zd bytes from system", size);
          panic(buf);
     }

     if (DEBUGGING_BUILD && DETAILED_MEMORY_LOG)
     {
          debug_printf("\"a\", %d, , %d, %d\n", malloc_blocks, block, size);
     }


     if (DEBUGGING_BUILD)
     {
          size_t *tmp_block = (size_t *) block;
          *tmp_block = (size - 4);
          tmp_block += 1;
          block = (void *) tmp_block;
     }

     return (block);
}

void safe_free(void *block)
{
     if (block == NULL)
          return;

     if (DEBUGGING_BUILD)
     {
          size_t *tmp_block;

          tmp_block = (size_t *) block;
          tmp_block -= 1;
          block = (void *) tmp_block;

          if (DETAILED_MEMORY_LOG)
               debug_printf("\"d\", , , %d, \n", block);
     }

     free(block);
}


END_NAMESPACE
