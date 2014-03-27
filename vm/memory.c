
/*
 * memory.cpp --
 *
 * Garbage collected heap management. The GC heap is a heap of lobject_t's
 * managed by a conservative mark and sweep  garbage collector.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

#include <stdlib.h>


/**** The C Heap
 *
 * These functions wrap the C malloc/free allocator, to provide a few
 * useful bits of additional functionality:
 *
 * 1) Optional detailed alloc/free logging.
 * 2) Guaranteed to always return. (They terminate the process
 *    on allocation failures.)
 * 3) Maintenance of allocation counters for GC support.
 */
void *gc_malloc(size_t size)
{
     if (size == 0)
          size = 1;

     void *block = malloc(size);

     if (block == NULL)
     {
          _TCHAR buf[STACK_STRBUF_LEN];

          _sntprintf(buf, STACK_STRBUF_LEN,
                     "Failed to allocate %zd bytes from system", size);

          panic(buf);
     }

     if (DEBUGGING_BUILD && DETAILED_MEMORY_LOG)
          debug_printf("\"a\", %d, , %d, %d\n", interp.gc_malloc_blocks, block, size);

     interp.gc_malloc_blocks += 1;
     interp.gc_malloc_bytes += size;

     return block;
}

void gc_free(void *block)
{
     if (block == NULL)
          return;

     if (DEBUGGING_BUILD && DETAILED_MEMORY_LOG)
          debug_printf("\"d\", , , %d, \n", block);

     free(block);
}

/*** The GC Timer ***/

static void gc_begin_timer()
{
     assert((interp.gc_start_time == 0.0)
            && "Cannot recursively enter the GC timer.");

     interp.gc_start_time = sys_runtime();
}

static double gc_end_timer()
{
     assert((interp.gc_start_time > 0.0)
            && "The GC timer must have been begun to be ended.");

     double gc_run_time = sys_runtime() - interp.gc_start_time;

     interp.gc_start_time = 0.0;

     interp.gc_total_run_time += gc_run_time;

     return gc_run_time;
}


/*** GC Root Registry ***/

static size_t gc_find_free_root()
{
     for (size_t root_index = 0; root_index < MAX_GC_ROOTS; root_index++)
     {
          if (CURRENT_TIB()->gc_roots[root_index].name == NULL)
               return root_index;
     }

     assert(!"Could not find free GC root.");

     // not reached

     return -1;
}

void gc_protect(const _TCHAR * name, lref_t * location, size_t n)
{
     size_t root_index = gc_find_free_root();

     struct gc_root_t *root =
          (struct gc_root_t *) & (CURRENT_TIB()->gc_roots[root_index]);

     root->name = name;
     root->location = location;
     root->length = n;
}

static void gc_init_cell(lref_t obj)
{
     SET_TYPE(obj, TC_FREE_CELL);
     SET_GC_MARK(obj, 0);
}

static int gc_sub_freelist_length(lref_t current_freelist)
{
     int len = 0;

     for (lref_t cell = current_freelist;
          cell != NULL;
          cell = NEXT_FREE_CELL(cell))
          len++;

     return len;
}

void gc_dump_freelists()
{
     for (lref_t flist = interp.gc_global_freelist;
          flist != NULL;
          flist = NEXT_FREE_LIST(flist))
     {
          dscwritef(DF_ALWAYS, ("{~c&:~cd}",
                                flist, gc_sub_freelist_length(flist)));
     }

     dscwritef(DF_ALWAYS, ("\n"));
}

static size_t gc_heap_freelist_length(void)
{
     size_t count = 0;

     for (lref_t flist = interp.gc_global_freelist;
          flist != NULL;
          flist = NEXT_FREE_LIST(flist))
          count++;

     return count;
}

/*** The heap segment allocator
 *
 * The GC heap is maintained as a variable sized array of heap segments. The VM
 * starts out with one subheap, and will allocate up to as many as
 * HEAP_SEGMENT_LIMIT heaps, on an as-needed basis.
 */

static void gc_init_heap_segment(lref_t seg_base)
{
     lref_t current_sub_freelist = NIL;
     size_t current_sub_freelist_size = 0;

     for (size_t ofs = 0; ofs < interp.gc_heap_segment_size; ofs++)
     {
          lref_t cell = &seg_base[ofs];

          gc_init_cell(cell);

          SET_NEXT_FREE_CELL(cell, current_sub_freelist);
          current_sub_freelist = cell;

          current_sub_freelist_size++;

          if (current_sub_freelist_size >= SUB_FREELIST_SIZE)
          {
               interp.gc_global_freelist =
                    SET_NEXT_FREE_LIST(current_sub_freelist,
                                       interp.gc_global_freelist);

               current_sub_freelist_size = 0;
               current_sub_freelist = NIL;
          }
     }

     if (!NULLP(current_sub_freelist))
          interp.gc_global_freelist = SET_NEXT_FREE_LIST(current_sub_freelist,
                                                         interp.gc_global_freelist);
}

static bool gc_enlarge_heap()
{
     dscwritef(DF_SHOW_GC_DETAILS, (";;; attempting to enlarge heap\n"));

     if (interp.gc_current_heap_segments > interp.gc_max_heap_segments)
     {
          dscwritef(DF_SHOW_GC_DETAILS,
                    (";;; HEAP ENLARGE FAILED! Too many segments.\n"));
          return false;
     }

     gc_begin_timer();

     lref_t seg_base = gc_malloc(sizeof(struct lobject_t) * interp.gc_heap_segment_size);

     size_t seg_idx = interp.gc_current_heap_segments;

     interp.gc_current_heap_segments++;

     interp.gc_malloc_bytes_threshold += (sizeof(struct lobject_t) * interp.gc_heap_segment_size);

     interp.gc_heap_segments[seg_idx] = seg_base;

     gc_init_heap_segment(seg_base);

     gc_end_timer();

     return true;
}

/*** The Mark-and-Sweep garbage collection algorithm ***/

/* possible_heap_pointer_p
 *
 * Heuristic used to determine if a value is conceivably a pointer.
 */
static bool gc_possible_heap_pointer_p(lref_t p)
{
     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
     {
          lref_t h = interp.gc_heap_segments[jj];

          /*  Skip unallocated gc_heap_segments; */
          if (h == NULL)
               continue;

          /*  Pointers point into gc_heap_segments */
          if ((p < h) || (p >= (h + interp.gc_heap_segment_size)))
               continue;

          /*  Pointers are aligned at lobject_t boundaries */
          if (((((uint8_t *) p) - ((uint8_t *) h)) % sizeof(struct lobject_t)) != 0)
               continue;

          /*  Pointers have types */
          if (TYPEP(p, TC_FREE_CELL))
               continue;

          return true;
     }

     return false;
}


/* gc_mark
 *
 * Mark an object and its descendants as being reachable. */
void gc_mark(lref_t initial_obj)
{
     lref_t obj = initial_obj;

     while (!NULLP(obj) && !LREF_IMMEDIATE_P(obj) && !GC_MARK(obj))
     {
          SET_GC_MARK(obj, 1);

          switch (TYPE(obj))
          {
          case TC_CONS:
               gc_mark(CAR(obj));

               obj = CDR(obj);
               break;

          case TC_SYMBOL:
               gc_mark((*obj).storage_as.symbol.props);
               obj = SYMBOL_VCELL(obj);
               break;

          case TC_PACKAGE:
               gc_mark(PACKAGE_BINDINGS(obj));
               gc_mark(PACKAGE_USE_LIST(obj));

               obj = PACKAGE_NAME(obj);
               break;

          case TC_CLOSURE:
               gc_mark(CLOSURE_CODE(obj));
               gc_mark(CLOSURE_PROPERTY_LIST(obj));

               obj = CLOSURE_ENV(obj);
               break;

          case TC_MACRO:
               obj = MACRO_TRANSFORMER(obj);
               break;

          case TC_FLONUM:
               obj = FLOIM(obj);
               break;

          case TC_SUBR:
               obj = SUBR_NAME(obj);
               break;

          case TC_HASH:
               for (size_t jj = 0; jj < HASH_SIZE(obj); ++jj)
               {
                    gc_mark(HASH_DATA(obj)[jj].key);
                    gc_mark(HASH_DATA(obj)[jj].val);
               }

               obj = NIL;
               break;

          case TC_PORT:
               obj = port_gc_mark(obj);
               break;

          case TC_VECTOR:
               for (size_t jj = 0; jj < VECTOR_DIM(obj); ++jj)
                    gc_mark(VECTOR_ELEM(obj, jj));

               obj = NIL;
               break;

          case TC_STRUCTURE:
               for (size_t jj = 0; jj < STRUCTURE_DIM(obj); ++jj)
                    gc_mark(STRUCTURE_ELEM(obj, jj));

               obj = STRUCTURE_LAYOUT(obj);
               break;

          case TC_VALUES_TUPLE:
               obj = VALUES_TUPLE_VALUES(obj);
               break;

          case TC_FAST_OP:
               gc_mark(FAST_OP_ARG1(obj));
               gc_mark(FAST_OP_ARG2(obj));
               obj = FAST_OP_NEXT(obj);
               break;

          case TC_FASL_READER:
               obj = fasl_reader_gc_mark(obj);
               break;

          default:
               /* By default, objects are either immediate or otherwise self
                * contained, and do not need special-case handling in gc_mark.
                */
               break;
          }
     }
}


static void gc_mark_range_array(lref_t * base, size_t n)
{
     for (size_t jj = 0; jj < n; ++jj)
     {
          lref_t p = base[jj];

          if (gc_possible_heap_pointer_p(p))
               gc_mark(p);
     }
}

static void gc_mark_roots(void)
{
     for (size_t root_idx = 0; root_idx < MAX_GC_ROOTS; root_idx++)
          gc_mark_range_array(interp.thread.gc_roots[root_idx].location,
                              interp.thread.gc_roots[root_idx].length);
}

static void gc_mark_range(lref_t * start, lref_t * end)
{
     if (start > end)
     {
          lref_t *tmp = start;
          start = end;
          end = tmp;
     }

     size_t n = end - start;

     gc_mark_range_array(start, n);
}

static void gc_clear_cell(lref_t obj)
{
     switch (TYPE(obj))
     {
     case TC_STRING:
          gc_free(STRING_DATA(obj));
          break;

     case TC_STRUCTURE:
          gc_free(STRUCTURE_DATA(obj));;
          break;

     case TC_VECTOR:
          gc_free(VECTOR_DATA(obj));
          break;

     case TC_HASH:
          gc_free(HASH_DATA(obj));
          break;

     case TC_PORT:
          port_gc_free(obj);
          break;

     case TC_FASL_READER:
          gc_free(FASL_READER_STREAM(obj));
          break;

     default:
          /*  By default, objects are either immediate or otherwise self
           * contained, and do not need special-case handling in
           * gc_clear_cell. */
          break;

     }

     gc_init_cell(obj);
}


/* gc_sweep
 *
 * Sweeps all unmarked memory cells back into the interp.gc_heap_freelist,
 * calling the appropriate gc_free hooks along the way.
 */
static fixnum_t gc_sweep()
{
     fixnum_t free_cells = 0;
     fixnum_t cells_freed = 0;

     lref_t current_sub_freelist = NIL;
     size_t current_sub_freelist_size = 0;


     for (size_t heap_num = 0;
          heap_num < interp.gc_max_heap_segments;
          heap_num++)
     {
          if (interp.gc_heap_segments[heap_num] == NULL)
               continue;

          lref_t org = interp.gc_heap_segments[heap_num];
          lref_t end = org + interp.gc_heap_segment_size;

          for (lref_t obj = org; obj < end; ++obj)
          {
               if (GC_MARK(obj))
               {
                    SET_GC_MARK(obj, 0);
                    continue;
               }

               free_cells++;

               if (FREE_CELL_P(obj))
                    continue;

               cells_freed++;

               gc_clear_cell(obj);

               current_sub_freelist_size++;
               current_sub_freelist =
                    SET_NEXT_FREE_CELL(obj, current_sub_freelist);

               if (current_sub_freelist_size >= SUB_FREELIST_SIZE)
               {
                    interp.gc_global_freelist =
                         SET_NEXT_FREE_LIST(obj, interp.gc_global_freelist);

                    current_sub_freelist_size = 0;
                    current_sub_freelist = NIL;
               }
          }
     }

     if (!NULLP(current_sub_freelist))
          interp.gc_global_freelist =
               SET_NEXT_FREE_LIST(current_sub_freelist,
                                  interp.gc_global_freelist);

     dscwritef(DF_SHOW_GC_DETAILS, (";;; GC sweep done, freed:~cd, free:~cd\n",
                                    cells_freed, free_cells));

     return free_cells;
}

static void gc_mark_stack()
{
     jmp_buf registers;
     lref_t stack_end;

     setjmp(registers);

     gc_mark_range((lref_t *) registers,
                   (lref_t *) (((uint8_t *) registers) + sizeof(registers)));

     gc_mark_range((lref_t *) sys_get_stack_start(),
                   (lref_t *) & stack_end);
}

static void gc_begin_stats(void)
{
     gc_begin_timer();

     if (!DEBUG_FLAG(DF_SHOW_GC))
          return;

     if ((interp.gc_malloc_bytes > 0) || (interp.gc_malloc_blocks > 0))
          dscwritef(DF_ALWAYS, (_T("; ~cd/~cd C bytes/blocks allocated since last GC.\n"),
                     interp.gc_malloc_bytes, interp.gc_malloc_blocks));

     dscwritef(DF_ALWAYS, (_T("; GC @ T+~cf:"), time_since_launch()));
}

static double gc_end_stats(void)
{
     interp.gc_malloc_bytes = 0;
     interp.gc_malloc_blocks = 0;

     return gc_end_timer();
}

static fixnum_t gc_mark_and_sweep(void)
{
     gc_begin_stats();

     gc_mark_stack();
     gc_mark_roots();

     fixnum_t free_cells = gc_sweep();

     double gc_run_time  = gc_end_stats();

     dscwritef(DF_SHOW_GC, (" ~cfs., ~cd free cells\n", gc_run_time, free_cells));

     return free_cells;
}


/*** The main entry point to the GC */

static fixnum_t gc_collect_garbage(void)
{
     fixnum_t free_cells = gc_mark_and_sweep();

     if (NULLP(interp.gc_global_freelist))
          gc_enlarge_heap();

     if (NULLP(interp.gc_global_freelist))
          panic("ran out of storage");

     vmtrap(TRAP_AFTER_GC, VMT_OPTIONAL_TRAP, 1, fixcons(free_cells));

     return free_cells;
}
/*** Global freelist enqueue and dequeue */

lref_t gc_claim_freelist()
{
     lref_t new_freelist = NIL;

     if (NULLP(interp.gc_global_freelist)
         || (interp.gc_malloc_bytes > interp.gc_malloc_bytes_threshold)
         || ALWAYS_GC)
          gc_collect_garbage();

     if (NULLP(interp.gc_global_freelist))
          gc_enlarge_heap();

     assert(!NULLP(interp.gc_global_freelist));

     new_freelist = interp.gc_global_freelist;

     interp.gc_global_freelist = NEXT_FREE_LIST(interp.gc_global_freelist);

     SET_NEXT_FREE_LIST(new_freelist, NIL);

     return new_freelist;
}

static size_t gc_count_active_heap_segments(void)
{
     size_t count = 0;

     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          if (interp.gc_heap_segments[jj] != NULL)
               count++;

     return count;;
}

/*** GC heap startup and shutdown */

void gc_initialize_heap()
{
     /* Initialize the heap table */
     interp.gc_heap_segments =
          (lref_t *) gc_malloc(sizeof(lref_t) * interp.gc_max_heap_segments);

     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          interp.gc_heap_segments[jj] = NULL;

     /* Get us started with one heap */
     gc_enlarge_heap();
}

void gc_release_heap()
{
     gc_sweep();

     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          if (interp.gc_heap_segments[jj])
               gc_free(interp.gc_heap_segments[jj]);
}

/**** Scheme interface functions */


lref_t lirequest_heap_size(lref_t c)
{
     size_t requested = 1;
     size_t created = 0;

     if (!NULLP(c))
     {
          fixnum_t r = get_c_fixnum(c);

          if ((r < 1) || ((size_t) r > interp.gc_max_heap_segments))
               vmerror_arg_out_of_range(c, _T("[1,MAX_HEAPS]"));

          requested = (size_t) r - interp.gc_current_heap_segments;
     }

     for (created = 0; created < requested; created++)
          if (!gc_enlarge_heap())
               break;

     dscwritef(DF_SHOW_GC, (_T("; Allocated ~cd heap~cs of ~cd requested.\n"),
                            created, created > 1 ? "s" : "", requested));

     return fixcons(interp.gc_current_heap_segments);
}

lref_t lgc()
{
     gc_collect_garbage();

     return NIL;
}

lref_t lgc_info()
{
     lref_t argv[7];

     argv[0] = fixcons(gc_count_active_heap_segments());
     argv[1] = fixcons(interp.gc_heap_segment_size);
     argv[2] = fixcons(interp.gc_max_heap_segments);
     argv[3] = fixcons(gc_heap_freelist_length());
     argv[4] = fixcons(interp.gc_total_cells_allocated);
     argv[5] = fixcons(interp.gc_malloc_bytes);
     argv[6] = fixcons(interp.gc_malloc_blocks);

     return lvector(7, argv);
}
