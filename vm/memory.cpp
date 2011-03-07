
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

#include "scan.h"

BEGIN_NAMESPACE(scan)

/*** GC heap startup and shutdown */

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

     gc_root_t *root = (gc_root_t *) & (CURRENT_TIB()->gc_roots[root_index]);

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

     for (lref_t cell = current_freelist; cell != NULL; cell = NEXT_FREE_CELL(cell))
          len++;

     return len;
}

void gc_dump_freelists()
{
     for (lref_t flist = interp.global_freelist; flist != NULL; flist = NEXT_FREE_LIST(flist))
     {
          dscwritef(DF_ALWAYS, ("{~c&:~cd}", flist, gc_sub_freelist_length(flist)));
     }

     dscwritef(DF_ALWAYS, ("\n"));
}

static size_t gc_heap_freelist_length(void)
{
     size_t count = 0;

     for (lref_t flist = interp.global_freelist; flist != NULL;  flist = NEXT_FREE_LIST(flist))
          count++;

     return count;
}

/*** The heap segment allocator
 *
 * The GC heap is maintained as a variable sized array of heap segments. The VM
 * starts out with one subheap, and will allocate up to as many as HEAP_SEGMENT_LIMIT
 * heaps, on an as-needed basis.
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
               interp.global_freelist =
                   SET_NEXT_FREE_LIST(current_sub_freelist, interp.global_freelist);

               current_sub_freelist_size = 0;
               current_sub_freelist = NIL;
          }
     }

     if (!NULLP(current_sub_freelist))
          interp.global_freelist = SET_NEXT_FREE_LIST(current_sub_freelist, interp.global_freelist);
}

static bool gc_enlarge_heap()
{
     dscwritef(DF_SHOW_GC_DETAILS, (";;; attempting to enlarge heap\n"));

     if (interp.gc_current_heap_segments > interp.gc_max_heap_segments)
     {
          dscwritef(DF_SHOW_GC_DETAILS, (";;; HEAP ENLARGE FAILED! Too many segments.\n"));
          return false;
     }

     lref_t seg_base = (lref_t) safe_malloc(sizeof(lobject_t) * interp.gc_heap_segment_size);

     if (seg_base == NULL)
     {
          dscwritef(DF_SHOW_GC_DETAILS, (";;; HEAP ENLARGE FAILED! Could not allocate memory from system.\n"));
          return false;
     }

     size_t seg_idx = interp.gc_current_heap_segments;

     interp.gc_current_heap_segments++;

     interp.c_bytes_gc_threshold += (sizeof(lobject_t) * interp.gc_heap_segment_size);

     interp.gc_heap_segments[seg_idx] = seg_base;

     gc_init_heap_segment(seg_base);

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
          if (((((uint8_t *) p) - ((uint8_t *) h)) % sizeof(lobject_t)) != 0)
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
               gc_mark((*obj).storage_as.symbol.props); /*  REVISIT: better accessor? */
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
                    gc_mark(HASH_DATA(obj)[jj]._key);
                    gc_mark(HASH_DATA(obj)[jj]._val);
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

          case TC_INSTANCE:
               gc_mark(INSTANCE_MAP(obj));
               for (size_t jj = 0; jj < INSTANCE_DIM(obj); jj++)
                    gc_mark(INSTANCE_ELEM(obj, jj));
               break;

          case TC_FAST_OP:
               gc_mark(FAST_OP_ARG1(obj));
               gc_mark(FAST_OP_ARG2(obj));
               obj = FAST_OP_ARG3(obj);

          default:
               /*  By default, objects are either immediate or otherwise self contained, and 
                *  do not need special-case handling in gc_mark. */
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
          safe_free(STRING_DATA(obj));
          break;

     case TC_VECTOR:
          safe_free(VECTOR_DATA(obj));
          break;
     
     case TC_HASH:
          safe_free(HASH_DATA(obj));
          break;


     case TC_PORT:
          port_gc_free(obj);
          break;

     case TC_INSTANCE:
          safe_free(INSTANCE_DATA(obj));
          break;

     case TC_GC_TRIP_WIRE:
          if (interp.gc_trip_wires_armed)
               panic("GC trip wire freed!");
          break;

     default:
          /*  By default, objects are either immediate or otherwise self contained, and
           *  do not need special-case  */
          break;

     }

     gc_init_cell(obj);
}


/* gc_sweep
 *
 * Sweeps all unmarked memory cells back into the interp.gc_heap_freelist,
 * calling the appropriate gc_free hooks along the way.
 */
fixnum_t gc_sweep()
{
     fixnum_t free_cells = 0;
     fixnum_t cells_freed = 0;

     lref_t current_sub_freelist = NIL;
     size_t current_sub_freelist_size = 0;


     for (size_t heap_num = 0; heap_num < interp.gc_max_heap_segments; heap_num++)
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
               current_sub_freelist = SET_NEXT_FREE_CELL(obj, current_sub_freelist);

               if (current_sub_freelist_size >= SUB_FREELIST_SIZE)
               {
                    interp.global_freelist = SET_NEXT_FREE_LIST(obj, interp.global_freelist);

                    current_sub_freelist_size = 0;
                    current_sub_freelist = NIL;
               }
          }
     }

     if (!NULLP(current_sub_freelist))
          interp.global_freelist = SET_NEXT_FREE_LIST(current_sub_freelist, interp.global_freelist);

     interp.gc_cells_collected = cells_freed;

     dscwritef(DF_SHOW_GC_DETAILS, (";;; GC sweep done, freed:~cd, free:~cd\n", cells_freed,
                                    free_cells));

     return free_cells;
}

static void gc_mark_stack()
{
     jmp_buf registers;
     lref_t stack_end;

     setjmp(registers);

     gc_mark_range((lref_t *) registers, (lref_t *) (((uint8_t *) registers) + sizeof(registers)));

     gc_mark_range((lref_t *) sys_get_stack_start(), (lref_t *) & stack_end);
}


static void gc_begin_stats(void)
{
     interp.gc_run_time = sys_runtime();
     interp.gc_cells_collected = 0;

     if (!DEBUG_FLAG(DF_SHOW_GC))
          return;

     unsigned long bytes_alloced =
          (unsigned long) (malloc_bytes - interp.malloc_bytes_at_last_gc);
     unsigned long blocks_alloced =
          (unsigned long) (malloc_blocks - interp.malloc_blocks_at_last_gc);

     if ((bytes_alloced > 0) || (blocks_alloced > 0))
          dscwritef(DF_ALWAYS, (_T("; ~cd C bytes in ~cd blocks allocated since last GC.\n"),
                                bytes_alloced, blocks_alloced));

     dscwritef(DF_ALWAYS, (_T("; GC @ T+~cf:"), time_since_launch()));
}

static void gc_end_stats(void)
{
     interp.gc_run_time = sys_runtime() - interp.gc_run_time;
     interp.gc_total_run_time += interp.gc_run_time;

     dscwritef(DF_SHOW_GC, (" ~cfs., ~cd cells freed\n", interp.gc_run_time, interp.gc_cells_collected));

     interp.malloc_bytes_at_last_gc = malloc_bytes;
     interp.malloc_blocks_at_last_gc = malloc_blocks;
}

fixnum_t gc_mark_and_sweep(void)
{
     fixnum_t cells_freed;

     gc_begin_stats();

     gc_mark_stack();
     gc_mark_roots();

     cells_freed = gc_sweep();

     gc_end_stats();

     return cells_freed;
}


/*** The main entry point to the GC */

static fixnum_t gc_collect_garbage(void)
{
     fixnum_t cells_freed = 0;

     cells_freed = gc_mark_and_sweep();

     if (NULLP(interp.global_freelist))
          gc_enlarge_heap();

     if (NULLP(interp.global_freelist))
          panic("ran out of storage");

     vmtrap(TRAP_AFTER_GC, VMT_OPTIONAL_TRAP, 1, fixcons(cells_freed));

     return cells_freed;
}
/*** Global freelist enqueue and dequeue */

lref_t gc_claim_freelist()
{
     fixnum_t cells_freed = 0;
     lref_t new_freelist = NIL;

     if (NULLP(interp.global_freelist)
         || ((malloc_bytes - interp.malloc_bytes_at_last_gc) > interp.c_bytes_gc_threshold)
         || ALWAYS_GC)
          cells_freed = gc_collect_garbage();

     if (NULLP(interp.global_freelist))
          gc_enlarge_heap();

     assert(!NULLP(interp.global_freelist));

     new_freelist = interp.global_freelist;

     interp.global_freelist = NEXT_FREE_LIST(interp.global_freelist);

     SET_NEXT_FREE_LIST(new_freelist, NIL);

     return new_freelist;
}

void gc_initialize_heap()
{
     /* Initialize the heap table */
     interp.gc_heap_segments = (lref_t *) safe_malloc(sizeof(lref_t) * interp.gc_max_heap_segments);

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
               safe_free(interp.gc_heap_segments[jj]);
}

static size_t gc_count_active_heap_segments(void)
{
     size_t count = 0;

     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          if (interp.gc_heap_segments[jj] != NULL)
               count++;

     return count;;
}

/**** Scheme interface functions */


lref_t lenlarge_heap(lref_t c)
{
     size_t requested = 1;
     size_t created = 0;

     if (!NULLP(c))
     {
          fixnum_t r = get_c_fixnum(c);

          if ((r < 1) || ((size_t) r >= interp.gc_max_heap_segments))
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
     lref_t argv[8];
     argv[0] = fixcons(gc_count_active_heap_segments());
     argv[1] = fixcons(gc_heap_freelist_length());
     argv[2] = fixcons(interp.gc_total_cells_allocated);
     argv[3] = fixcons(0);
     argv[4] = fixcons(malloc_bytes);
     argv[5] = fixcons(interp.malloc_bytes_at_last_gc);
     argv[6] = fixcons(malloc_blocks);
     argv[7] = fixcons(interp.malloc_blocks_at_last_gc);

     return lvector(8, argv);
}

/* GC Trip wire support.
 *
 * GC trip wires pretty much do what they sound like, they blow up when the garbage
 * collector touches (attempts to free) them. They are used in tests to verify that
 * the GC is picking up all object references.
 */

lref_t ligc_trip_wire()
{
     return new_cell(TC_GC_TRIP_WIRE);
}

lref_t liarm_gc_trip_wires(lref_t f)
{
     bool new_state = TRUEP(f);

     interp.gc_trip_wires_armed = new_state;

     return boolcons(new_state);
}

END_NAMESPACE
