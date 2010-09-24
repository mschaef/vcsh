
/* memory.cpp
 *
 * Garbage collected heap management. The GC heap is a heap of LObject's managed by
 * a conservative mark and sweep  garbage collector.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

/*** GC heap startup and shutdown */

/*** GC Root Registry ***/
void gc_protect(const _TCHAR * name, LRef * location, size_t n)
{
     size_t root_index = 0;

     for (root_index = 0; root_index < MAX_GC_ROOTS; root_index++)
          if (CURRENT_TIB()->gc_roots[root_index].name == NULL)
               break;

     assert(root_index < MAX_GC_ROOTS);

     gc_root_t *root = (gc_root_t *) & (CURRENT_TIB()->gc_roots[root_index]);

     root->name = name;
     root->location = location;
     root->length = n;
}

LRef gc_protect_sym(LRef * location, const _TCHAR * st, LRef package)
{
     *location = simple_intern(st, package);

     gc_protect(st, location, 1);

     return *location;
}

static void gc_init_cell(LRef obj)
{
     memset(obj, 0, sizeof(LObject));
     SET_TYPE(obj, TC_FREE_CELL);
     SET_GC_MARK(obj, 0);
}

void dump_freelists()
{
     for (LRef current_freelist = interp.global_freelist;
          current_freelist != NULL; current_freelist = NEXT_FREE_LIST(current_freelist))
     {
          dscwritef("{~c&: ", current_freelist);

          int len = 0;

          for (LRef cell = current_freelist; cell != NULL; cell = NEXT_FREE_CELL(cell))
               len++;

          dscwritef("~cd }", len);
     }

     dscwritef("\n");
}

/*** The heap segment allocator
 *
 * The GC heap is maintained as a variable sized array of heap segments. The VM
 * starts out with one subheap, and will allocate up to as many as HEAP_SEGMENT_LIMIT
 * heaps, on an as-needed basis.
 */

static void gc_init_heap_segment(LRef seg_base)
{
     LRef current_sub_freelist = NIL;
     size_t current_sub_freelist_size = 0;

     for (size_t ofs = 0; ofs < interp.gc_heap_segment_size; ofs++)
     {
          LRef cell = &seg_base[ofs];

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

static bool enlarge_heap()
{
     bool succeeded = false;

     dscwritef(DF_SHOW_GC_DETAILS, ";;; attempting to enlarge heap\n");

     if (interp.gc_current_heap_segments < interp.gc_max_heap_segments)
     {
          LRef seg_base = (LRef) safe_malloc(sizeof(LObject) * interp.gc_heap_segment_size);

          if (seg_base != NULL)
          {
               size_t seg_idx = interp.gc_current_heap_segments;

               interp.gc_current_heap_segments++;

               interp.c_bytes_gc_threshold += (sizeof(LObject) * interp.gc_heap_segment_size);

               interp.gc_heap_segments[seg_idx] = seg_base;

               gc_init_heap_segment(seg_base);

               succeeded = true;
          }
     }


     dscwritef(DF_SHOW_GC_DETAILS,
               succeeded ? ";;; enlarged heap\n" : ";;; HEAP ENLARGE FAILED!\n");

     return succeeded;
}

LRef lenlarge_heap(LRef c)
{
     size_t requested = 1;
     size_t created = 0;

     if (!NULLP(c))
     {
          fixnum_t r = get_c_fixnum(c);

          if (r < 1)
               return vmerror("Heap size requests cannot be < 1", c);

          if ((size_t) r >= interp.gc_max_heap_segments)
               return
                   vmerror("Heap size requests cannot be larger than the maximum number of heaps.",
                           c);

          requested = (size_t) r - interp.gc_current_heap_segments;
     }

     for (created = 0; created < requested; created++)
          if (!enlarge_heap())
               break;

     dscwritef(DF_SHOW_GC, _T("; Allocated ~cd heap~cs of ~cd requested.\n"),
               created, created > 1 ? "s" : "", requested);

     return fixcons(interp.gc_current_heap_segments);
}

/*** The Mark-and-Sweep garbage collection algorithm ***/

/* possible_heap_pointer_p
 *
 * Heuristic used to determine if a value is conceivably a pointer.
 */
static bool possible_heap_pointer_p(LRef p)
{
     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
     {
          LRef h = interp.gc_heap_segments[jj];

          /*  Skip unallocated gc_heap_segments; */
          if (h == NULL)
               continue;

          /*  Pointers point into gc_heap_segments */
          if ((p < h) || (p >= (h + interp.gc_heap_segment_size)))
               continue;

          /*  Pointers are aligned at LObject boundaries */
          if (((((u8_t *) p) - ((u8_t *) h)) % sizeof(LObject)) != 0)
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
void gc_mark(LRef initial_obj)
{
     LRef obj = initial_obj;

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
               obj = SUBR_PROPERTY_LIST(obj);
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

  /* mark_protected_registers
   *
   * Walk the list of GC roots, calling mark on each root */
static void gc_mark_roots(void)
{
     for (size_t root_idx = 0; root_idx < MAX_GC_ROOTS; root_idx++)
          for (size_t ii = 0; ii < interp.thread.gc_roots[root_idx].length; ii++)
               gc_mark((interp.thread.gc_roots[root_idx].location)[ii]);
}

static void gc_mark_range_array(LRef * base, size_t n)
{
     for (size_t jj = 0; jj < n; ++jj)
     {
          LRef p = base[jj];

          if (possible_heap_pointer_p(p))
               gc_mark(p);
     }
}

static void gc_mark_range(LRef * start, LRef * end)
{
     if (start > end)
     {
          LRef *tmp = start;
          start = end;
          end = tmp;
     }

     size_t n = end - start;

     gc_mark_range_array(start, n);
}

static void gc_clear_cell(LRef obj)
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

     LRef current_sub_freelist = NIL;
     size_t current_sub_freelist_size = 0;


     for (size_t heap_num = 0; heap_num < interp.gc_max_heap_segments; heap_num++)
     {
          if (interp.gc_heap_segments[heap_num] == NULL)
               continue;

          LRef org = interp.gc_heap_segments[heap_num];
          LRef end = org + interp.gc_heap_segment_size;

          for (LRef obj = org; obj < end; ++obj)
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

     assert(!NULLP(interp.global_freelist));

     interp.gc_cells_collected = cells_freed;

     dscwritef(DF_SHOW_GC_DETAILS, ";;; GC sweep done, freed:~cd, free:~cd\n", cells_freed,
               free_cells);

     return free_cells;
}

static void gc_mark_stack()
{
     jmp_buf registers;
     LRef stack_end;

     setjmp(registers);

     gc_mark_range((LRef *) registers, (LRef *) (((u8_t *) registers) + sizeof(registers)));

     gc_mark_range((LRef *) sys_get_stack_start(), (LRef *) & stack_end);
}


static void gc_begin_stats(void)
{
     interp.gc_run_time = sys_runtime();
     interp.gc_count++;
     interp.gc_cells_collected = 0;

     if (DEBUG_FLAG(DF_SHOW_GC))
     {
          unsigned long bytes_alloced =
              (unsigned long) (malloc_bytes - interp.malloc_bytes_at_last_gc);
          unsigned long blocks_alloced =
              (unsigned long) (malloc_blocks - interp.malloc_blocks_at_last_gc);

          if ((bytes_alloced > 0) || (blocks_alloced > 0))
               dscwritef(_T("; ~cd C bytes in ~cd blocks allocated since last GC.\n"),
                         bytes_alloced, blocks_alloced);

          dscwritef(_T("; GC @ T+~cf:"), time_since_launch());
     }
}

static void gc_end_stats(void)
{
     interp.gc_run_time = sys_runtime() - interp.gc_run_time;
     interp.gc_total_run_time += interp.gc_run_time;

     if (DEBUG_FLAG(DF_SHOW_GC))
          dscwritef(" ~cfs., ~cd cells freed\n", interp.gc_run_time, interp.gc_cells_collected);

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

     /*  Normally, the *after-gc* hook will enlarge the heap according
      *  to whatever policy. If it doesn't, this gives the interpreter
      *  a sort of last ditch way to keep running. */
     if (NULLP(interp.global_freelist))
          lenlarge_heap(NIL);

     if (NULLP(interp.global_freelist))
          panic("ran out of storage");

     return cells_freed;
}

static void invoke_after_gc_hook(fixnum_t cells_freed)
{
     LRef after_gc_hook = SYMBOL_VCELL(interp.sym_after_gc);

     if (!NULLP(after_gc_hook))
          call_lisp_procedure(after_gc_hook, NULL, NULL,
                              2, fixcons(cells_freed), fixcons(interp.gc_heap_segment_size));
}

/*** Global freelist enqueue and dequeue */

void gc_release_freelist(LRef new_freelist)
{
     if (NULLP(new_freelist))
          return;

     SET_NEXT_FREE_LIST(CURRENT_TIB()->freelist, interp.global_freelist);

     interp.global_freelist = CURRENT_TIB()->freelist;

     CURRENT_TIB()->freelist = NULL;
}

LRef gc_claim_freelist()
{
     fixnum_t cells_freed = 0;
     LRef new_freelist = NIL;

     if (NULLP(interp.global_freelist)
         || ((malloc_bytes - interp.malloc_bytes_at_last_gc) > interp.c_bytes_gc_threshold)
         || ALWAYS_GC)
          cells_freed = gc_collect_garbage();

     assert(!NULLP(interp.global_freelist));

     new_freelist = interp.global_freelist;

     interp.global_freelist = NEXT_FREE_LIST(interp.global_freelist);

     SET_NEXT_FREE_LIST(new_freelist, NIL);

     if (cells_freed > 0)
          invoke_after_gc_hook(cells_freed);

     return new_freelist;
}

void create_gc_heap()
{
     /* Initialize the heap table */
     interp.gc_heap_segments = (LRef *) safe_malloc(sizeof(LRef) * interp.gc_max_heap_segments);
     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          interp.gc_heap_segments[jj] = NULL;

     /* Get us started with one heap */
     enlarge_heap();

     /* Set up space for global bindings. */
     interp.last_global_env_entry = 1;
     interp.global_env = vectorcons(GLOBAL_ENV_BLOCK_SIZE, UNBOUND_MARKER);
     gc_protect(_T("global-environment"), &interp.global_env, 1);
}

void free_gc_heap()
{
     gc_sweep();

     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          if (interp.gc_heap_segments[jj])
               safe_free(interp.gc_heap_segments[jj]);

}

/**** The type manager
 *
 * All supported types are registered with the interpreter. This
 * is done for a couple reasons:
 *
 * - Make it easier to perform sanity checks on the heap
 * - Make it possible to look up type information by FASL code
 */

LRef make_type_name(typecode_t type_code)
{
     LRef name = interp.syms_internal_type_names[type_code];

     assert(SYMBOLP(name));

     return name;
}

/**** Scheme interface functions */

LRef lgc()
{
     fixnum_t cells_freed = gc_collect_garbage();

     invoke_after_gc_hook(cells_freed);

     return NIL;
}

static size_t count_active_gc_heap_segments(void)
{
     size_t count = 0;

     for (size_t jj = 0; jj < interp.gc_max_heap_segments; jj++)
          if (interp.gc_heap_segments[jj] != NULL)
               count++;

     return count;;
}

static size_t gc_heap_freelist_length(void)
{
     size_t n;
     LRef l;

     for (n = 0, l = interp.global_freelist; !NULLP(l); ++n)
          l = CDR(l);

     return n;
}

LRef lgc_status(LRef new_gc_status)
{
     if (!NULLP(new_gc_status))
          interp.gc_status_flag = TRUEP(new_gc_status) ? 1 : 0;

     if (interp.gc_status_flag)
          dscwritef(DF_SHOW_GC, "garbage collection verbose");
     else
          dscwritef(DF_SHOW_GC, "garbage collection silent");

     size_t m = count_active_gc_heap_segments();
     size_t n = gc_heap_freelist_length();

     dscwritef(DF_SHOW_GC, "~cd of ~cd heap segs, ~cd allocated ~cd free\n", m, interp.gc_max_heap_segments,
               m * interp.gc_heap_segment_size - n, n);

     return boolcons(interp.gc_status_flag != 0);
}

LRef lgc_info()
{
     LRef argv[8];
     argv[0] = fixcons(count_active_gc_heap_segments());
     argv[1] = fixcons(gc_heap_freelist_length());
     argv[2] = fixcons(interp.gc_total_cells_allocated);
     argv[3] = fixcons(interp.gc_total_environment_cells_allocated);
     argv[4] = fixcons(malloc_bytes);
     argv[5] = fixcons(interp.malloc_bytes_at_last_gc);
     argv[6] = fixcons(malloc_blocks);
     argv[7] = fixcons(interp.malloc_blocks_at_last_gc);

     return lvector(8, argv);
}

END_NAMESPACE
