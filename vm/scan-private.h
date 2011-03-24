/*
 * scan-private.h --
 *
 * Private declarations used internally to the interpreter
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __SCAN_PRIVATE_H
#define __SCAN_PRIVATE_H

#include "scan-base.h"
#include "scan-constants.h"
#include "scan.h"

BEGIN_NAMESPACE(scan)

/**** Global Interpreter State ****/

struct frame_t
{
     frame_type_t type;

     union
     {
          struct
          {
               lref_t *form;
               lref_t initial_form;
               lref_t env;
          } eval;
          struct
          {
               lref_t tag;
               jmp_buf cframe;
          } escape;
          struct
          {
               lref_t after;
          } unwind;
          struct
          {
               lref_t function;
          } prim;
     } as;
};

struct gc_root_t
{
     const _TCHAR *name;
     lref_t *location;
     size_t length;
};


struct interpreter_thread_info_block_t
{
     lref_t freelist;
     void *stack_base;
     gc_root_t gc_roots[MAX_GC_ROOTS];

     lref_t handler_frames;

     frame_t frame_stack[FRAME_STACK_SIZE];
     frame_t *fsp;

     frame_t *throw_target;
     lref_t throw_value;

#if defined(WITH_FOPLOG_SUPPORT)
     bool foplog_enable;
     lref_t foplog[FOPLOG_SIZE];
     bool foplog_active;
     size_t foplog_index;
#endif
};

struct interpreter_t
{
     /*  A statically allocated lobject_t used to hold a debugger output port.
      *  This is intended to be available before the GC heap is operational,
      *  so it has to be located here, and not on the heap. */
     lobject_t debugger_output;

     /* Debugger flags. */
     debug_flag_t debug_flags;

     vminterrupt_t interrupts_pending;
     bool interrupts_masked;

     lref_t trap_handlers[TRAP_LAST + 1];


     lref_t control_fields[VMCTRL_LAST + 1];

     size_t init_load_file_count;
     _TCHAR *init_load_file_name[MAX_INIT_LOAD_FILES];

     flonum_t launch_realtime;

     lref_t fasl_package_list;

     lref_t base_instance;

     lref_t internal_files;
     lref_t subr_table;
     lref_t startup_args;

     /* GC-specific info. */
     bool gc_trip_wires_armed;

     size_t gc_heap_segment_size;
     size_t gc_max_heap_segments;
     size_t gc_current_heap_segments;
     lref_t *gc_heap_segments;

     lref_t gc_global_freelist;

     size_t gc_total_cells_allocated;

     size_t gc_malloc_bytes;
     size_t gc_malloc_blocks;
     size_t gc_malloc_bytes_at_last_gc;
     size_t gc_malloc_blocks_at_last_gc;
     size_t gc_malloc_bytes_threshold;

     flonum_t gc_total_run_time;
     flonum_t gc_start_time;

     /* Per-thread info. */
     interpreter_thread_info_block_t thread;
};

extern interpreter_t interp;    /*  One interpter... one global state variable. */


/**** Standard accessors for current port. ****/

INLINE lref_t CURRENT_INPUT_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_INPUT_PORT];
}

INLINE lref_t CURRENT_OUTPUT_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_OUTPUT_PORT];
}

INLINE lref_t CURRENT_ERROR_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_ERROR_PORT];
}

INLINE lref_t CURRENT_DEBUG_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_DEBUG_PORT];
}


/***** Debugging tools *****/

void init_debugger_output();

debug_flag_t debug_flags_from_string(debug_flag_t initial,
                                     const _TCHAR * source_name,
                                     const _TCHAR * str);

debug_flag_t debug_flags_from_environment(debug_flag_t initial);

lref_t topmost_primitive();

void scan_postmortem_dump();

INLINE lref_t VM_DEBUG_PORT()
{
     return (&interp.debugger_output);
}

INLINE bool DEBUG_FLAG(debug_flag_t flag)
{
     if (!DEBUGGING_BUILD)
          return false;

     return ((fixnum_t) flag == (interp.debug_flags & (fixnum_t) flag));
}


#define dscwritef(flag, args) \
     do { if (DEBUG_FLAG(flag)) ::scan::dscwritef_impl args; } while(0);

/***** Memory Allocation *****/

INLINE interpreter_thread_info_block_t *CURRENT_TIB()
{
     return &interp.thread;
}


INLINE lref_t new_cell(typecode_t type)
{
     interpreter_thread_info_block_t *thread = CURRENT_TIB();

     if (NULLP(thread->freelist))
          thread->freelist = gc_claim_freelist();

     lref_t retval = thread->freelist;
     thread->freelist = NEXT_FREE_CELL(thread->freelist);

     ++interp.gc_total_cells_allocated;

     SET_TYPE(retval, type);

     return retval;
}


/* Structure base metaclass operations */

bool init_slots(lref_t obj, lref_t initargs, bool names_must_be_symbols);

void port_gc_free(lref_t port);
lref_t port_gc_mark(lref_t obj);

bool string_equal(lref_t a, lref_t b);
bool hash_equal(lref_t a, lref_t b);
bool instance_equal(lref_t a, lref_t b);
bool vector_equal(lref_t a, lref_t b);
bool structure_equal(lref_t sta, lref_t stb);
bool fast_op_equal(lref_t a, lref_t b);

void create_initial_packages();

void init_stdio_ports();

extern port_class_t stderr_port_class;

lref_t initialize_port(lref_t s,
                     port_class_t * cls,
                     lref_t port_name, port_mode_t mode, lref_t user_object, void *user_data);


size_t object_length(lref_t obj);
size_t hash_length(lref_t hash);
size_t port_length(lref_t port);

bool equalp(lref_t, lref_t);
double round(double n);


END_NAMESPACE;

#endif
