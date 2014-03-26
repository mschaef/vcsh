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

/**** Global Interpreter State ****/

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
     struct gc_root_t gc_roots[MAX_GC_ROOTS];

     lref_t handler_frames;

     lref_t frame_stack[FRAME_STACK_SIZE];
     lref_t *fsp;
     lref_t *frame;

     lref_t *escape_frame;
     lref_t escape_value;

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
     struct lobject_t debugger_output;

     /* Debugger flags. */
     enum debug_flag_t debug_flags;

     enum vminterrupt_t intr_pending;
     bool intr_masked;

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
     struct interpreter_thread_info_block_t thread;
};

extern struct interpreter_t interp;  /*  Interpeter global state variable. */

INLINE struct interpreter_thread_info_block_t *CURRENT_TIB()
{
     return &interp.thread;
}

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

enum debug_flag_t debug_flags_from_string(enum debug_flag_t initial,
                                          const _TCHAR * source_name,
                                          const _TCHAR * str);

enum debug_flag_t debug_flags_from_environment(enum debug_flag_t initial);

lref_t topmost_primitive();

void scan_postmortem_dump();

INLINE lref_t VM_DEBUG_PORT()
{
     return (&interp.debugger_output);
}

INLINE bool DEBUG_FLAG(enum debug_flag_t flag)
{
     return ((fixnum_t) flag == (interp.debug_flags & (fixnum_t) flag));
}

#define pdscwritef(flag, args) \
     do { if (DEBUG_FLAG(flag)) dscwritef_impl args; } while(0);

/***** I/O Encode and Decode *****/

void io_encode_uint8(uint8_t *buf, unsigned_fixnum_t num);
unsigned_fixnum_t io_decode_uint8(uint8_t *buf);

void io_encode_int8(uint8_t *buf, fixnum_t num);
fixnum_t io_decode_int8(uint8_t *buf);

void io_encode_uint16(uint8_t *buf, unsigned_fixnum_t num);
unsigned_fixnum_t io_decode_uint16(uint8_t *buf);

void io_encode_int16(uint8_t *buf, fixnum_t num);
fixnum_t io_decode_int16(uint8_t *buf);

void io_encode_uint32(uint8_t *buf, unsigned_fixnum_t num);
unsigned_fixnum_t io_decode_uint32(uint8_t *buf);

void io_encode_int32(uint8_t *buf, fixnum_t num);
fixnum_t io_decode_int32(uint8_t *buf);

void io_encode_uint64(uint8_t *buf, unsigned_fixnum_t num);
unsigned_fixnum_t io_decode_uint64(uint8_t *buf);

void io_encode_int64(uint8_t *buf, fixnum_t num);
fixnum_t io_decode_int64(uint8_t *buf);

/***** Memory Management *****/

void gc_initialize_heap();
void gc_release_heap();

void gc_protect(const _TCHAR * name, lref_t * location, size_t n);

void gc_mark(lref_t obj);

lref_t gc_claim_freelist();

void *gc_malloc(size_t size);
void gc_free(void *mem);

INLINE lref_t new_cell(enum typecode_t type)
{
     struct interpreter_thread_info_block_t *thread = CURRENT_TIB();

     if (NULLP(thread->freelist))
          thread->freelist = gc_claim_freelist();

     lref_t retval = thread->freelist;
     thread->freelist = NEXT_FREE_CELL(thread->freelist);

     ++interp.gc_total_cells_allocated;

     SET_TYPE(retval, type);

     return retval;
}

/**** VM Unit Tests ****/

size_t execute_vm_tests();

/**** Startup and Shutdown Routines for subsystems ****/

void create_initial_packages();

void init_stdio_ports();

/**** Structure/Instance ****/

void port_gc_free(lref_t port);
lref_t port_gc_mark(lref_t obj);
lref_t fasl_reader_gc_mark(lref_t obj);

/**** Subr Binding ****/

lref_t find_subr_by_name(lref_t subr_name);

/**** Port I/O ****/

extern struct port_class_t stderr_port_class;

lref_t initialize_port(lref_t s,
                       struct port_class_t * cls,
                       lref_t port_name,
                       enum port_mode_t mode,
                       lref_t user_object,
                       void *user_data);

struct port_text_info_t *allocate_text_info();

/**** Length and Equal ****/

size_t object_length(lref_t obj);
size_t hash_length(lref_t hash);
size_t port_length(lref_t port);

bool equalp(lref_t, lref_t);

bool string_equal(lref_t a, lref_t b);
bool hash_equal(lref_t a, lref_t b);
bool vector_equal(lref_t a, lref_t b);
bool structure_equal(lref_t sta, lref_t stb);
bool fast_op_equal(lref_t a, lref_t b);

/**** Time ****/

flonum_t time_since_launch();

/**** Vector Resizing ****/

lref_t vector_resize(lref_t vec, size_t new_size, lref_t new_element);

/**** String I/O Support ****/

void string_appendd(lref_t str, const _TCHAR *buf, size_t len);

/**** Macro constructor ****/

lref_t macrocons(lref_t t);

/**** Fast Op Constructor ****/

lref_t fast_op(int opcode, lref_t arg1, lref_t arg2, lref_t next);

#endif
