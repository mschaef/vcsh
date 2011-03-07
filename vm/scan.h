
/*
 * scan.h --
 *
 * The primary interpreter header file.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __SCAN_H
#define __SCAN_H

// #define WITH_FOPLOG_SUPPORT

#include "base-types.h"

#include <setjmp.h>
#include <stdarg.h>

#include "constants.h"
#include "sys.h"

BEGIN_NAMESPACE(scan)

extern int64_t malloc_bytes;      /* REVISIT: Should this be uint64_t? Should this even be here? */

/*** Interpreter Paramaters ***/

#define SCAN_VERSION _T("SCAN 0.60")

enum
{
     /*  Default size of a heap segment, in cells */
     DEFAULT_HEAP_SEGMENT_SIZE = 262144,

     /*  Default limit on the Maximum number of heap segments */
     DEFAULT_MAX_HEAP_SEGMENTS = 256,

     /*  Default size for FASL loader tables */
     DEFAULT_FASL_TABLE_SIZE = 16384,

     /*  Local (stack) string buffer size */
     STACK_STRBUF_LEN = 256,

     /* The number of characters that can be ungotten from a port */
     PORT_UNGET_BUFFER_SIZE = 8,

     /*  Record individual safe_mallocs to debug */
     DETAILED_MEMORY_LOG = FALSE,

     /*  Garbage collect on each call to newCell (Very slow) */
     ALWAYS_GC = FALSE,

     /*  The depth of the stack the FASL loader uses to store load unit state */
     FAST_LOAD_STACK_DEPTH = 16,

     /*  The number of arguments contained in argment buffers */
     ARG_BUF_LEN = 32,

     /*  The number of cells on a sub-freelist */
     SUB_FREELIST_SIZE = 1024,

     /*  The maximum number of GC roots per thread */
     MAX_GC_ROOTS = 32,

     /*  The debug printer's flonum precisionn */
     DEBUG_FLONUM_PRINT_PRECISION = 8,

     /* The maximum number of init load files. */
     MAX_INIT_LOAD_FILES = 8,

     /* The number of frames that can be stored on the frame stack. */
     FRAME_STACK_SIZE = 2048,

     /*** Hash table tuning settings ***/

     /*  Default initial size for hash tables */
     HASH_DEFAULT_INITIAL_SIZE = 8,

     /* The maximum allowable load factor for a hash table. If the fraction of
      * used table entries exceeds this, then the hash table is enlarged. */
     HASH_MAX_LOAD_FACTOR = 67, /* percent */

     /* The factor by which 'small' hash tables are enlarged. */
     HASH_SMALL_ENLARGE_FACTOR = 2,

     /* The factor by which 'large' hash tables are enlarged. */
     HASH_LARGE_ENLARGE_FACTOR = 4,

     /* The number of active elements a hash table needs in order to
      * be considered 'large'. */
     HASH_SMALL_ENLARGE_THRESHOLD = 50000,

     /* The maximum size of blocks of text sent to the debug port. */
     DEBUG_PORT_BLOCK_SIZE = 256,
};

#if defined(WITH_FOPLOG_SUPPORT)
#  define FOPLOG_SIZE (1024 * 1024)
#endif

/*** Interpreter specific types ***/

#define FIXNUM_64BIT            /* Support for MSC style 64-bit integers */

/*** Fixnum and Flonum ***/

#ifdef FIXNUM_64BIT
typedef int64_t fixnum_t;
typedef uint64_t unsigned_fixnum_t;

#   define FIXNUM_BITS (64)
#   define FIXNUM_MAX           INT64_MAX
#   define FIXNUM_MIN           INT64_MIN
#   define FIXNUM_UNSIGNED_MAX  UINT64_MAX
#   define FIXNUM_UNSIGNED_MIN  UINT64_MIN
#   define PRINTF_PREFIX_FIXNUM PRINTF_PREFIX_INT64

#else
typedef int32_t fixnum_t;
typedef uint32_t unsigned_fixnum_t;

#   define FIXNUM_BITS (32)
#   define FIXNUM_MAX           INT32_MAX
#   define FIXNUM_MIN           INT32_MIN
#   define FIXNUM_UNSIGNED_MAX  UINT32_MAX
#   define FIXNUM_UNSIGNED_MIN  UINT32_MIN
#   define PRINTF_PREFIX_FIXNUM ""
#endif

typedef double flonum_t;

#define FLONUM_MAX DBL_MAX
#define FLONUM_MIN -DBL_MAX
#define FLONUM_EPSILON DBL_EPSILON

/*** Data Block ***/

/* Microsoft C and gcc appear to have differing opinions on how to
 * initialize a structure with an indefinate sized array at the end. */

#if defined(_MSC_VER)

typedef uint8_t internal_file_data_t[];
#  define INTERNAL_FILE_DATA_CAST

#else

typedef uint8_t *internal_file_data_t;
#  define INTERNAL_FILE_DATA_CAST (uint8_t [])

#endif

#ifdef SCAN_WINDOWS
#  pragma warning (push)
#  pragma warning (disable: 4200)
#endif

struct internal_file_t
{
     _TCHAR *_name;
     size_t _length;
     internal_file_data_t _bytes;
};

#define DECL_INTERNAL_FILE struct scan::internal_file_t

#ifdef SCAN_WINDOWS
#  pragma warning (pop)
#endif

/* _The_ type *************************************************
 *
 * This is the structure of a Lisp cell. The root of every Lisp
 * object is one of these... */

/* ...Forward declarations and typedefs... */

struct port_class_t;
struct port_info_t;
struct lobject_t;
typedef lobject_t *lref_t;

enum lref_tag_t
{
     /* First tagging stage, least sig two bits. */
     LREF1_TAG_MASK = 0x3,
     LREF1_TAG_SHIFT = 2,
     LREF1_REF = 0x0,
     LREF1_FIXNUM = 0x1,
     LREF1_SPECIAL = 0x3,       /*  signals second stage tagging */


     /* Second tagging stage, least sig five bits. */
     LREF2_TAG_MASK = 0x1F,
     LREF2_TAG_SHIFT = 5,
     LREF2_BOOL = LREF1_TAG_MASK | (0x1 << 2),
     LREF2_CHARACTER = LREF1_TAG_MASK | (0x2 << 2),
     LREF2_EOF = LREF1_TAG_MASK | (0x3 << 2),
     LREF2_UNBOUND = LREF1_TAG_MASK | (0x4 << 2),
};

#define UNBOUND_MARKER ((lref_t)LREF2_UNBOUND)

enum
{
     /*  REVISIT: change to 'IPTR_MAX/MIN' */
     MAX_LREF_FIXNUM = INT32_MAX >> LREF1_TAG_SHIFT,
     MIN_LREF_FIXNUM = INT32_MIN >> LREF1_TAG_SHIFT,
};

INLINE lref_t LREF1_CONS(lref_tag_t tag, intptr_t val)
{
     return (lref_t) ((val << LREF1_TAG_SHIFT) | tag);
}

INLINE lref_t LREF2_CONS(lref_tag_t tag, intptr_t val)
{
     return (lref_t) ((val << LREF2_TAG_SHIFT) | tag);
}

INLINE lref_tag_t LREF1_TAG(lref_t ref)
{
     return (lref_tag_t) ((intptr_t) ref & LREF1_TAG_MASK);
}

INLINE lref_tag_t LREF2_TAG(lref_t ref)
{
     return (lref_tag_t) ((intptr_t) ref & LREF2_TAG_MASK);
}

INLINE intptr_t LREF1_VAL(lref_t ref)
{
     return ((intptr_t) ref & ~LREF1_TAG_MASK) >> LREF1_TAG_SHIFT;
}

INLINE intptr_t LREF2_VAL(lref_t ref)
{
     return ((intptr_t) ref & ~LREF2_TAG_MASK) >> LREF2_TAG_SHIFT;
}

INLINE bool LREF_IMMEDIATE_P(lref_t ref)
{
     return LREF1_TAG(ref) != LREF1_REF;
}

typedef lref_t(*f_0_t) (void);
typedef lref_t(*f_1_t) (lref_t);
typedef lref_t(*f_2_t) (lref_t, lref_t);
typedef lref_t(*f_3_t) (lref_t, lref_t, lref_t);
typedef lref_t(*f_4_t) (lref_t, lref_t, lref_t, lref_t);
typedef lref_t(*f_5_t) (lref_t, lref_t, lref_t, lref_t, lref_t);
typedef lref_t(*f_6_t) (lref_t, lref_t, lref_t, lref_t, lref_t, lref_t);
typedef lref_t(*f_m_t) (lref_t *, lref_t *);
typedef lref_t(*f_f_t) (lref_t, lref_t);
typedef lref_t(*f_argc_t) (size_t, lref_t[]);

/*
 * ...This is the boxed object type...
 */

struct hash_entry_t
{
     lref_t _key;                 /*  == UNBOUND_MARKER for empty. */
     lref_t _val;
};

#pragma pack(push, 4)
struct lobject_t
{
     struct
     {
          typecode_t type:8;
          unsigned int opcode:8;
          unsigned int gc_mark:1;
#if defined(__LP64__)
          unsigned int pad:32;  /*  Explicit pad to keep the LP64 header the same size as an LP64 pointer. */
#endif
     } header;

     union
     {
          struct
          {
               lref_t car;
               lref_t cdr;
          } cons;
          struct
          {
               fixnum_t data;
          } fixnum;
          struct
          {
               flonum_t data;
               lref_t im_part;
          } flonum;
          struct
          {
               lref_t props;
               lref_t vcell;
               lref_t home;
          } symbol;
          struct
          {
               lref_t name;
               lref_t symbol_bindings;
               lref_t use_list;
          } package;
          struct
          {
               lref_t env;
               lref_t code;
               lref_t property_list;
          } closure;
          struct
          {
               lref_t transformer;
          } macro;
          struct
          {
               size_t _dim;
               size_t _ofs;
               _TCHAR *_data;
          } string;
          struct
          {
               size_t dim;
               lref_t *data;
               lref_t layout;
          } vector;
          struct
          {
               lref_t _map;
               size_t _dim;
               lref_t *_data;
          } instance;

          struct
          {
               port_class_t *_class;
               port_info_t *_pinf;
          } port;
          struct
          {
               void *p1;
               void *p2;
               void *p3;
          } misc;
          struct
          {
               lref_t _values;
          } values_tuple;
          struct
          {
               lref_t arg1;
               lref_t arg2;
               lref_t arg3;
          } fast_op;

          struct
          {
               size_t _mask;
               hash_entry_t *_data;

               struct
               {
                    unsigned int shallow_keys:1;
                    unsigned int count:31;
               } info;
          } hash;

          struct
          {
               lref_t name;
               subr_arity_t type;
               union
               {
                    f_0_t f_0;
                    f_1_t f_1;
                    f_2_t f_2;
                    f_3_t f_3;
                    f_4_t f_4;
                    f_5_t f_5;
                    f_6_t f_6;
                    f_m_t f_m;
                    f_f_t f_f;
                    f_argc_t f_argc;

                    void *ptr;
               } code;
          } subr;

     } storage_as;
};
#pragma pack(pop)

const lref_t NIL = ((lobject_t *) 0);

INLINE bool EQ(lref_t x, lref_t y)
{
     return x == y;
};

INLINE bool NULLP(lref_t x)
{
     return EQ(x, NIL);
};

INLINE void SET_GC_MARK(lref_t object, int new_gc_mark_bit)
{
     checked_assert(!LREF_IMMEDIATE_P(object));

     object->header.gc_mark = new_gc_mark_bit;
}

INLINE int GC_MARK(lref_t object)
{
     return object->header.gc_mark;
}

INLINE typecode_t TYPE(lref_t object)
{
     if (NULLP(object))
          return TC_NIL;
     else if (LREF1_TAG(object) == LREF1_REF)
          return NULLP(object) ? TC_NIL : object->header.type;
     else if (LREF1_TAG(object) == LREF1_FIXNUM)
          return TC_FIXNUM;
     else                       /*  if (LREF_TAG(object) == LREF_SPECIAL) */
     {
          if (LREF2_TAG(object) == LREF2_BOOL)
               return TC_BOOLEAN;
          else if (LREF2_TAG(object) == LREF2_CHARACTER)
               return TC_CHARACTER;
          else if (LREF2_TAG(object) == LREF2_UNBOUND)
               return TC_UNBOUND_MARKER;
          else                  /*  if ((LREF_TAG(object) == LREF_SPECIAL) && (LREF_TAG2(object) == LREF_EOF)) */
               return TC_END_OF_FILE;
     }
}

INLINE void SET_TYPE(lref_t object, typecode_t new_type)
{
     checked_assert(!LREF_IMMEDIATE_P(object));

     object->header.type = new_type;
}

INLINE bool TYPEP(lref_t object, typecode_t typeCode)
{
     return TYPE(object) == typeCode;
}

/*** Debugging flags ***/
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

     lref_t global_freelist;

     fixnum_t gc_total_cells_allocated;
     fixnum_t gc_cells_collected;

     fixnum_t malloc_bytes_at_last_gc;
     fixnum_t malloc_blocks_at_last_gc;
     fixnum_t c_bytes_gc_threshold;

     flonum_t gc_total_run_time;
     flonum_t gc_run_time;

     /* Per-thread info. */
     interpreter_thread_info_block_t thread;
};

extern interpreter_t interp;    /*  One interpter... one global state variable. */

/**** Type predicates ****/

/* full INLINE causes problems with gcc 3.4.4, due to prototype. */
inline  lref_t FLOIM(lref_t x);

INLINE bool FREE_CELL_P(lref_t x)
{
     return TYPEP(x, TC_FREE_CELL);
}

INLINE bool CHARP(lref_t x)
{
     return TYPEP(x, TC_CHARACTER);
}

INLINE bool BOOLP(lref_t x)
{
     return TYPEP(x, TC_BOOLEAN);
}

INLINE bool CONSP(lref_t x)
{
     return TYPEP(x, TC_CONS);
}

INLINE bool SYMBOLP(lref_t x)
{
     return TYPEP(x, TC_SYMBOL);
}

INLINE bool FIXNUMP(lref_t x)
{
     return TYPEP(x, TC_FIXNUM);
}

INLINE bool FLONUMP(lref_t x)
{
     return TYPEP(x, TC_FLONUM);
}

INLINE bool REALP(lref_t x)
{
     return (FIXNUMP(x) || (FLONUMP(x) && NULLP(FLOIM(x))));
}

INLINE bool COMPLEXP(lref_t x)
{
     return (FLONUMP(x) && !NULLP(FLOIM(x)));
}

INLINE bool STRINGP(lref_t x)
{
     return TYPEP(x, TC_STRING);
}

INLINE bool NUMBERP(lref_t x)
{
     return (FIXNUMP(x) || FLONUMP(x));
}

INLINE bool PACKAGEP(lref_t x)
{
     return TYPEP(x, TC_PACKAGE);
}

INLINE bool PORTP(lref_t x)
{
     return TYPEP(x, TC_PORT);
}

INLINE bool VECTORP(lref_t x)
{
     return TYPEP(x, TC_VECTOR);
}

INLINE bool STRUCTUREP(lref_t x)
{
     return TYPEP(x, TC_STRUCTURE);
}

INLINE bool HASHP(lref_t x)
{
     return TYPEP(x, TC_HASH);
}

INLINE bool CLOSUREP(lref_t x)
{
     return TYPEP(x, TC_CLOSURE);
}

INLINE bool SUBRP(lref_t x)
{
     return TYPEP(x, TC_SUBR);
}

INLINE bool PROCEDUREP(lref_t x)
{
     return CLOSUREP(x) || SUBRP(x);
}

INLINE bool MACROP(lref_t x)
{
     return TYPEP(x, TC_MACRO);
}

INLINE bool VALUES_TUPLE_P(lref_t x)
{
     return TYPEP(x, TC_VALUES_TUPLE);
}

INLINE bool EOFP(lref_t x)
{
     return TYPEP(x, TC_END_OF_FILE);
}

INLINE bool INSTANCEP(lref_t x)
{
     return TYPEP(x, TC_INSTANCE);
}

INLINE bool UNBOUND_MARKER_P(lref_t x)
{
     return EQ(x, UNBOUND_MARKER);
}

INLINE bool GC_TRIP_WIRE_P(lref_t x)
{
     return TYPEP(x, TC_GC_TRIP_WIRE);
}

INLINE bool FAST_OP_P(lref_t x)
{
     return TYPEP(x, TC_FAST_OP);
}

INLINE bool TRUEP(lref_t x)
{
     return (x) != LREF2_CONS(LREF2_BOOL, 0);
}

INLINE bool FALSEP(lref_t x)
{
     return !TRUEP(x);
}


/*** Boxed object accessors and constructors ***/


/*** boolean **/
INLINE lref_t boolcons(bool val)
{
     return LREF2_CONS(LREF2_BOOL, val ? 1 : 0);
}


INLINE bool BOOLV(lref_t x)
{
     checked_assert(BOOLP(x));

     return LREF2_VAL(x) != 0;
}

  /*** cons/free-cell **/
lref_t listn(long n, ...);
lref_t listv(long n, va_list args);
lref_t lista(size_t n, lref_t args[]);


INLINE lref_t & _CAR(lref_t x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.car);
}

INLINE lref_t CAR(lref_t x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.car);
}

INLINE void SET_CAR(lref_t x, lref_t nv)
{
     checked_assert(CONSP(x));
     ((*x).storage_as.cons.car) = nv;
}

INLINE lref_t & _CDR(lref_t x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.cdr);
}

INLINE lref_t CDR(lref_t x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.cdr);
}

INLINE void SET_CDR(lref_t x, lref_t nv)
{
     checked_assert(CONSP(x));
     ((*x).storage_as.cons.cdr) = nv;
}


INLINE lref_t NEXT_FREE_LIST(lref_t x)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     return ((*x).storage_as.cons.car);
}

INLINE lref_t SET_NEXT_FREE_LIST(lref_t x, lref_t next)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     ((*x).storage_as.cons.car) = next;

     return x;
}

INLINE lref_t NEXT_FREE_CELL(lref_t x)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     return ((*x).storage_as.cons.cdr);
}

INLINE lref_t SET_NEXT_FREE_CELL(lref_t x, lref_t next)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     ((*x).storage_as.cons.cdr) = next;

     return x;
}

  /*** fix/flonum **/
#define fixabs labs

lref_t fixcons(uint32_t high, uint32_t low);
lref_t fixcons(fixnum_t x);
lref_t flocons(double x);
lref_t cmplxcons(flonum_t re, flonum_t im);

fixnum_t get_c_fixnum(lref_t x);
long get_c_long(lref_t x);
double get_c_double(lref_t x);
flonum_t get_c_flonum(lref_t x);
flonum_t get_c_flonum_im(lref_t x);
bool get_c_port_mode(lref_t mode);

INLINE fixnum_t & _FIXNM(lref_t x)
{
     checked_assert(FIXNUMP(x));

     return ((*x).storage_as.fixnum.data);
}

INLINE fixnum_t FIXNM(lref_t x)
{
     checked_assert(FIXNUMP(x));

     if (LREF1_TAG(x) == LREF1_FIXNUM)
          return LREF1_VAL(x);

     return ((*x).storage_as.fixnum.data);
}

INLINE flonum_t FLONM(lref_t x)
{
     checked_assert(FLONUMP(x));
     return ((*x).storage_as.flonum.data);
}

INLINE void SET_FLONM(lref_t x, double val)
{
     checked_assert(FLONUMP(x));
     ((*x).storage_as.flonum.data) = val;
}

inline /* full INLINE causes problems with gcc 3.4.4, due to prototype. */ lref_t FLOIM(lref_t x)
{
     checked_assert(FLONUMP(x));

     return ((*x).storage_as.flonum.im_part);
}

INLINE void SET_FLOIM(lref_t x, lref_t val)
{
     checked_assert(FLONUMP(x));

     ((*x).storage_as.flonum.im_part) = val;
}

INLINE flonum_t CMPLXRE(lref_t x)
{
     return FLONM(x);
}

INLINE flonum_t CMPLXIM(lref_t x)
{
     return FLONM(FLOIM(x));
}

 /*** character **/
lref_t charcons(_TCHAR ch);

INLINE _TCHAR CHARV(lref_t x)
{
     checked_assert(CHARP(x));

     return (_TCHAR) LREF2_VAL(x);
}

  /*** vector **/
lref_t vector_resize(lref_t vec, size_t new_size, lref_t new_element);

lref_t vectorcons(fixnum_t n, lref_t initial = NIL);

INLINE size_t VECTOR_DIM(lref_t obj)
{
     checked_assert(VECTORP(obj));
     return ((obj)->storage_as.vector.dim);
}

INLINE void SET_VECTOR_DIM(lref_t obj, size_t new_dim)
{
     checked_assert(VECTORP(obj));
     ((obj)->storage_as.vector.dim) = new_dim;
}

INLINE lref_t *VECTOR_DATA(lref_t obj)
{
     checked_assert(VECTORP(obj));
     return ((obj)->storage_as.vector.data);
}

INLINE lref_t *SET_VECTOR_DATA(lref_t obj, lref_t * new_data)
{
     checked_assert(VECTORP(obj));
     return ((obj)->storage_as.vector.data) = new_data;
}

INLINE lref_t VECTOR_ELEM(lref_t vec, fixnum_t index)
{
     checked_assert(VECTORP(vec));
     return ((vec)->storage_as.vector.data[(index)]);
}

INLINE lref_t & _VECTOR_ELEM(lref_t vec, fixnum_t index)
{
     checked_assert(VECTORP(vec));
     return ((vec)->storage_as.vector.data[(index)]);
}

INLINE void SET_VECTOR_ELEM(lref_t vec, fixnum_t index, lref_t new_value)
{
     checked_assert(VECTORP(vec));
     ((vec)->storage_as.vector.data[(index)]) = new_value;
}

                                                                                                                                                                              /*** structure ***//*  REVISIT:  how much of the structure representation can be shared with vectors? */

INLINE size_t STRUCTURE_DIM(lref_t obj)
{
     checked_assert(STRUCTUREP(obj));
     return ((obj)->storage_as.vector.dim);
}

INLINE void SET_STRUCTURE_DIM(lref_t obj, size_t len)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.dim) = len;
}

INLINE void SET_STRUCTURE_DATA(lref_t obj, lref_t * data)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.data) = data;
}


INLINE lref_t STRUCTURE_LAYOUT(lref_t obj)
{
     checked_assert(STRUCTUREP(obj));
     return ((obj)->storage_as.vector.layout);
}

INLINE void SET_STRUCTURE_LAYOUT(lref_t obj, lref_t new_layout)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.layout) = new_layout;
}

INLINE lref_t STRUCTURE_ELEM(lref_t obj, fixnum_t index)
{
     checked_assert(STRUCTUREP(obj));
     return ((obj)->storage_as.vector.data[(index)]);
}

INLINE void SET_STRUCTURE_ELEM(lref_t obj, fixnum_t index, lref_t new_value)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.data[(index)]) = new_value;
}

  /*** symbol **/
lref_t symcons(_TCHAR * pname, lref_t home);
lref_t symcons(lref_t pname, lref_t home);

lref_t simple_intern(lref_t name, lref_t package);
lref_t simple_intern(const _TCHAR * name, lref_t package);

lref_t intern(lref_t name, lref_t package);
lref_t keyword_intern(const _TCHAR * name);

INLINE lref_t SYMBOL_PNAME(lref_t sym)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).storage_as.symbol.props));

     lref_t pname = NIL;

     if (STRINGP((*sym).storage_as.symbol.props))
          pname = ((*sym).storage_as.symbol.props);
     else
     {
          checked_assert(CONSP((*sym).storage_as.symbol.props));
          pname = CAR((*sym).storage_as.symbol.props);
          checked_assert(STRINGP(pname));
     }

     return pname;
}

INLINE void SET_SYMBOL_PNAME(lref_t sym, lref_t pname)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(STRINGP(pname));

     ((*sym).storage_as.symbol.props) = pname;
}

INLINE lref_t SYMBOL_PROPS(lref_t sym)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).storage_as.symbol.props));

     if (STRINGP((*sym).storage_as.symbol.props))
          return NIL;
     else
     {
          checked_assert(CONSP((*sym).storage_as.symbol.props));
          return CDR((*sym).storage_as.symbol.props);
     }
}

lref_t lcons(lref_t x, lref_t y);     /*  Forward decl */

INLINE void SET_SYMBOL_PROPS(lref_t sym, lref_t props)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).storage_as.symbol.props));

     if (STRINGP((*sym).storage_as.symbol.props))
     {
          (*sym).storage_as.symbol.props = lcons((*sym).storage_as.symbol.props, props);
     }
     else
     {
          checked_assert(CONSP((*sym).storage_as.symbol.props));
          return SET_CDR((*sym).storage_as.symbol.props, props);
     }
}

INLINE lref_t SYMBOL_VCELL(lref_t sym)
{
     checked_assert(SYMBOLP(sym));
     return ((*sym).storage_as.symbol.vcell);
}


INLINE void SET_SYMBOL_VCELL(lref_t sym, lref_t value)
{
     checked_assert(SYMBOLP(sym));
     ((*sym).storage_as.symbol.vcell) = value;
}

INLINE lref_t SYMBOL_HOME(lref_t x)
{
     checked_assert(SYMBOLP(x));
     return ((*x).storage_as.symbol.home);
}

INLINE void SET_SYMBOL_HOME(lref_t x, lref_t home)
{
     checked_assert(SYMBOLP(x));
     ((*x).storage_as.symbol.home) = home;
}


  /*** package **/
INLINE lref_t PACKAGE_NAME(lref_t x)
{
     checked_assert(PACKAGEP(x));
     return (((*x).storage_as.package.name));
}

INLINE void SET_PACKAGE_NAME(lref_t x, lref_t name)
{
     checked_assert(PACKAGEP(x));
     (((*x).storage_as.package.name)) = name;
}

INLINE lref_t PACKAGE_BINDINGS(lref_t x)
{
     checked_assert(PACKAGEP(x));
     return (((*x).storage_as.package.symbol_bindings));
}

INLINE void SET_PACKAGE_BINDINGS(lref_t x, lref_t symbol_bindings)
{
     checked_assert(PACKAGEP(x));
     (((*x).storage_as.package.symbol_bindings)) = symbol_bindings;
}

INLINE lref_t PACKAGE_USE_LIST(lref_t x)
{
     checked_assert(PACKAGEP(x));
     return (((*x).storage_as.package.use_list));
}

INLINE void SET_PACKAGE_USE_LIST(lref_t x, lref_t use_list)
{
     checked_assert(PACKAGEP(x));
     (((*x).storage_as.package.use_list)) = use_list;
}

  /*** subr **/
INLINE subr_arity_t SUBR_TYPE(lref_t x)
{
     checked_assert(SUBRP(x));
     return (((*x).storage_as.subr.type));
}

INLINE void SET_SUBR_TYPE(lref_t x, subr_arity_t type)
{
     checked_assert(SUBRP(x));
     (((*x).storage_as.subr.type)) = type;
}

INLINE lref_t SUBR_NAME(lref_t x)
{
     checked_assert(SUBRP(x));
     return (((*x).storage_as.subr.name));
}

INLINE void SET_SUBR_NAME(lref_t x, lref_t name)
{
     checked_assert(SUBRP(x));
     checked_assert(STRINGP(name));
     (((*x).storage_as.subr.name)) = name;
}

INLINE void SET_SUBR_CODE(lref_t x, void *code)
{
     ((*x).storage_as.subr.code.ptr) = code;
}

INLINE void *SUBR_CODE(lref_t x)
{
     return (void *)(*x).storage_as.subr.code.ptr;
}

INLINE f_0_t SUBR_F0(lref_t x)
{
     return ((*x).storage_as.subr.code.f_0);
}

INLINE f_1_t SUBR_F1(lref_t x)
{
     return ((*x).storage_as.subr.code.f_1);
}

INLINE f_2_t SUBR_F2(lref_t x)
{
     return ((*x).storage_as.subr.code.f_2);
}

INLINE f_3_t SUBR_F3(lref_t x)
{
     return ((*x).storage_as.subr.code.f_3);
}

INLINE f_4_t SUBR_F4(lref_t x)
{
     return ((*x).storage_as.subr.code.f_4);
}

INLINE f_5_t SUBR_F5(lref_t x)
{
     return ((*x).storage_as.subr.code.f_5);
}

INLINE f_6_t SUBR_F6(lref_t x)
{
     return ((*x).storage_as.subr.code.f_6);
}

INLINE f_argc_t SUBR_FARGC(lref_t x)
{
     return ((*x).storage_as.subr.code.f_argc);
}

  /*** closure **/
INLINE lref_t CLOSURE_CODE(lref_t x)
{
     checked_assert(CLOSUREP(x));
     return ((*x).storage_as.closure.code);
}

INLINE void SET_CLOSURE_CODE(lref_t x, lref_t code)
{
     checked_assert(CLOSUREP(x));
     ((*x).storage_as.closure.code) = code;
}

INLINE lref_t CLOSURE_ENV(lref_t x)
{
     checked_assert(CLOSUREP(x));
     return ((*x).storage_as.closure.env);
}

INLINE void SET_CLOSURE_ENV(lref_t x, lref_t env)
{
     checked_assert(CLOSUREP(x));
     ((*x).storage_as.closure.env) = env;
}

INLINE lref_t CLOSURE_PROPERTY_LIST(lref_t x)
{
     checked_assert(CLOSUREP(x));
     return ((*x).storage_as.closure.property_list);
}

INLINE void SET_CLOSURE_PROPERTY_LIST(lref_t x, lref_t plist)
{
     checked_assert(CLOSUREP(x));
     ((*x).storage_as.closure.property_list) = plist;
}

  /*** macro **/
lref_t macrocons(lref_t t);

INLINE lref_t MACRO_TRANSFORMER(lref_t x)
{
     checked_assert(MACROP(x));
     return (((*x).storage_as.macro.transformer));
}

INLINE void SET_MACRO_TRANSFORMER(lref_t x, lref_t transformer)
{
     checked_assert(MACROP(x));
     (((*x).storage_as.macro.transformer)) = transformer;
}

  /*** string **/
lref_t strcons();
lref_t strcons(_TCHAR ch);
lref_t strcons(const _TCHAR * buffer);
lref_t strcons(const _TCHAR * buffer, _TCHAR trailing);
lref_t strcons(lref_t str);
lref_t strcons(size_t length, const _TCHAR * buffer);
lref_t strcons_transfer_buffer(size_t length, _TCHAR * buffer);

_TCHAR *get_c_string(lref_t x);
_TCHAR *get_c_string_dim(lref_t x, size_t *);
_TCHAR *try_get_c_string(lref_t x);


int str_next_character(lref_t obj);
void str_append_str(lref_t obj, _TCHAR * str, size_t len);

INLINE size_t STRING_DIM(lref_t x)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._dim);
}

INLINE void SET_STRING_DIM(lref_t x, size_t dim)
{
     checked_assert(STRINGP(x));
     ((*x).storage_as.string._dim) = dim;
}

INLINE size_t STRING_OFS(lref_t x)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._ofs);
}

INLINE void SET_STRING_OFS(lref_t x, size_t ofs)
{
     checked_assert(STRINGP(x));
     ((*x).storage_as.string._ofs) = ofs;
}

INLINE _TCHAR *STRING_DATA(lref_t x)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._data);
}

INLINE _TCHAR *SET_STRING_DATA(lref_t x, _TCHAR * data)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._data) = data;
}


  /*** hash **/

lref_t hashcons(bool shallow, size_t size = HASH_DEFAULT_INITIAL_SIZE);

bool hash_ref(lref_t table, lref_t key, lref_t *result);

INLINE size_t HASH_MASK(lref_t obj)
{
     checked_assert(HASHP(obj));
     return ((obj)->storage_as.hash._mask);
}

INLINE void SET_HASH_MASK(lref_t obj, size_t mask)
{
     checked_assert(HASHP(obj));
     ((obj)->storage_as.hash._mask) = mask;
}

INLINE size_t HASH_SIZE(lref_t obj)
{
     return HASH_MASK(obj) + 1;
}

INLINE hash_entry_t *HASH_DATA(lref_t obj)
{
     checked_assert(HASHP(obj));
     return ((obj)->storage_as.hash._data);
}

INLINE hash_entry_t *SET_HASH_DATA(lref_t obj, hash_entry_t * data)
{
     checked_assert(HASHP(obj));
     return ((obj)->storage_as.hash._data) = data;
}

typedef size_t hash_iter_t;
void hash_iter_begin(lref_t hash, hash_iter_t * iter);
bool hash_iter_next(lref_t hash, hash_iter_t * iter, lref_t * key, lref_t * val);

  /*** instance **/

lref_t instancecons(lref_t proto);


INLINE lref_t INSTANCE_MAP(lref_t obj)
{
     checked_assert(INSTANCEP(obj));
     return ((obj)->storage_as.instance._map);
}

INLINE void SET_INSTANCE_MAP(lref_t obj, lref_t map)
{
     checked_assert(INSTANCEP(obj));
     ((obj)->storage_as.instance._map) = map;
}

INLINE size_t INSTANCE_DIM(lref_t obj)
{
     checked_assert(INSTANCEP(obj));
     return ((obj)->storage_as.instance._dim);
}

INLINE void SET_INSTANCE_DIM(lref_t obj, size_t dim)
{
     checked_assert(INSTANCEP(obj));
     ((obj)->storage_as.instance._dim) = dim;
}

INLINE lref_t *INSTANCE_DATA(lref_t obj)
{
     checked_assert(INSTANCEP(obj));
     return ((obj)->storage_as.instance._data);
}

INLINE void SET_INSTANCE_DATA(lref_t obj, lref_t * data)
{
     checked_assert(INSTANCEP(obj));
     ((obj)->storage_as.instance._data) = data;
}

INLINE lref_t INSTANCE_ELEM(lref_t obj, size_t index)
{
     checked_assert(INSTANCEP(obj));
     assert(index < INSTANCE_DIM(obj));
     return ((obj)->storage_as.instance._data)[index];
}

INLINE void SET_INSTANCE_ELEM(lref_t obj, size_t index, lref_t new_value)
{
     checked_assert(INSTANCEP(obj));
     assert(index < INSTANCE_DIM(obj));
     ((obj)->storage_as.instance._data)[index] = new_value;
}

INLINE lref_t INSTANCE_PROTO(lref_t obj)
{
     return INSTANCE_ELEM(obj, 0);
}

INLINE void SET_INSTANCE_PROTO(lref_t obj, lref_t proto)
{
     SET_INSTANCE_ELEM(obj, 0, proto);
}


  /*** port **/

enum port_mode_t
{
     PORT_CLOSED = 0x00,
     PORT_INPUT = 0x01,
     PORT_OUTPUT = 0x02,

     PORT_INPUT_OUTPUT = PORT_INPUT | PORT_OUTPUT,

     PORT_DIRECTION = PORT_INPUT | PORT_OUTPUT,

     PORT_BINARY = 0x08
};

struct port_text_translation_info_t
{
     int _unread_buffer[PORT_UNGET_BUFFER_SIZE];
     size_t _unread_valid;

     bool _crlf_translate;
     bool _needs_lf;

     fixnum_t _column;
     fixnum_t _row;

     fixnum_t _previous_line_length;
};

struct port_info_t
{
     lref_t _port_name;

     void *_user_data;
     lref_t _user_object;
     lref_t _fasl_table;

     lref_t _fasl_stack[FAST_LOAD_STACK_DEPTH];
     size_t _fasl_stack_ptr;
     lref_t _fasl_accum;

     port_mode_t _mode;

     port_text_translation_info_t *_text_info;

     size_t _bytes_read;
     size_t _bytes_written;
};

struct port_class_t
{
     const _TCHAR *_name;
     port_mode_t _valid_modes;

     void (*_open) (lref_t);

      bool(*_read_readyp) (lref_t);
      size_t(*_read) (void *, size_t, size_t, lref_t);

      size_t(*_write) (const void *, size_t, size_t, lref_t);
      bool(*_rich_write) (lref_t, bool, lref_t);

     int (*_flush) (lref_t);
     void (*_close) (lref_t);
     void (*_gc_free) (lref_t);

      size_t(*_length) (lref_t);
};

INLINE port_info_t *PORT_PINFO(lref_t x)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._pinf));
}

INLINE port_info_t *SET_PORT_PINFO(lref_t x, port_info_t * pinf)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._pinf)) = pinf;
}

INLINE port_class_t *PORT_CLASS(lref_t x)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._class));
}

INLINE port_class_t *SET_PORT_CLASS(lref_t x, port_class_t * klass)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._class)) = klass;
}

INLINE port_text_translation_info_t *PORT_TEXT_INFO(lref_t x)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->_text_info);
}

INLINE port_text_translation_info_t *SET_PORT_TEXT_INFO(lref_t x,
                                                        port_text_translation_info_t * text_info)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->_text_info) = text_info;
}

INLINE port_mode_t PORT_MODE(lref_t x)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->_mode);
}

INLINE void SET_PORT_MODE(lref_t x, port_mode_t mode)
{
     checked_assert(PORTP(x));
     (PORT_PINFO(x)->_mode) = mode;
}

INLINE bool PORT_BINARYP(lref_t x)
{
     return (PORT_TEXT_INFO(x) == NULL);
}

  /*** values-tuple ***/
INLINE lref_t VALUES_TUPLE_VALUES(lref_t vt)
{
     checked_assert(VALUES_TUPLE_P(vt));
     return ((*vt).storage_as.values_tuple._values);
}

INLINE void SET_VALUES_TUPLE_VALUES(lref_t vt, lref_t vals)
{
     checked_assert(VALUES_TUPLE_P(vt));
     ((*vt).storage_as.values_tuple._values) = vals;
}


  /*** fast op ***/
INLINE int FAST_OP_OPCODE(lref_t fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).header.opcode);
}

INLINE void SET_FAST_OP_OPCODE(lref_t fo, int opcode)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).header.opcode) = opcode;
}

INLINE lref_t FAST_OP_ARG1(lref_t fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).storage_as.fast_op.arg1);
}

INLINE void SET_FAST_OP_ARG1(lref_t fo, lref_t arg1)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).storage_as.fast_op.arg1) = arg1;
}

INLINE lref_t FAST_OP_ARG2(lref_t fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).storage_as.fast_op.arg2);
}

INLINE void SET_FAST_OP_ARG2(lref_t fo, lref_t arg2)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).storage_as.fast_op.arg2) = arg2;
}

INLINE lref_t FAST_OP_ARG3(lref_t fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).storage_as.fast_op.arg3);
}

INLINE void SET_FAST_OP_ARG3(lref_t fo, lref_t arg3)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).storage_as.fast_op.arg3) = arg3;
}

lref_t fast_op(int opcode, lref_t arg1, lref_t arg2, lref_t arg3);

/**** Input/Output ****/

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

/*  This is the 'universally availble' debugger output port. */
INLINE lref_t VM_DEBUG_PORT()
{
     return (&interp.debugger_output);
}

lref_t portcons(port_class_t * cls, lref_t port_name, port_mode_t mode, lref_t user_object,
              void *user_data);

size_t read_raw(void *buf, size_t size, size_t count, lref_t port);
size_t write_raw(const void *buf, size_t size, size_t count, lref_t port);

int read_char(lref_t port);
int unread_char(int ch, lref_t port);
int peek_char(lref_t port);
void write_char(int ch, lref_t port);
size_t write_text(const _TCHAR * buf, size_t count, lref_t port);


bool read_binary_fixnum(fixnum_t length, bool signedp, lref_t port, fixnum_t & result);
bool read_binary_flonum(lref_t port, flonum_t & result);

void scwritef(const _TCHAR * format_str, lref_t port, ...);

void dscwritef_impl(const _TCHAR * format_str, ...);

#define dscwritef(flag, args) do { if (DEBUG_FLAG(flag)) dscwritef_impl args; } while(0);

lref_t debug_print_object(lref_t exp, lref_t port, bool machine_readable);

void register_internal_file(internal_file_t *data);

lref_t open_c_data_input(internal_file_t *data);

typedef bool(*blocking_input_read_data_fn_t) (lref_t port, void *userdata);
typedef void (*blocking_input_close_port_fn_t) (lref_t port, void *userdata);

void blocking_input_post_data(lref_t port, void *data, size_t size);
void blocking_input_post_eof(lref_t port);
bool blocking_input_is_data_available(lref_t port);

lref_t blocking_input_cons(const _TCHAR * port_name, bool binary,
                         blocking_input_read_data_fn_t read_fn,
                         blocking_input_close_port_fn_t close_fn, void *userdata);

  /****************************************************************
                         The C API
  ****************************************************************/

  /****** Startup/Shutdown, and custom extension */

void init0(int argc, _TCHAR * argv[], debug_flag_t initial_debug_flags);
void init(int argc, _TCHAR * argv[], debug_flag_t initial_debug_flags);

void signal_interrupt(vminterrupt_t intr);

void shutdown();

const _TCHAR *build_id_string();

void register_subr(const _TCHAR * name, subr_arity_t arity, void *implementation);
lref_t find_subr_by_name(lref_t subr_name);
lref_t run();

  /****** Evaluator and Loader */

lref_t apply1(lref_t fn, size_t argc, lref_t argv[]);

  /****** Error handling and control */

enum vmt_options_t {
     /* Use one of these... */
     VMT_MANDATORY_TRAP      = 0x0,
     VMT_OPTIONAL_TRAP       = 0x1,

     /* ...and optionally this. */
     VMT_HANDLER_MUST_ESCAPE = 0x2
};

lref_t vmtrap(trap_type_t trap, vmt_options_t options, size_t argc, ...);

void vmerror_wrong_type(lref_t new_errobj);
void vmerror_wrong_type(int which_argument, lref_t new_errobj);
void vmerror_unbound(lref_t v);
void vmerror_index_out_of_bounds(lref_t index, lref_t obj);
void vmerror_arg_out_of_range(lref_t arg, const _TCHAR *range_desc = NULL);
void vmerror_unsupported(const _TCHAR *desc);
void vmerror_unimplemented(const _TCHAR *desc);
void vmerror_divide_by_zero();
void vmerror_io_error(const _TCHAR *desc, lref_t info);
void fast_read_error(const _TCHAR * message, lref_t port, lref_t details = NIL);

void vmerror_stack_overflow(uint8_t * obj);

  /****** Memory management */

void gc_initialize_heap();
void gc_release_heap();

void gc_protect(const _TCHAR * name, lref_t * location, size_t n);

void gc_mark(lref_t obj);

lref_t gc_claim_freelist();

/***** Time *****/
flonum_t time_since_launch();

/***** Prototypes for C Primitives *****/

lref_t lacos(lref_t x);
lref_t ladd(lref_t x, lref_t y);
lref_t ladd_symbol_to_package(lref_t symbol, lref_t package);
lref_t langle(lref_t cmplx);
lref_t lapply(size_t argc, lref_t argv[]);
lref_t lasin(lref_t x);
lref_t latan(lref_t x, lref_t y);
lref_t lbinary_portp(lref_t obj);
lref_t lbinary_write_flonum(lref_t v, lref_t port);
lref_t lbitwise_and(lref_t x, lref_t y);
lref_t lbitwise_ashr(lref_t x, lref_t n);
lref_t lbitwise_not(lref_t x);
lref_t lbitwise_or(lref_t x, lref_t y);
lref_t lbitwise_shl(lref_t x, lref_t n);
lref_t lbitwise_shr(lref_t x, lref_t n);
lref_t lbitwise_xor(lref_t x, lref_t y);
lref_t lbooleanp(lref_t x);
lref_t lcar(lref_t x);
lref_t lcdr(lref_t x);
lref_t lceiling(lref_t x);
lref_t lchar2integer(lref_t s);
lref_t lchar_readyp(lref_t port);
lref_t lcharacter2string(lref_t obj);
lref_t lcharp(lref_t x);
lref_t lclone_c_data_port(lref_t port);
lref_t lclone_instance(lref_t inst);
lref_t lclose_port(lref_t port);
lref_t lclosure_code(lref_t exp);
lref_t lclosure_env(lref_t exp);
lref_t lclosurecons(lref_t env, lref_t code, lref_t property_list);
lref_t lclosurep(lref_t obj);
lref_t lcomplexp(lref_t x);
lref_t lconsp(lref_t x);
lref_t lcopy_structure(lref_t st);
lref_t lcos(lref_t x);
lref_t ldebug_flags();
lref_t ldebug_write(lref_t form);
lref_t ldelete_file(lref_t filename);
lref_t ldisplay_to_string(lref_t exp);
lref_t ldivide(lref_t x, lref_t y);
lref_t ldo_external_symbols(lref_t args, lref_t env);
lref_t ldo_symbols(lref_t args, lref_t env);
lref_t ldump_heap_state(lref_t port);
lref_t lenlarge_heap(lref_t count);
lref_t lenvironment();
lref_t lenvlookup(lref_t var, lref_t env);
lref_t leof_objectp(lref_t obj);
lref_t leq(lref_t x, lref_t y);
lref_t leql(lref_t x, lref_t y);
lref_t lequal(lref_t, lref_t);
lref_t lexact2inexact(lref_t x);
lref_t lexactp(lref_t x);
lref_t lexp(lref_t x);
lref_t lexpt(lref_t x, lref_t y);
lref_t lfast_op(lref_t opcode, lref_t arg1, lref_t arg2, lref_t arg3);
lref_t lfast_op_args(lref_t fastop);
lref_t lfast_op_opcode(lref_t fastop);
lref_t lfast_read(lref_t port);
lref_t lfloor(lref_t x);
lref_t lflush_port(lref_t port);
lref_t lflush_whitespace(lref_t port, lref_t slc);
lref_t lfresh_line(lref_t port);
lref_t lgc();
lref_t lgc_info();
lref_t lgc_runtime();
lref_t lgc_status(lref_t new_gc_status);
lref_t lget_current_frames(lref_t skip_count);
lref_t lget_output_string(lref_t port);
lref_t lhandler_frames();
lref_t lhas_slotp(lref_t this_obj, lref_t key);
lref_t lhash2alist(lref_t hash);
lref_t lhash2list(lref_t hash);
lref_t lhash_clear(lref_t hash);
lref_t lhash_copy(lref_t hash);
lref_t lhash_hasp(lref_t table, lref_t key);
lref_t lhash_key(lref_t obj);
lref_t lhash_ref(size_t argc, lref_t argv[]);
lref_t lhash_refs(lref_t table, lref_t key);
lref_t lhash_remove(lref_t table, lref_t key);
lref_t lhash_set(lref_t table, lref_t key, lref_t value);
lref_t lhash_type(lref_t hash);
lref_t lhashp(lref_t obj);
lref_t lheap_cell_count_by_typecode();
lref_t liarm_gc_trip_wires(lref_t f);
lref_t licontrol_field(lref_t control_field_id);
lref_t lidebug_printer(lref_t obj, lref_t port, lref_t machine_readable_p);
lref_t lidefine_global(lref_t var, lref_t val);
lref_t lidirectory(lref_t dirname, lref_t mode);
lref_t lieee754_bits_to(lref_t x);
lref_t lifile_details(lref_t path, lref_t existance_onlyp);
lref_t ligc_trip_wire();
lref_t lihash_binding_vector(lref_t hash);
lref_t liifasl_load(lref_t port);
lref_t liimmediate_p(lref_t obj);
lref_t liinstance_map(lref_t inst);
lref_t liinstance_proto(lref_t instance);
lref_t liinstance_slots(lref_t instance);
lref_t liinternal_files();
lref_t liload(lref_t fname);
lref_t limacrocons(lref_t t);
lref_t limag_part(size_t argc, lref_t argv[]);
lref_t linexact2display_string(lref_t n, lref_t sf, lref_t sci, lref_t s);
lref_t linexact2exact(lref_t x);
lref_t linexactp(lref_t x);
lref_t linfinitep(lref_t x);
lref_t linput_portp(lref_t obj);
lref_t linstancep(lref_t obj);
lref_t linteger2char(lref_t s);     /*  REVISIT: rename to exact->char */
lref_t lintegerp(lref_t x);
lref_t lipackagecons(lref_t name);
lref_t liset_control_field(lref_t control_field_id, lref_t new_value);
lref_t liset_instance_proto(lref_t instance, lref_t new_proto);
lref_t liset_trap_handler(lref_t trap_id, lref_t new_handler);
lref_t lislot_ref(lref_t obj, lref_t key);
lref_t lislot_set(lref_t obj, lref_t key, lref_t value);
lref_t lisp_strcmp(lref_t s1, lref_t s2);
lref_t listartup_args();
lref_t lisubr_table();
lref_t lisymbol_globally_boundp(lref_t sym);
lref_t lisymbol_index(lref_t symbol);
lref_t litrap_handler(lref_t trap_id);
lref_t litypecode(lref_t obj);
lref_t lkeywordp(lref_t x);
lref_t llast(lref_t);
lref_t llength(lref_t obj);
lref_t llisp_heap_stress_thread(lref_t t, lref_t c, lref_t s);
lref_t llist(lref_t l);
lref_t llist2hash(lref_t obj);
lref_t llist2vector(lref_t l);
lref_t llog(lref_t x);
lref_t lmacro_transformer(lref_t mac);
lref_t lmacrop(lref_t obj);
lref_t lmagnitude(lref_t cmplx);
lref_t lmake_eof();
lref_t lmake_hash(lref_t key_type);
lref_t lmake_instance(lref_t args);
lref_t lmake_polar(lref_t r, lref_t theta);
lref_t lmake_rectangular(lref_t re, lref_t im);
lref_t lmake_vector(lref_t dim, lref_t initial);
lref_t lmemref_byte(lref_t addr);
lref_t lmodulo(lref_t x, lref_t y);
lref_t lmultiply(lref_t x, lref_t y);
lref_t lnanp(lref_t x);
lref_t lnewline(lref_t);
lref_t lnotp(lref_t x);
lref_t lnullp(lref_t x);
lref_t lnum_eq(size_t argc, lref_t argv[]);
lref_t lnum_ge(size_t argc, lref_t argv[]);
lref_t lnum_gt(size_t argc, lref_t argv[]);
lref_t lnum_le(size_t argc, lref_t argv[]);
lref_t lnum_lt(size_t argc, lref_t argv[]);
lref_t lnumber2string(lref_t x, lref_t r, lref_t s, lref_t p);
lref_t lnumberp(lref_t x);
lref_t lobaddr(lref_t object);
lref_t lopen_debug_port();
lref_t lopen_input_file(lref_t filename, lref_t mode);
lref_t lopen_input_string(lref_t string);
lref_t lopen_null_port();
lref_t lopen_output_file(lref_t filename, lref_t mode);
lref_t lopen_output_string();
lref_t loutput_portp(lref_t obj);
lref_t lpackagcons(lref_t name);
lref_t lpackage_bindings(lref_t p);
lref_t lpackage_name(lref_t p);
lref_t lpackage_use_list(lref_t p);
lref_t lpackagep(lref_t x);
lref_t lpanic(lref_t msg);
lref_t lpeek_char(lref_t port);
lref_t lport_io_counts(lref_t port);
lref_t lport_location(lref_t port);
lref_t lport_mode(lref_t obj);
lref_t lport_name(lref_t port);
lref_t lport_set_translate_mode(lref_t port, lref_t mode);
lref_t lport_translate_mode(lref_t port);
lref_t lprimitivep(lref_t obj);
lref_t lprocedurep(lref_t exp);
lref_t lproperty_list(lref_t exp);
lref_t lqsort(lref_t l, lref_t f, lref_t g);
lref_t lquotient(lref_t x, lref_t y);
lref_t lrandom(lref_t n);
lref_t lrationalp(lref_t x);
lref_t lread_binary_fixnum(lref_t l, lref_t sp, lref_t port);
lref_t lread_binary_flonum(lref_t port);
lref_t lread_binary_string(lref_t l, lref_t port);
lref_t lread_char(lref_t port);
lref_t lread_line(lref_t port);
lref_t lread_port_to_string(lref_t port);
lref_t lreal_part(lref_t cmplx);
lref_t lrealp(lref_t x);
lref_t lrealtime(void);
lref_t lrealtime_time_zone_offset();
lref_t lremainder(lref_t x, lref_t y);
lref_t lrepresentation_of(lref_t obj);
lref_t lrich_write(lref_t obj, lref_t machine_readable, lref_t port);
lref_t lround(lref_t x);
lref_t lruntime(void);
lref_t lsend(lref_t args);
lref_t lset_closure_code(lref_t exp, lref_t code);
lref_t lset_closure_env(lref_t exp, lref_t env);
lref_t lset_debug_flags(lref_t c);
lref_t lset_environment_variable(lref_t varname, lref_t value);
lref_t lset_fasl_package_list(lref_t packages);
lref_t lset_handler_frames(lref_t new_frames);
lref_t lset_interrupt_mask(lref_t new_mask);
lref_t lset_package_name(lref_t p, lref_t new_name);
lref_t lset_package_use_list(lref_t p, lref_t use_list);
lref_t lset_property_list(lref_t exp, lref_t property_list);
lref_t lset_random_seed(lref_t s);
lref_t lset_stack_limit(lref_t);
lref_t lset_symbol_package(lref_t sym, lref_t package);
lref_t lset_symbol_vcell(lref_t sym, lref_t val);
lref_t lsetcar(lref_t cell, lref_t value);
lref_t lsetcdr(lref_t cell, lref_t value);
lref_t lsin(lref_t x);
lref_t lsleep(lref_t ms);
lref_t lsqrt(lref_t x);
lref_t lstress_c_heap(lref_t c, lref_t s);
lref_t lstress_lisp_heap(lref_t c);
lref_t lstring2number(lref_t, lref_t);
lref_t lstring2uninterned_symbol(lref_t str);
lref_t lstring_append(size_t argc, lref_t argv[]);
lref_t lstring_copy(lref_t string);
lref_t lstring_downcase(lref_t);
lref_t lstring_downcased(lref_t);
lref_t lstring_first_char(lref_t string, lref_t char_set, lref_t initial_ofs);
lref_t lstring_first_substring(lref_t string, lref_t char_set, lref_t initial_ofs);
lref_t lstring_length(lref_t string);
lref_t lstring_ref(lref_t a, lref_t i);
lref_t lstring_search(lref_t token, lref_t str, lref_t maybe_from);
lref_t lstring_search_from_right(lref_t tok, lref_t str, lref_t maybe_from);
lref_t lstring_set(lref_t a, lref_t i, lref_t v);
lref_t lstring_trim(lref_t, lref_t);
lref_t lstring_trim_left(lref_t, lref_t);
lref_t lstring_trim_right(lref_t, lref_t);
lref_t lstring_upcase(lref_t);
lref_t lstring_upcased(lref_t);
lref_t lstringp(lref_t x);
lref_t lstructure_layout(lref_t st);
lref_t lstructure_length(lref_t st);
lref_t lstructure_ref(lref_t st, lref_t index);
lref_t lstructure_set(lref_t st, lref_t index, lref_t value);
lref_t lstructurecons(lref_t slots, lref_t layout);
lref_t lstructurep(lref_t st, lref_t expected_layout);
lref_t lsubr_name(lref_t subr);
lref_t lsubr_type_code(lref_t subr);
lref_t lsubset(lref_t fcn, lref_t l);
lref_t lsubstring(lref_t, lref_t, lref_t);
lref_t lsubtract(lref_t x, lref_t y);
lref_t lsxhash(lref_t obj, lref_t hash);
lref_t lsymbol_name(lref_t sym);
lref_t lsymbol_name(lref_t sym);
lref_t lsymbol_package(lref_t sym);
lref_t lsymbol_vcell(lref_t sym);
lref_t lsymbolp(lref_t x);
lref_t lsysob(lref_t addr);
lref_t lsystem(size_t argc, lref_t argv[]);
lref_t lsystem_info();
lref_t ltan(lref_t x);
lref_t ltemporary_file_name(lref_t prefix);
lref_t ltest_blocking_input(lref_t block_size, lref_t length, lref_t binary);
lref_t ltime_apply0(lref_t fn);
lref_t lto_ieee754_bits(lref_t x);
lref_t ltruncate(lref_t x);
lref_t lunbound_marker();
lref_t lunread_char(lref_t ch, lref_t port);
lref_t lvalues(lref_t values);
lref_t lvalues2list(lref_t obj);
lref_t lvector(size_t argc, lref_t argv[]);
lref_t lvector2list(lref_t vec);
lref_t lvector_copy(lref_t vec);
lref_t lvector_fill(lref_t vec, lref_t v);
lref_t lvector_ref(lref_t a, lref_t i, lref_t d);
lref_t lvector_resize(lref_t vec, lref_t new_size, lref_t new_element);
lref_t lvector_set(lref_t a, lref_t i, lref_t v);
lref_t lvectorp(lref_t obj);
lref_t lwrite_binary_fixnum(lref_t v, lref_t l, lref_t sp, lref_t port);
lref_t lwrite_binary_string(lref_t string, lref_t port);
lref_t lwrite_char(lref_t ch, lref_t port);
lref_t lwrite_strings(size_t argc, lref_t argv[]);
lref_t lwrite_to_string(lref_t exp);


#if defined(WITH_FOPLOG_SUPPORT)
lref_t lifoplog_reset();
lref_t lifoplog_enable(lref_t enablep);
lref_t lifoplog_snapshot();
#endif

bool equalp(lref_t, lref_t);
double round(double n);

void scan_postmortem_dump();

/***** Debugging tools *****/

INLINE bool DEBUG_FLAG(debug_flag_t flag)
{
     if (!DEBUGGING_BUILD)
          return false;

     return ((fixnum_t) flag == (interp.debug_flags & (fixnum_t) flag));
}

bool parse_string_as_fixnum(_TCHAR * string, int radix, fixnum_t & result);

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

void init_debugger_output();
void init_stdio_ports();

extern port_class_t stderr_port_class;

lref_t initialize_port(lref_t s,
                     port_class_t * cls,
                     lref_t port_name, port_mode_t mode, lref_t user_object, void *user_data);


size_t object_length(lref_t obj);
size_t hash_length(lref_t hash);
size_t port_length(lref_t port);


debug_flag_t debug_flags_from_string(debug_flag_t initial, const _TCHAR * source_name,
                                     const _TCHAR * str);
debug_flag_t debug_flags_from_environment(debug_flag_t initial);

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

lref_t topmost_primitive();

END_NAMESPACE;

#endif
