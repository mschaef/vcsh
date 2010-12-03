
/* scan.h
 *
 * Interpreter Header File
 */

#ifndef __SCAN_H
#define __SCAN_H

#include "../util/base-types.h"
#include "../util/base-assert.h"
#include "../util/base-tchar.h"
#include "sys.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>

BEGIN_NAMESPACE(scan)

#define CONST_C_HEADER
#include "constants.i"
#undef CONST_C_HEADER

extern i64_t malloc_bytes;      /* REVISIT: Should this be u64_t? */

/*** Interpreter Paramaters ***/

#define SCAN_VERSION _T("SCAN 0.50")

enum
{
     /*  Default size of a heap segment, in cells */
     DEFAULT_HEAP_SEGMENT_SIZE = 262144,

     /*  Default limit on the Maximum number of heap segments */
     DEFAULT_MAX_HEAP_SEGMENTS = 256,

     /*  Default size for FASL loader tables */
     DEFAULT_FASL_TABLE_SIZE = 8192,

     /*  Local (stack) string buffer size */
     STACK_STRBUF_LEN = 256,

     /* The number of characters that can be ungotten from a port */
     PORT_UNGET_BUFFER_SIZE = 8,

     /*  Record individual safe_mallocs to debug */
     DETAILED_MEMORY_LOG = FALSE,

     /*  Garbage collect on each call to newCell (Very slow) */
     ALWAYS_GC = FALSE,

     /*  The default allocation unit for global environment vectors. */
     GLOBAL_ENV_BLOCK_SIZE = 4096,

     /*  The depth of the stack the FASL loader uses to store load unit state */
     FAST_LOAD_STACK_DEPTH = 16,

     /*  The number of arguments contained in argment buffers */
     ARG_BUF_LEN = 32,

     /*  The number of LRef's that can be stored on the value stack. */
     VALUE_STACK_SIZE = 2048,

     /*  The number of cells on a sub-freelist */
     SUB_FREELIST_SIZE = 1024,

     /*  The maximum number of GC roots per thread */
     MAX_GC_ROOTS = 32,

     /*  The debug printer's flonum precisionn */
     DEBUG_FLONUM_PRINT_PRECISION = 8,

     /* The maximum number of init load files. */
     MAX_INIT_LOAD_FILES = 8,

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

/* _The_ type *************************************************
 *
 * This is the structure of a Lisp cell. The root of every Lisp
 * object is one of these... */

/* ...Forward declarations and typedefs... */

struct port_class_t;
struct port_info_t;
struct LObject;
typedef LObject *LRef;

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

#define UNBOUND_MARKER ((LRef)LREF2_UNBOUND)

enum
{
     /*  REVISIT: change to 'IPTR_MAX/MIN' */
     MAX_LREF_FIXNUM = I32_MAX >> LREF1_TAG_SHIFT,
     MIN_LREF_FIXNUM = I32_MIN >> LREF1_TAG_SHIFT,
};

INLINE LRef LREF1_CONS(lref_tag_t tag, iptr_t val)
{
     return (LRef) ((val << LREF1_TAG_SHIFT) | tag);
}

INLINE LRef LREF2_CONS(lref_tag_t tag, iptr_t val)
{
     return (LRef) ((val << LREF2_TAG_SHIFT) | tag);
}

INLINE lref_tag_t LREF1_TAG(LRef ref)
{
     return (lref_tag_t) ((iptr_t) ref & LREF1_TAG_MASK);
}

INLINE lref_tag_t LREF2_TAG(LRef ref)
{
     return (lref_tag_t) ((iptr_t) ref & LREF2_TAG_MASK);
}

INLINE iptr_t LREF1_VAL(LRef ref)
{
     return ((iptr_t) ref & ~LREF1_TAG_MASK) >> LREF1_TAG_SHIFT;
}

INLINE iptr_t LREF2_VAL(LRef ref)
{
     return ((iptr_t) ref & ~LREF2_TAG_MASK) >> LREF2_TAG_SHIFT;
}

INLINE bool LREF_IMMEDIATE_P(LRef ref)
{
     return LREF1_TAG(ref) != LREF1_REF;
}

typedef LRef(*f_0_t) (void);
typedef LRef(*f_1_t) (LRef);
typedef LRef(*f_2_t) (LRef, LRef);
typedef LRef(*f_3_t) (LRef, LRef, LRef);
typedef LRef(*f_4_t) (LRef, LRef, LRef, LRef);
typedef LRef(*f_5_t) (LRef, LRef, LRef, LRef, LRef);
typedef LRef(*f_6_t) (LRef, LRef, LRef, LRef, LRef, LRef);
typedef LRef(*f_m_t) (LRef *, LRef *);
typedef LRef(*f_f_t) (LRef, LRef);
typedef LRef(*f_argc_t) (size_t, LRef[]);

/*
 * ...This is the boxed object type...
 */

struct hash_entry_t
{
     LRef _key;                 /*  == UNBOUND_MARKER for empty. */
     LRef _val;
};

#pragma pack(push, 4)
struct LObject
{
     struct
     {
          typecode_t type:8;
          unsigned int opcode:8;
          unsigned int gc_mark:1;       /*  REVISIT: multiple bits, for shallow/weak refs */
#if defined(__LP64__)
          unsigned int pad:32;  /*  Explicit pad to keep the LP64 header the same size as an LP64 pointer. */
#endif
     } header;

     union
     {
          struct
          {
               LRef car;
               LRef cdr;
          } cons;
          struct
          {
               fixnum_t data;
          } fixnum;
          struct
          {
               flonum_t data;
               LRef im_part;
          } flonum;
          struct
          {
               LRef props;
               size_t env_index;
               LRef home;
          } symbol;
          struct
          {
               LRef name;
               LRef symbol_bindings;
               LRef use_list;
          } package;
          struct
          {
               LRef env;
               LRef code;
               LRef property_list;
          } closure;
          struct
          {
               LRef transformer;
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
               LRef *data;
               LRef layout;
          } vector;
          struct
          {
               LRef name;
               size_t dim;
               LRef *data;
          } genv;
          struct
          {
               LRef _map;
               size_t _dim;
               LRef *_data;
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
               LRef _values;
          } values_tuple;
          struct
          {
               LRef arg1;
               LRef arg2;
               LRef arg3;
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
               LRef name;
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

const LRef NIL = ((LObject *) 0);

INLINE bool EQ(LRef x, LRef y)
{
     return x == y;
};

INLINE bool NULLP(LRef x)
{
     return EQ(x, NIL);
};

INLINE void SET_GC_MARK(LRef object, int new_gc_mark_bit)
{
     checked_assert(!LREF_IMMEDIATE_P(object));

     object->header.gc_mark = new_gc_mark_bit;
}

INLINE int GC_MARK(LRef object)
{
     return object->header.gc_mark;
}

INLINE typecode_t TYPE(LRef object)
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

INLINE void SET_TYPE(LRef object, typecode_t new_type)
{
     checked_assert(!LREF_IMMEDIATE_P(object));

     object->header.type = new_type;
}

INLINE bool TYPEP(LRef object, typecode_t typeCode)
{
     return TYPE(object) == typeCode;
}

/*** Debugging flags ***/
struct frame_record_t
{
     frame_record_t *prev;

     frame_type_t type;

     union
     {
          struct
          {
               LRef *form;
               LRef initial_form;
               LRef env;
          } eval;
          struct
          {
               LRef tag;
               LRef retval;
               jmp_buf cframe;
               bool pending;
               bool unwinding;
          } escape;
          struct
          {
               LRef function;
          } prim;
          struct
          {
               LRef tag;
          } marker;
     } as;
};

struct gc_root_t
{
     const _TCHAR *name;
     LRef *location;
     size_t length;
};


struct interpreter_thread_info_block_t
{
     LRef freelist;
     void *stack_base;
     gc_root_t gc_roots[MAX_GC_ROOTS];

     frame_record_t *frame_stack;
     LRef handler_frames;

     LRef vstack[VALUE_STACK_SIZE];
     size_t vsp;
};

struct interpreter_t
{
     // TODO: per-interrupt masking
     // TODO: interrupt handler per bit.
     vminterrupt_t interrupts_pending;

     bool interrupts_masked;

     bool gc_trip_wires_armed;

     bool shutting_down;

     LRef global_env;
     size_t last_global_env_entry;

     size_t init_load_file_count;
     _TCHAR *init_load_file_name[MAX_INIT_LOAD_FILES];

     size_t gc_heap_segment_size;
     size_t gc_max_heap_segments;
     size_t gc_current_heap_segments;
     LRef *gc_heap_segments;

     LRef global_freelist;

     flonum_t launch_realtime;

     LRef control_fields[VMCTRL_LAST + 1];

     LRef fasl_package_list;

     LRef base_instance;

     LRef internal_files;
     LRef subr_table;
     LRef startup_args;

     LRef trap_handlers[TRAP_LAST + 1];

     /*  A statically allocated LObject used to hold a debugger output port.
      *  This is intended to be available before the GC heap is operational,
      *  so it has to be located here, and not on the heap. */
     LObject debugger_output;

     /*  Statistics Counters */
     fixnum_t gc_total_cells_allocated;
     fixnum_t gc_cells_collected;

     fixnum_t malloc_bytes_at_last_gc;
     fixnum_t malloc_blocks_at_last_gc;
     fixnum_t c_bytes_gc_threshold;

     flonum_t gc_total_run_time;
     flonum_t gc_run_time;

     debug_flag_t debug_flags;

     interpreter_thread_info_block_t thread;
};

extern interpreter_t interp;    /*  One interpter... one global state variable. */

/**** Boxed types ****/

/* ...Type Predicates... */

inline /* full INLINE causes problems with gcc 3.4.4, due to prototype. */ LRef FLOIM(LRef x);

INLINE bool FREE_CELL_P(LRef x)
{
     return TYPEP(x, TC_FREE_CELL);
}

INLINE bool CHARP(LRef x)
{
     return TYPEP(x, TC_CHARACTER);
}

INLINE bool BOOLP(LRef x)
{
     return TYPEP(x, TC_BOOLEAN);
}

INLINE bool CONSP(LRef x)
{
     return TYPEP(x, TC_CONS);
}

INLINE bool SYMBOLP(LRef x)
{
     return TYPEP(x, TC_SYMBOL);
}

INLINE bool FIXNUMP(LRef x)
{
     return TYPEP(x, TC_FIXNUM);
}

INLINE bool FLONUMP(LRef x)
{
     return TYPEP(x, TC_FLONUM);
}

INLINE bool REALP(LRef x)
{
     return (FIXNUMP(x) || (FLONUMP(x) && NULLP(FLOIM(x))));
}

INLINE bool COMPLEXP(LRef x)
{
     return (FLONUMP(x) && !NULLP(FLOIM(x)));
}

INLINE bool STRINGP(LRef x)
{
     return TYPEP(x, TC_STRING);
}

INLINE bool NUMBERP(LRef x)
{
     return (FIXNUMP(x) || FLONUMP(x));
}

INLINE bool PACKAGEP(LRef x)
{
     return TYPEP(x, TC_PACKAGE);
}

INLINE bool PORTP(LRef x)
{
     return TYPEP(x, TC_PORT);
}

INLINE bool VECTORP(LRef x)
{
     return TYPEP(x, TC_VECTOR);
}

INLINE bool STRUCTUREP(LRef x)
{
     return TYPEP(x, TC_STRUCTURE);
}

INLINE bool HASHP(LRef x)
{
     return TYPEP(x, TC_HASH);
}

INLINE bool CLOSUREP(LRef x)
{
     return TYPEP(x, TC_CLOSURE);
}

INLINE bool SUBRP(LRef x)
{
     return TYPEP(x, TC_SUBR);
}

INLINE bool PROCEDUREP(LRef x)
{
     return CLOSUREP(x) || SUBRP(x);
}

INLINE bool MACROP(LRef x)
{
     return TYPEP(x, TC_MACRO);
}

INLINE bool VALUES_TUPLE_P(LRef x)
{
     return TYPEP(x, TC_VALUES_TUPLE);
}

INLINE bool EOFP(LRef x)
{
     return TYPEP(x, TC_END_OF_FILE);
}

INLINE bool INSTANCEP(LRef x)
{
     return TYPEP(x, TC_INSTANCE);
}

INLINE bool UNBOUND_MARKER_P(LRef x)
{
     return EQ(x, UNBOUND_MARKER);
}

INLINE bool GC_TRIP_WIRE_P(LRef x)
{
     return TYPEP(x, TC_GC_TRIP_WIRE);
}

INLINE bool FAST_OP_P(LRef x)
{
     return TYPEP(x, TC_FAST_OP);
}

INLINE bool GENVP(LRef x)
{
     return TYPEP(x, TC_GENV);
}

INLINE bool TRUEP(LRef x)
{
     return (x) != LREF2_CONS(LREF2_BOOL, 0);
}

INLINE bool FALSEP(LRef x)
{
     return !TRUEP(x);
}


/*** Boxed object accessors and constructors ***/

/*  REVISIT: Seperate out setter accessors */

/*** boolean **/
INLINE LRef boolcons(bool val)
{
     return LREF2_CONS(LREF2_BOOL, val ? 1 : 0);
}


INLINE bool BOOLV(LRef x)
{
     checked_assert(BOOLP(x));

     return LREF2_VAL(x) != 0;
}

  /*** cons/free-cell **/
LRef listn(long n, ...);
LRef listv(long n, va_list args);
LRef lista(size_t n, LRef args[]);


INLINE LRef & _CAR(LRef x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.car);
}

INLINE LRef CAR(LRef x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.car);
}

INLINE void SET_CAR(LRef x, LRef nv)
{
     checked_assert(CONSP(x));
     ((*x).storage_as.cons.car) = nv;
}

INLINE LRef & _CDR(LRef x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.cdr);
}

INLINE LRef CDR(LRef x)
{
     checked_assert(CONSP(x));
     return ((*x).storage_as.cons.cdr);
}

INLINE void SET_CDR(LRef x, LRef nv)
{
     checked_assert(CONSP(x));
     ((*x).storage_as.cons.cdr) = nv;
}


INLINE LRef NEXT_FREE_LIST(LRef x)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     return ((*x).storage_as.cons.car);
}

INLINE LRef SET_NEXT_FREE_LIST(LRef x, LRef next)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     ((*x).storage_as.cons.car) = next;

     return x;
}

INLINE LRef NEXT_FREE_CELL(LRef x)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     return ((*x).storage_as.cons.cdr);
}

INLINE LRef SET_NEXT_FREE_CELL(LRef x, LRef next)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     ((*x).storage_as.cons.cdr) = next;

     return x;
}

  /*** fix/flonum **/
#define fixabs labs

LRef fixcons(u32_t high, u32_t low);
LRef fixcons(fixnum_t x);
LRef flocons(double x);
LRef cmplxcons(flonum_t re, flonum_t im);

fixnum_t get_c_fixnum(LRef x);
long get_c_long(LRef x);
double get_c_double(LRef x);
flonum_t get_c_flonum(LRef x);
flonum_t get_c_flonum_im(LRef x);
bool get_c_port_mode(LRef mode);

INLINE fixnum_t & _FIXNM(LRef x)
{
     checked_assert(FIXNUMP(x));

     return ((*x).storage_as.fixnum.data);
}

INLINE fixnum_t FIXNM(LRef x)
{
     checked_assert(FIXNUMP(x));

     if (LREF1_TAG(x) == LREF1_FIXNUM)
          return LREF1_VAL(x);

     return ((*x).storage_as.fixnum.data);
}

INLINE flonum_t FLONM(LRef x)
{
     checked_assert(FLONUMP(x));
     return ((*x).storage_as.flonum.data);
}

INLINE void SET_FLONM(LRef x, double val)
{
     checked_assert(FLONUMP(x));
     ((*x).storage_as.flonum.data) = val;
}

inline /* full INLINE causes problems with gcc 3.4.4, due to prototype. */ LRef FLOIM(LRef x)
{
     checked_assert(FLONUMP(x));

     return ((*x).storage_as.flonum.im_part);
}

INLINE void SET_FLOIM(LRef x, LRef val)
{
     checked_assert(FLONUMP(x));

     ((*x).storage_as.flonum.im_part) = val;
}

INLINE flonum_t CMPLXRE(LRef x)
{
     return FLONM(x);
}

INLINE flonum_t CMPLXIM(LRef x)
{
     return FLONM(FLOIM(x));
}

 /*** character **/
LRef charcons(_TCHAR ch);

INLINE _TCHAR CHARV(LRef x)
{
     checked_assert(CHARP(x));

     return (_TCHAR) LREF2_VAL(x);
}

  /*** vector **/
LRef vector_resize(LRef vec, size_t new_size, LRef new_element);

LRef vectorcons(fixnum_t n, LRef initial = NIL);

INLINE size_t VECTOR_DIM(LRef obj)
{
     checked_assert(VECTORP(obj));
     return ((obj)->storage_as.vector.dim);
}

INLINE void SET_VECTOR_DIM(LRef obj, size_t new_dim)
{
     checked_assert(VECTORP(obj));
     ((obj)->storage_as.vector.dim) = new_dim;
}

INLINE LRef *VECTOR_DATA(LRef obj)
{
     checked_assert(VECTORP(obj));
     return ((obj)->storage_as.vector.data);
}

INLINE LRef *SET_VECTOR_DATA(LRef obj, LRef * new_data)
{
     checked_assert(VECTORP(obj));
     return ((obj)->storage_as.vector.data) = new_data;
}

INLINE LRef VECTOR_ELEM(LRef vec, fixnum_t index)
{
     checked_assert(VECTORP(vec));
     return ((vec)->storage_as.vector.data[(index)]);
}

INLINE LRef & _VECTOR_ELEM(LRef vec, fixnum_t index)
{
     checked_assert(VECTORP(vec));
     return ((vec)->storage_as.vector.data[(index)]);
}

INLINE void SET_VECTOR_ELEM(LRef vec, fixnum_t index, LRef new_value)
{
     checked_assert(VECTORP(vec));
     ((vec)->storage_as.vector.data[(index)]) = new_value;
}

LRef genvcons(size_t dim, LRef name);

INLINE LRef GENV_NAME(LRef obj)
{
     checked_assert(GENVP(obj));
     return ((obj)->storage_as.genv.name);
}

INLINE void SET_GENV_NAME(LRef obj, LRef new_name)
{
     checked_assert(GENVP(obj));
     ((obj)->storage_as.genv.name) = new_name;
}

INLINE size_t GENV_DIM(LRef obj)
{
     checked_assert(GENVP(obj));
     return ((obj)->storage_as.genv.dim);
}

INLINE void SET_GENV_DIM(LRef obj, size_t new_dim)
{
     checked_assert(GENVP(obj));
     ((obj)->storage_as.genv.dim) = new_dim;
}

INLINE LRef *GENV_DATA(LRef obj)
{
     checked_assert(GENVP(obj));
     return ((obj)->storage_as.genv.data);
}

INLINE LRef *SET_GENV_DATA(LRef obj, LRef * new_data)
{
     checked_assert(GENVP(obj));
     return ((obj)->storage_as.genv.data) = new_data;
}

INLINE LRef GENV_ELEM(LRef vec, fixnum_t index)
{
     checked_assert(GENVP(vec));
     return ((vec)->storage_as.genv.data[(index)]);
}

INLINE void SET_GENV_ELEM(LRef vec, fixnum_t index, LRef new_value)
{
     checked_assert(GENVP(vec));
     ((vec)->storage_as.genv.data[(index)]) = new_value;
}
                                                                                                                                                                              /*** structure ***//*  REVISIT:  how much of the structure representation can be shared with vectors? */

INLINE size_t STRUCTURE_DIM(LRef obj)
{
     checked_assert(STRUCTUREP(obj));
     return ((obj)->storage_as.vector.dim);
}

INLINE void SET_STRUCTURE_DIM(LRef obj, size_t len)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.dim) = len;
}

INLINE void SET_STRUCTURE_DATA(LRef obj, LRef * data)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.data) = data;
}


INLINE LRef STRUCTURE_LAYOUT(LRef obj)
{
     checked_assert(STRUCTUREP(obj));
     return ((obj)->storage_as.vector.layout);
}

INLINE void SET_STRUCTURE_LAYOUT(LRef obj, LRef new_layout)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.layout) = new_layout;
}

INLINE LRef STRUCTURE_ELEM(LRef obj, fixnum_t index)
{
     checked_assert(STRUCTUREP(obj));
     return ((obj)->storage_as.vector.data[(index)]);
}

INLINE void SET_STRUCTURE_ELEM(LRef obj, fixnum_t index, LRef new_value)
{
     checked_assert(STRUCTUREP(obj));
     ((obj)->storage_as.vector.data[(index)]) = new_value;
}

  /*** symbol **/
LRef symcons(_TCHAR * pname, LRef home);
LRef symcons(LRef pname, LRef home);

LRef simple_intern(LRef name, LRef package);
LRef simple_intern(const _TCHAR * name, LRef package);

LRef intern(LRef name, LRef package);
LRef keyword_intern(const _TCHAR * name);

INLINE LRef SYMBOL_PNAME(LRef sym)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).storage_as.symbol.props));

     LRef pname = NIL;

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

INLINE void SET_SYMBOL_PNAME(LRef sym, LRef pname)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(STRINGP(pname));
     checked_assert(NULLP((*sym).storage_as.symbol.props));

     ((*sym).storage_as.symbol.props) = pname;
}

INLINE LRef SYMBOL_PROPS(LRef sym)
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

LRef lcons(LRef x, LRef y);     /*  Forward decl */

INLINE void SET_SYMBOL_PROPS(LRef sym, LRef props)
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

INLINE size_t SYMBOL_INDEX(LRef sym)
{
     checked_assert(SYMBOLP(sym));
     return ((*sym).storage_as.symbol.env_index);
}


INLINE void SET_SYMBOL_INDEX(LRef sym, size_t index)
{
     checked_assert(SYMBOLP(sym));
     ((*sym).storage_as.symbol.env_index) = index;
}


INLINE LRef SYMBOL_VCELL(LRef sym)
{
     checked_assert(SYMBOLP(sym));

     if (SYMBOL_INDEX(sym) == 0)
          return UNBOUND_MARKER;

     checked_assert(interp.last_global_env_entry < GENV_DIM(interp.global_env));

     return GENV_ELEM(interp.global_env, SYMBOL_INDEX(sym));
}

INLINE void SET_SYMBOL_VCELL(LRef sym, LRef val)
{
     checked_assert(SYMBOL_INDEX(sym) != 0);
     checked_assert(interp.last_global_env_entry < GENV_DIM(interp.global_env));

     return SET_GENV_ELEM(interp.global_env, SYMBOL_INDEX(sym), val);
}

INLINE LRef SYMBOL_HOME(LRef x)
{
     checked_assert(SYMBOLP(x));
     return ((*x).storage_as.symbol.home);
}

INLINE void SET_SYMBOL_HOME(LRef x, LRef home)
{
     checked_assert(SYMBOLP(x));
     ((*x).storage_as.symbol.home) = home;
}


  /*** package **/
INLINE LRef PACKAGE_NAME(LRef x)
{
     checked_assert(PACKAGEP(x));
     return (((*x).storage_as.package.name));
}

INLINE void SET_PACKAGE_NAME(LRef x, LRef name)
{
     checked_assert(PACKAGEP(x));
     (((*x).storage_as.package.name)) = name;
}

INLINE LRef PACKAGE_BINDINGS(LRef x)
{
     checked_assert(PACKAGEP(x));
     return (((*x).storage_as.package.symbol_bindings));
}

INLINE void SET_PACKAGE_BINDINGS(LRef x, LRef symbol_bindings)
{
     checked_assert(PACKAGEP(x));
     (((*x).storage_as.package.symbol_bindings)) = symbol_bindings;
}

INLINE LRef PACKAGE_USE_LIST(LRef x)
{
     checked_assert(PACKAGEP(x));
     return (((*x).storage_as.package.use_list));
}

INLINE void SET_PACKAGE_USE_LIST(LRef x, LRef use_list)
{
     checked_assert(PACKAGEP(x));
     (((*x).storage_as.package.use_list)) = use_list;
}

  /*** subr **/
INLINE subr_arity_t SUBR_TYPE(LRef x)
{
     checked_assert(SUBRP(x));
     return (((*x).storage_as.subr.type));
}

INLINE void SET_SUBR_TYPE(LRef x, subr_arity_t type)
{
     checked_assert(SUBRP(x));
     (((*x).storage_as.subr.type)) = type;
}

INLINE LRef SUBR_NAME(LRef x)
{
     checked_assert(SUBRP(x));
     return (((*x).storage_as.subr.name));
}

INLINE void SET_SUBR_NAME(LRef x, LRef name)
{
     checked_assert(SUBRP(x));
     checked_assert(STRINGP(x));
     (((*x).storage_as.subr.name)) = name;
}

INLINE void SET_SUBR_CODE(LRef x, void *code)
{
     ((*x).storage_as.subr.code.ptr) = code;
}

INLINE void *SUBR_CODE(LRef x)
{
     return (void *)(*x).storage_as.subr.code.ptr;
}

INLINE f_0_t SUBR_F0(LRef x)
{
     return ((*x).storage_as.subr.code.f_0);
}

INLINE f_1_t SUBR_F1(LRef x)
{
     return ((*x).storage_as.subr.code.f_1);
}

INLINE f_2_t SUBR_F2(LRef x)
{
     return ((*x).storage_as.subr.code.f_2);
}

INLINE f_3_t SUBR_F3(LRef x)
{
     return ((*x).storage_as.subr.code.f_3);
}

INLINE f_4_t SUBR_F4(LRef x)
{
     return ((*x).storage_as.subr.code.f_4);
}

INLINE f_5_t SUBR_F5(LRef x)
{
     return ((*x).storage_as.subr.code.f_5);
}

INLINE f_6_t SUBR_F6(LRef x)
{
     return ((*x).storage_as.subr.code.f_6);
}

INLINE f_argc_t SUBR_FARGC(LRef x)
{
     return ((*x).storage_as.subr.code.f_argc);
}

  /*** closure **/
INLINE LRef CLOSURE_CODE(LRef x)
{
     checked_assert(CLOSUREP(x));
     return ((*x).storage_as.closure.code);
}

INLINE void SET_CLOSURE_CODE(LRef x, LRef code)
{
     checked_assert(CLOSUREP(x));
     ((*x).storage_as.closure.code) = code;
}

INLINE LRef CLOSURE_ENV(LRef x)
{
     checked_assert(CLOSUREP(x));
     return ((*x).storage_as.closure.env);
}

INLINE void SET_CLOSURE_ENV(LRef x, LRef env)
{
     checked_assert(CLOSUREP(x));
     ((*x).storage_as.closure.env) = env;
}

INLINE LRef CLOSURE_PROPERTY_LIST(LRef x)
{
     checked_assert(CLOSUREP(x));
     return ((*x).storage_as.closure.property_list);
}

INLINE void SET_CLOSURE_PROPERTY_LIST(LRef x, LRef plist)
{
     checked_assert(CLOSUREP(x));
     ((*x).storage_as.closure.property_list) = plist;
}

  /*** macro **/
LRef macrocons(LRef t);

INLINE LRef MACRO_TRANSFORMER(LRef x)
{
     checked_assert(MACROP(x));
     return (((*x).storage_as.macro.transformer));
}

INLINE void SET_MACRO_TRANSFORMER(LRef x, LRef transformer)
{
     checked_assert(MACROP(x));
     (((*x).storage_as.macro.transformer)) = transformer;
}

  /*** string **/
LRef strcons();
LRef strcons(_TCHAR ch);
LRef strcons(const _TCHAR * buffer);
LRef strcons(const _TCHAR * buffer, _TCHAR trailing);
LRef strcons(LRef str);
LRef strcons(size_t length, const _TCHAR * buffer);
LRef strcons_transfer_buffer(size_t length, _TCHAR * buffer);

_TCHAR *get_c_string(LRef x);
_TCHAR *get_c_string_dim(LRef x, size_t *);
_TCHAR *try_get_c_string(LRef x);


int str_next_character(LRef obj);
void str_append_str(LRef obj, _TCHAR * str, size_t len);

INLINE size_t STRING_DIM(LRef x)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._dim);
}

INLINE void SET_STRING_DIM(LRef x, size_t dim)
{
     checked_assert(STRINGP(x));
     ((*x).storage_as.string._dim) = dim;
}

INLINE size_t STRING_OFS(LRef x)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._ofs);
}

INLINE void SET_STRING_OFS(LRef x, size_t ofs)
{
     checked_assert(STRINGP(x));
     ((*x).storage_as.string._ofs) = ofs;
}

INLINE _TCHAR *STRING_DATA(LRef x)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._data);
}

INLINE _TCHAR *SET_STRING_DATA(LRef x, _TCHAR * data)
{
     checked_assert(STRINGP(x));
     return ((*x).storage_as.string._data) = data;
}


  /*** hash **/
fixnum_t sxhash_eq(LRef obj);
fixnum_t sxhash(LRef obj);
LRef lsxhash(LRef obj, LRef hash);

LRef hashcons(bool shallow, size_t size = HASH_DEFAULT_INITIAL_SIZE);

bool hash_ref(LRef table, LRef key, LRef & result);     /*  TODO: convert to pointer */

INLINE size_t HASH_MASK(LRef obj)
{
     checked_assert(HASHP(obj));
     return ((obj)->storage_as.hash._mask);
}

INLINE void SET_HASH_MASK(LRef obj, size_t mask)
{
     checked_assert(HASHP(obj));
     ((obj)->storage_as.hash._mask) = mask;
}

INLINE size_t HASH_SIZE(LRef obj)
{
     return HASH_MASK(obj) + 1;
}

INLINE hash_entry_t *HASH_DATA(LRef obj)
{
     checked_assert(HASHP(obj));
     return ((obj)->storage_as.hash._data);
}

INLINE hash_entry_t *SET_HASH_DATA(LRef obj, hash_entry_t * data)
{
     checked_assert(HASHP(obj));
     return ((obj)->storage_as.hash._data) = data;
}

typedef size_t hash_iter_t;
void hash_iter_begin(LRef hash, hash_iter_t * iter);
bool hash_iter_next(LRef hash, hash_iter_t * iter, LRef * key, LRef * val);

  /*** instance **/

LRef instancecons(LRef proto);


INLINE LRef INSTANCE_MAP(LRef obj)
{
     checked_assert(INSTANCEP(obj));
     return ((obj)->storage_as.instance._map);
}

INLINE void SET_INSTANCE_MAP(LRef obj, LRef map)
{
     checked_assert(INSTANCEP(obj));
     ((obj)->storage_as.instance._map) = map;
}

INLINE size_t INSTANCE_DIM(LRef obj)
{
     checked_assert(INSTANCEP(obj));
     return ((obj)->storage_as.instance._dim);
}

INLINE void SET_INSTANCE_DIM(LRef obj, size_t dim)
{
     checked_assert(INSTANCEP(obj));
     ((obj)->storage_as.instance._dim) = dim;
}

INLINE LRef *INSTANCE_DATA(LRef obj)
{
     checked_assert(INSTANCEP(obj));
     return ((obj)->storage_as.instance._data);
}

INLINE void SET_INSTANCE_DATA(LRef obj, LRef * data)
{
     checked_assert(INSTANCEP(obj));
     ((obj)->storage_as.instance._data) = data;
}

INLINE LRef INSTANCE_ELEM(LRef obj, size_t index)
{
     checked_assert(INSTANCEP(obj));
     assert(index < INSTANCE_DIM(obj));
     return ((obj)->storage_as.instance._data)[index];
}

INLINE void SET_INSTANCE_ELEM(LRef obj, size_t index, LRef new_value)
{
     checked_assert(INSTANCEP(obj));
     assert(index < INSTANCE_DIM(obj));
     ((obj)->storage_as.instance._data)[index] = new_value;
}

INLINE LRef INSTANCE_PROTO(LRef obj)
{
     return INSTANCE_ELEM(obj, 0);
}

INLINE void SET_INSTANCE_PROTO(LRef obj, LRef proto)
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
     LRef _port_name;

     void *_user_data;
     LRef _user_object;
     LRef _fasl_table;

     LRef _fasl_stack[FAST_LOAD_STACK_DEPTH];
     size_t _fasl_stack_ptr;
     LRef _fasl_accum;

     port_mode_t _mode;

     port_text_translation_info_t *_text_info;

     size_t _bytes_read;
     size_t _bytes_written;
};

struct port_class_t
{
     const _TCHAR *_name;
     port_mode_t _valid_modes;

     void (*_open) (LRef);

      bool(*_read_readyp) (LRef);
      size_t(*_read) (void *, size_t, size_t, LRef);

      size_t(*_write) (const void *, size_t, size_t, LRef);
      bool(*_rich_write) (LRef, bool, LRef);

     int (*_flush) (LRef);
     void (*_close) (LRef);
     void (*_gc_free) (LRef);

      size_t(*_length) (LRef);
};

INLINE port_info_t *PORT_PINFO(LRef x)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._pinf));
}

INLINE port_info_t *SET_PORT_PINFO(LRef x, port_info_t * pinf)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._pinf)) = pinf;
}

INLINE port_class_t *PORT_CLASS(LRef x)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._class));
}

INLINE port_class_t *SET_PORT_CLASS(LRef x, port_class_t * klass)
{
     checked_assert(PORTP(x));
     return (((*x).storage_as.port._class)) = klass;
}

INLINE port_text_translation_info_t *PORT_TEXT_INFO(LRef x)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->_text_info);
}

INLINE port_text_translation_info_t *SET_PORT_TEXT_INFO(LRef x,
                                                        port_text_translation_info_t * text_info)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->_text_info) = text_info;
}

INLINE port_mode_t PORT_MODE(LRef x)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->_mode);
}

INLINE void SET_PORT_MODE(LRef x, port_mode_t mode)
{
     checked_assert(PORTP(x));
     (PORT_PINFO(x)->_mode) = mode;
}

INLINE bool PORT_BINARYP(LRef x)
{
     return (PORT_TEXT_INFO(x) == NULL);
}

  /*** values-tuple ***/
INLINE LRef VALUES_TUPLE_VALUES(LRef vt)
{
     checked_assert(VALUES_TUPLE_P(vt));
     return ((*vt).storage_as.values_tuple._values);
}

INLINE void SET_VALUES_TUPLE_VALUES(LRef vt, LRef vals)
{
     checked_assert(VALUES_TUPLE_P(vt));
     ((*vt).storage_as.values_tuple._values) = vals;
}

LRef lvalues(LRef values);
LRef valuesn(long n, ...);
LRef lvalues2list(LRef obj);

  /*** fast op ***/
INLINE int FAST_OP_OPCODE(LRef fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).header.opcode);
}

INLINE void SET_FAST_OP_OPCODE(LRef fo, int opcode)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).header.opcode) = opcode;
}

INLINE LRef FAST_OP_ARG1(LRef fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).storage_as.fast_op.arg1);
}

INLINE void SET_FAST_OP_ARG1(LRef fo, LRef arg1)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).storage_as.fast_op.arg1) = arg1;
}

INLINE LRef FAST_OP_ARG2(LRef fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).storage_as.fast_op.arg2);
}

INLINE void SET_FAST_OP_ARG2(LRef fo, LRef arg2)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).storage_as.fast_op.arg2) = arg2;
}

INLINE LRef FAST_OP_ARG3(LRef fo)
{
     checked_assert(FAST_OP_P(fo));
     return ((*fo).storage_as.fast_op.arg3);
}

INLINE void SET_FAST_OP_ARG3(LRef fo, LRef arg3)
{
     checked_assert(FAST_OP_P(fo));
     ((*fo).storage_as.fast_op.arg3) = arg3;
}

LRef fast_op(int opcode, LRef arg1, LRef arg2, LRef arg3);
LRef lfast_op(LRef opcode, LRef arg1, LRef arg2, LRef arg3);
LRef lfast_op_opcode(LRef fastop);
LRef lfast_op_args(LRef fastop);

/**** Input/Output ****/
const LRef DEFAULT_PORT = NIL;

INLINE LRef CURRENT_INPUT_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_INPUT_PORT];
}

INLINE LRef CURRENT_OUTPUT_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_OUTPUT_PORT];
}

INLINE LRef CURRENT_ERROR_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_ERROR_PORT];
}

INLINE LRef CURRENT_DEBUG_PORT()
{
     return interp.control_fields[VMCTRL_CURRENT_DEBUG_PORT];
}

/*  This is the 'universally availble' debugger output port. */
INLINE LRef VM_DEBUG_PORT()
{
     return (&interp.debugger_output);
}

LRef portcons(port_class_t * cls, LRef port_name, port_mode_t mode, LRef user_object,
              void *user_data);

size_t read_raw(void *buf, size_t size, size_t count, LRef port);
size_t write_raw(const void *buf, size_t size, size_t count, LRef port);

int read_char(LRef port);
int unread_char(int ch, LRef port);
int peek_char(LRef port);
void write_char(int ch, LRef port);
size_t write_text(const _TCHAR * buf, size_t count, LRef port);

#define WRITE_TEXT_CONSTANT(buf, port) write_text(buf, (sizeof(buf) / sizeof(_TCHAR)) - 1, port)


LRef scvwritef(const _TCHAR * format_str, LRef port, va_list arglist);
void scwritef(const _TCHAR * format_str, LRef port, ...);
void dscwritef(const _TCHAR * format_str, ...);
void dscwritef(debug_flag_t flag, const _TCHAR * format_str, ...);

LRef debug_print_object(LRef exp, LRef port, bool machine_readable);

void register_internal_file(const _TCHAR * filename, bool binary_data, data_block_t *data);
LRef open_c_data_input(bool binary_data, data_block_t *data);

typedef bool(*blocking_input_read_data_fn_t) (LRef port, void *userdata);
typedef void (*blocking_input_close_port_fn_t) (LRef port, void *userdata);

void blocking_input_post_data(LRef port, void *data, size_t size);
void blocking_input_post_eof(LRef port);
bool blocking_input_is_data_available(LRef port);

LRef blocking_input_cons(const _TCHAR * port_name, bool binary,
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
LRef find_subr_by_name(LRef subr_name);
LRef run();

  /****** Evaluator and Loader */

LRef apply1(LRef fn, size_t argc, LRef argv[]);

bool call_lisp_procedurev(LRef closure, LRef * out_retval, LRef * out_escape_tag, LRef leading_args,
                          size_t n, va_list args);
bool call_lisp_procedure(LRef closure, LRef * out_retval, LRef * out_escape_tag, size_t n, ...);

LRef lidefine_global(LRef var, LRef val, LRef genv);

  /****** Error handling and control */


bool infop();                   /*  REVISIT: still used? */
void info(const _TCHAR * message, ...);

enum vmt_options_t {
     /* Use one of these... */
     VMT_MANDATORY_TRAP      = 0x0,
     VMT_OPTIONAL_TRAP       = 0x1,

     /* ...and optionally this. */
     VMT_HANDLER_MUST_ESCAPE = 0x2
};

LRef vmtrap(trap_type_t trap, vmt_options_t options, size_t argc, ...);

void vmerror_wrong_type(LRef new_errobj);
void vmerror_wrong_type(int which_argument, LRef new_errobj);
void vmerror_unbound(LRef v);
void vmerror_index_out_of_bounds(LRef index, LRef obj);
void vmerror_arg_out_of_range(LRef arg, const _TCHAR *range_desc = NULL);
void vmerror_unsupported(const _TCHAR *desc);
void vmerror_unimplemented(const _TCHAR *desc);
void vmerror_divide_by_zero();
void vmerror_io_error(const _TCHAR *desc, LRef info);
void fast_read_error(const _TCHAR * message, LRef port, LRef details = NIL);

void vmerror_stack_overflow(u8_t * obj);

  /****** Memory management */

void gc_protect(const _TCHAR * name, LRef * location, size_t n);
LRef gc_protect_sym(LRef * location, const _TCHAR * st, LRef package);

void gc_register_thread(interpreter_thread_info_block_t * thr);

void gc_release_freelist(LRef new_freelist);
LRef gc_claim_freelist();

void *safe_malloc(size_t size);

void safe_free(void *block);

void set_global_env(LRef genv);
void check_global_environment_size();

/***** Time *****/
flonum_t time_since_launch();

/***** Prototypes for C Primitives *****/

LRef get_current_frames(fixnum_t skip_count);
LRef lacos(LRef x);
LRef ladd(LRef x, LRef y);
LRef ladd_symbol_to_package(LRef symbol, LRef package);
LRef langle(LRef cmplx);
LRef lapply(size_t argc, LRef argv[]);
LRef lasin(LRef x);
LRef latan(LRef x, LRef y);
LRef lbinary_portp(LRef obj);
LRef lbinary_write_flonum(LRef v, LRef port);
LRef lbitwise_and(LRef x, LRef y);
LRef lbitwise_ashr(LRef x, LRef n);
LRef lbitwise_not(LRef x);
LRef lbitwise_or(LRef x, LRef y);
LRef lbitwise_shl(LRef x, LRef n);
LRef lbitwise_shr(LRef x, LRef n);
LRef lbitwise_xor(LRef x, LRef y);
LRef lbooleanp(LRef x);
LRef lcar(LRef x);
LRef lcatch_apply0(LRef tag, LRef fn);
LRef lcdr(LRef x);
LRef lceiling(LRef x);
LRef lchar2integer(LRef s);
LRef lchar_readyp(LRef port);
LRef lcharacter2string(LRef obj);
LRef lcharp(LRef x);
LRef lclone_c_data_port(LRef port);
LRef lclone_instance(LRef inst);
LRef lclose_port(LRef port);
LRef lclosure_code(LRef exp);
LRef lclosure_env(LRef exp);
LRef lclosurecons(LRef env, LRef code, LRef property_list);
LRef lclosurep(LRef obj);
LRef lcomplexp(LRef x);
LRef lconsp(LRef x);
LRef lcopy_global_environment(LRef genv, LRef name);
LRef lcopy_structure(LRef st);
LRef lcos(LRef x);
LRef lcurrent_global_environment();
LRef ldebug_flags();
LRef ldebug_write(LRef form);
LRef ldelete_file(LRef filename);
LRef ldisplay_to_string(LRef exp);
LRef ldivide(LRef x, LRef y);
LRef ldo_external_symbols(LRef args, LRef env);
LRef ldo_symbols(LRef args, LRef env);
LRef ldump_heap_state(LRef port);
LRef lenlarge_heap(LRef count);
LRef lenvironment();
LRef lenvlookup(LRef var, LRef env);
LRef leof_objectp(LRef obj);
LRef leq(LRef x, LRef y);
LRef leql(LRef x, LRef y);
LRef lequal(LRef, LRef);
LRef lexact2inexact(LRef x);
LRef lexactp(LRef x);
LRef lexp(LRef x);
LRef lexpt(LRef x, LRef y);
LRef lfast_read(LRef port);
LRef lfloor(LRef x);
LRef lflush_port(LRef port);
LRef lflush_whitespace(LRef port, LRef slc);
LRef lfresh_line(LRef port);
LRef lgc();
LRef lgc_info();
LRef lgc_runtime();
LRef lgc_status(LRef new_gc_status);
LRef lget_current_frames(LRef skip_count);
LRef lget_output_string(LRef port);
LRef lglobal_environment_name(LRef genv);
LRef lglobal_environment_ref(LRef genv, LRef i);
LRef lglobal_environment_set(LRef genv, LRef i, LRef v);
LRef lglobal_environmentp(LRef genv);
LRef lhandler_frames();
LRef lhas_slotp(LRef this_obj, LRef key);
LRef lhash2alist(LRef hash);
LRef lhash2list(LRef hash);
LRef lhash_clear(LRef hash);
LRef lhash_copy(LRef hash);
LRef lhash_hasp(LRef table, LRef key);
LRef lhash_key(LRef obj);
LRef lhash_ref(size_t argc, LRef argv[]);
LRef lhash_refs(LRef table, LRef key);
LRef lhash_remove(LRef table, LRef key);
LRef lhash_set(LRef table, LRef key, LRef value);
LRef lhash_type(LRef hash);
LRef lhashp(LRef obj);
LRef lheap_cell_count_by_typecode();
LRef liarm_gc_trip_wires(LRef f);
LRef licontrol_field(LRef control_field_id);
LRef lidebug_printer(LRef obj, LRef port, LRef machine_readable_p);
LRef lidirectory(LRef dirname, LRef mode);
LRef lieee754_bits_to(LRef x);
LRef lifile_details(LRef path, LRef existance_onlyp);
LRef ligc_trip_wire();
LRef lihash_binding_vector(LRef hash);
LRef liifasl_load(LRef port);
LRef liimmediate_p(LRef obj);
LRef liinstance_map(LRef inst);
LRef liinstance_proto(LRef instance);
LRef liinstance_slots(LRef instance);
LRef liinternal_files();
LRef liload(LRef fname);
LRef limacrocons(LRef t);
LRef limag_part(size_t argc, LRef argv[]);
LRef linexact2display_string(LRef n, LRef sf, LRef sci, LRef s);
LRef linexact2exact(LRef x);
LRef linexactp(LRef x);
LRef linfinitep(LRef x);
LRef linput_portp(LRef obj);
LRef linstancep(LRef obj);
LRef linteger2char(LRef s);     /*  REVISIT: rename to exact->char */
LRef lintegerp(LRef x);
LRef lipackagecons(LRef name);
LRef liset_control_field(LRef control_field_id, LRef new_value);
LRef liset_instance_proto(LRef instance, LRef new_proto);
LRef liset_trap_handler(LRef trap_id, LRef new_handler);
LRef lislot_ref(LRef obj, LRef key);
LRef lislot_set(LRef obj, LRef key, LRef value);
LRef lisp_strcmp(LRef s1, LRef s2);
LRef listartup_args();
LRef lisubr_table();
LRef lisymbol_index(LRef symbol);
LRef litrap_handler(LRef trap_id);
LRef litypecode(LRef obj);
LRef lkeywordp(LRef x);
LRef llast(LRef);
LRef llength(LRef obj);
LRef llisp_heap_stress_thread(LRef t, LRef c, LRef s);
LRef llist(LRef l);
LRef llist2hash(LRef obj);
LRef llist2vector(LRef l);
LRef llog(LRef x);
LRef lmacro_transformer(LRef mac);
LRef lmacrop(LRef obj);
LRef lmagnitude(LRef cmplx);
LRef lmake_eof();
LRef lmake_hash(LRef key_type);
LRef lmake_instance(LRef args);
LRef lmake_polar(LRef r, LRef theta);
LRef lmake_rectangular(LRef re, LRef im);
LRef lmake_vector(LRef dim, LRef initial);
LRef lmemref_byte(LRef addr);
LRef lmodulo(LRef x, LRef y);
LRef lmultiply(LRef x, LRef y);
LRef lnanp(LRef x);
LRef lnewline(LRef);
LRef lnotp(LRef x);
LRef lnullp(LRef x);
LRef lnum_eq(size_t argc, LRef argv[]);
LRef lnum_ge(size_t argc, LRef argv[]);
LRef lnum_gt(size_t argc, LRef argv[]);
LRef lnum_le(size_t argc, LRef argv[]);
LRef lnum_lt(size_t argc, LRef argv[]);
LRef lnumber2string(LRef x, LRef r, LRef s, LRef p);
LRef lnumberp(LRef x);
LRef lobaddr(LRef object);
LRef lopen_debug_port();
LRef lopen_des_input(LRef source, LRef key, LRef encoding, LRef mode);
LRef lopen_des_output(LRef dest, LRef key, LRef encoding, LRef mode);
LRef lopen_input_file(LRef filename, LRef mode);
LRef lopen_input_string(LRef string);
LRef lopen_null_port();
LRef lopen_output_file(LRef filename, LRef mode);
LRef lopen_output_string();
LRef loutput_portp(LRef obj);
LRef lpackagcons(LRef name);
LRef lpackage_bindings(LRef p);
LRef lpackage_name(LRef p);
LRef lpackage_use_list(LRef p);
LRef lpackagep(LRef x);
LRef lpanic(LRef msg);
LRef lpeek_char(LRef port);
LRef lport_io_counts(LRef port);
LRef lport_location(LRef port);
LRef lport_mode(LRef obj);
LRef lport_name(LRef port);
LRef lport_set_translate_mode(LRef port, LRef mode);
LRef lport_translate_mode(LRef port);
LRef lprimitivep(LRef obj);
LRef lprocedurep(LRef exp);
LRef lproperty_list(LRef exp);
LRef lqsort(LRef l, LRef f, LRef g);
LRef lquotient(LRef x, LRef y);
LRef lrandom(LRef n);
LRef lrationalp(LRef x);
LRef lread_binary_fixnum(LRef l, LRef sp, LRef port);
LRef lread_binary_flonum(LRef port);
LRef lread_binary_string(LRef l, LRef port);
LRef lread_char(LRef port);
LRef lread_line(LRef port);
LRef lread_port_to_string(LRef port);
LRef lreal_part(LRef cmplx);
LRef lrealp(LRef x);
LRef lrealtime(void);
LRef lrealtime_time_zone_offset();
LRef lremainder(LRef x, LRef y);
LRef lrepresentation_of(LRef obj);
LRef lrich_write(LRef obj, LRef machine_readable, LRef port);
LRef lround(LRef x);
LRef lruntime(void);
LRef lsend(LRef args);
LRef lset_closure_code(LRef exp, LRef code);
LRef lset_closure_env(LRef exp, LRef env);
LRef lset_debug_flags(LRef c);
LRef lset_environment_variable(LRef varname, LRef value);
LRef lset_fasl_package_list(LRef packages);
LRef lset_handler_frames(LRef new_frames);
LRef lset_interrupt_mask(LRef new_mask);
LRef lset_package_name(LRef p, LRef new_name);
LRef lset_package_use_list(LRef p, LRef use_list);
LRef lset_property_list(LRef exp, LRef property_list);
LRef lset_random_seed(LRef s);
LRef lset_stack_limit(LRef);
LRef lset_symbol_package(LRef sym, LRef package);
LRef lsetcar(LRef cell, LRef value);
LRef lsetcdr(LRef cell, LRef value);
LRef lsin(LRef x);
LRef lsleep(LRef ms);
LRef lsqrt(LRef x);
LRef lstress_c_heap(LRef c, LRef s);
LRef lstress_lisp_heap(LRef c);
LRef lstring2number(LRef, LRef);
LRef lstring2uninterned_symbol(LRef str);
LRef lstring_append(size_t argc, LRef argv[]);
LRef lstring_copy(LRef string);
LRef lstring_downcase(LRef);
LRef lstring_downcased(LRef);
LRef lstring_first_char(LRef string, LRef char_set, LRef initial_ofs);
LRef lstring_first_substring(LRef string, LRef char_set, LRef initial_ofs);
LRef lstring_length(LRef string);
LRef lstring_ref(LRef a, LRef i);
LRef lstring_search(LRef token, LRef str, LRef maybe_from);
LRef lstring_search_from_right(LRef tok, LRef str, LRef maybe_from);
LRef lstring_set(LRef a, LRef i, LRef v);
LRef lstring_trim(LRef, LRef);
LRef lstring_trim_left(LRef, LRef);
LRef lstring_trim_right(LRef, LRef);
LRef lstring_upcase(LRef);
LRef lstring_upcased(LRef);
LRef lstringp(LRef x);
LRef lstructure_layout(LRef st);
LRef lstructure_length(LRef st);
LRef lstructure_ref(LRef st, LRef index);
LRef lstructure_set(LRef st, LRef index, LRef value);
LRef lstructurecons(LRef slots, LRef layout);
LRef lstructurep(LRef st, LRef expected_layout);
LRef lsubr_name(LRef subr);
LRef lsubr_type_code(LRef subr);
LRef lsubset(LRef fcn, LRef l);
LRef lsubstring(LRef, LRef, LRef);
LRef lsubtract(LRef x, LRef y);
LRef lsymbol_name(LRef sym);
LRef lsymbol_name(LRef sym);
LRef lsymbol_package(LRef sym);
LRef lsymbolp(LRef x);
LRef lsysob(LRef addr);
LRef lsystem(size_t argc, LRef argv[]);
LRef lsystem_info();
LRef ltan(LRef x);
LRef ltemporary_file_name(LRef prefix);
LRef ltest_blocking_input(LRef block_size, LRef length, LRef binary);
LRef lthrow(LRef tag, LRef value);
LRef ltime_apply0(LRef fn);
LRef lto_ieee754_bits(LRef x);
LRef ltruncate(LRef x);
LRef lunbound_marker();
LRef lunread_char(LRef ch, LRef port);
LRef lunwind_protect(LRef thunk, LRef after);
LRef lvector(size_t argc, LRef argv[]);
LRef lvector2list(LRef vec);
LRef lvector_copy(LRef vec);
LRef lvector_fill(LRef vec, LRef v);
LRef lvector_ref(LRef a, LRef i, LRef d);
LRef lvector_resize(LRef vec, LRef new_size, LRef new_element);
LRef lvector_set(LRef a, LRef i, LRef v);
LRef lvectorp(LRef obj);
LRef lwrite_binary_fixnum(LRef v, LRef l, LRef sp, LRef port);
LRef lwrite_binary_string(LRef string, LRef port);
LRef lwrite_char(LRef ch, LRef port);
LRef lwrite_strings(size_t argc, LRef argv[]);
LRef lwrite_to_string(LRef exp);
bool equalp(LRef, LRef);
bool read_binary_fixnum(fixnum_t length, bool signedp, LRef port, fixnum_t & result);
bool read_binary_flonum(LRef port, flonum_t & result);
double round(double n);
void scan_postmortem_dump();

/***** Debugging tools *****/

INLINE bool DEBUG_FLAG(debug_flag_t flag)
{
     REFERENCED_BY_DEBUG_BUILD(flag);

     return DEBUGGING_BUILD && (interp.debug_flags & (fixnum_t) flag);
}


/* Frames and exceptions
 *
 * Frames are basically annotations on the dynamic stack. Each
 * frame has an "frame record" stored in an auto variable
 * local to a newly created scope. When the frame is entered,
 * the frame's frame record is registered on a global stack.
 * When the frame is left, the frame record is popped off of
 * the stack.
 */

#define ENTER_FRAME()                                               \
{                                                                   \
   frame_record_t  __frame;                                         \
                                                                    \
   __frame.prev = CURRENT_TIB()->frame_stack;                       \
                                                                    \
   CURRENT_TIB()->frame_stack = &__frame;

#define ENTER_MARKER_FRAME(__tag)                                   \
  ENTER_FRAME()                                                     \
     __frame.type = FRAME_MARKER;                                   \
     __frame.as.marker.tag = __tag;

#define ENTER_PRIMITIVE_FRAME(__f)                                  \
    ENTER_FRAME()                                                   \
       __frame.type              = FRAME_PRIMITIVE;                 \
       __frame.as.prim.function  = __f;

#define ENTER_DYNAMIC_ESCAPE_FRAME(__tag, __type)                         \
      ENTER_FRAME()                                                       \
         assert((__type == FRAME_EX_TRY) || (__type == FRAME_EX_UNWIND)); \
                                                                          \
          __frame.type                 = __type;            \
          __frame.as.escape.pending    = FALSE;             \
          __frame.as.escape.unwinding  = FALSE;             \
          __frame.as.escape.tag        = __tag;             \
          __frame.as.escape.retval     = NIL;

#define ENTER_EVAL_FRAME(__form, __env)                       \
      ENTER_FRAME()                                           \
         __frame.type                          = FRAME_EVAL;  \
                                                              \
         __frame.as.eval.form                  = __form;      \
         __frame.as.eval.initial_form          = *__form;     \
         __frame.as.eval.env                   = __env;

/* IF YOU DO AN EXPLICIT RETURN WITHIN A FRAME, THIS WILL CORRUPT THE FRAME RECORD STACK. */
#define LEAVE_FRAME()                                               \
  checked_assert(CURRENT_TIB()->frame_stack == &__frame);           \
  CURRENT_TIB()->frame_stack = __frame.prev;                        \
}

typedef bool(*frame_predicate) (frame_record_t * frame, uptr_t info);

frame_record_t *__frame_find(frame_predicate pred, uptr_t info);

/* C++-style exception handling */

#define ENTER_TRY(tag) ENTER_TRY_1(tag, FRAME_EX_TRY)

#define ENTER_UNWIND_PROTECT() ENTER_TRY_1(NULL, FRAME_EX_UNWIND)

#define ENTER_TRY_1(tag, guard)                                                      \
   ENTER_DYNAMIC_ESCAPE_FRAME(tag, guard)                                            \
   {                                                                                 \
      bool __block_successful =                                                      \
        (setjmp(CURRENT_TIB()->frame_stack->as.escape.cframe) == 0);   \
                                                                                     \
      if (__block_successful)                                                        \
      {

#define ON_ERROR()                                                \
      }                                                           \
      else                                                        \
      {

#define LEAVE_TRY()                                               \
      }                                                           \
   }                                                              \
   LEAVE_FRAME();


/* C-style Unwind Protect */

#define ON_UNWIND()                                               \
      }                                                           \
      CURRENT_TIB()->frame_stack->as.escape.unwinding = TRUE;

#define LEAVE_UNWIND_PROTECT()                                    \
      if (!__block_successful)                                    \
            lthrow(CURRENT_TIB()->frame_stack->as.escape.tag,     \
                   CURRENT_TIB()->frame_stack->as.escape.retval); \
                                                                  \
   }                                                              \
   LEAVE_FRAME();



bool parse_string_as_fixnum(_TCHAR * string, int radix, fixnum_t & result);

/* Structure base metaclass operations */

bool init_slots(LRef obj, LRef initargs, bool names_must_be_symbols);

void port_gc_free(LRef port);
LRef port_gc_mark(LRef obj);
bool string_equal(LRef a, LRef b);
bool hash_equal(LRef a, LRef b);
bool instance_equal(LRef a, LRef b);
bool vector_equal(LRef a, LRef b);
bool structure_equal(LRef sta, LRef stb);
bool fast_op_equal(LRef a, LRef b);

void collect_garbage();
void gc_mark(LRef obj);

void create_initial_packages();
void create_gc_heap();
void free_gc_heap();
void init_debugger_output();
void init_stdio_ports();

extern port_class_t stderr_port_class;

LRef initialize_port(LRef s,
                     port_class_t * cls,
                     LRef port_name, port_mode_t mode, LRef user_object, void *user_data);


size_t object_length(LRef obj);
size_t hash_length(LRef hash);
size_t port_length(LRef port);

#define CHARNAMECOUNT (33)
#define CHAREXTENDED (0x80)

debug_flag_t debug_flags_from_string(debug_flag_t initial, const _TCHAR * source_name,
                                     const _TCHAR * str);
debug_flag_t debug_flags_from_environment(debug_flag_t initial);

/****************************************************************
 * INLINE function definitions (REVISIT: not strictly true.)
 */

/* new_cell
 *
 * Allocate cells from the heap.
 */

INLINE interpreter_thread_info_block_t *CURRENT_TIB()
{
     return &interp.thread;
}


INLINE LRef new_cell(typecode_t type)
{
     checked_assert(!interp.shutting_down);

     interpreter_thread_info_block_t *thread = CURRENT_TIB();

     if (NULLP(thread->freelist))
          thread->freelist = gc_claim_freelist();

     assert(!NULLP(thread->freelist));  /*  Fired on out-of-memory... What then? */

     LRef retval = thread->freelist;
     thread->freelist = NEXT_FREE_CELL(thread->freelist);

     ++interp.gc_total_cells_allocated;

     SET_TYPE(retval, type);

     return retval;
}

END_NAMESPACE;

#endif
