
/*
 * scan-types.h --
 *
 * The core scan data types and their accessors.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __SCAN_TYPES_H
#define __SCAN_TYPES_H

#include "scan-base.h"
#include "scan-constants.h"
#include "scan-sys.h"

/*** Constants for the two level tagging scheme ***/

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

/*** Fixnum and Flonum ***/

typedef intptr_t fixnum_t;
typedef uintptr_t unsigned_fixnum_t;

#define FIXNUM_BITS          (sizeof(fixnum_t) * 8 - LREF1_TAG_SHIFT)
#define FIXNUM_MAX           (INTPTR_MAX >> LREF1_TAG_SHIFT)
#define FIXNUM_MIN           (INTPTR_MIN >> LREF1_TAG_SHIFT)
#define FIXNUM_UNSIGNED_MAX  (UINTPTR_MAX >> LREF1_TAG_SHIFT)
#define FIXNUM_UNSIGNED_MIN  (UINTPTR_MIN >> LREF1_TAG_SHIFT)

typedef double flonum_t;

#define FLONUM_MAX DBL_MAX
#define FLONUM_MIN (-DBL_MAX)
#define FLONUM_EPSILON DBL_EPSILON

/*** Forward declarations ***/

struct port_class_t;
struct port_info_t;
struct port_text_info_t;
struct lobject_t;
typedef struct lobject_t *lref_t;
struct fasl_stream_t;

#define UNBOUND_MARKER ((lref_t)LREF2_UNBOUND)

INLINE lref_t MAKE_LREF1(enum lref_tag_t tag, intptr_t val)
{
     return (lref_t) ((val << LREF1_TAG_SHIFT) | tag);
}

INLINE lref_t MAKE_LREF2(enum lref_tag_t tag, intptr_t val)
{
     return (lref_t) ((val << LREF2_TAG_SHIFT) | tag);
}

INLINE enum lref_tag_t LREF1_TAG(lref_t ref)
{
     return (enum lref_tag_t) ((intptr_t) ref & LREF1_TAG_MASK);
}

INLINE enum lref_tag_t LREF2_TAG(lref_t ref)
{
     return (enum lref_tag_t) ((intptr_t) ref & LREF2_TAG_MASK);
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

/*** Procedure data types ***/

typedef lref_t(*f_0_t) (void);
typedef lref_t(*f_1_t) (lref_t);
typedef lref_t(*f_2_t) (lref_t, lref_t);
typedef lref_t(*f_3_t) (lref_t, lref_t, lref_t);
typedef lref_t(*f_4_t) (lref_t, lref_t, lref_t, lref_t);
typedef lref_t(*f_argc_t) (size_t, lref_t[]);

struct hash_entry_t
{
     lref_t key; /*  == UNBOUND_MARKER for empty. */
     lref_t val;
};

struct hash_table_t
{
     size_t mask;
     bool is_shallow;
     size_t count;

     struct hash_entry_t data[0];
};

/*** The core boxed data type ***/

#pragma pack(push, 4)
struct lobject_t
{
     union
     {
          struct
          {
               enum typecode_t type:8;
               unsigned int opcode:8;
               unsigned int gc_mark:1;
          } header;

          /* Headers must be at least one pointer in size. */
          uintptr_t header_min_size;
     };

     union
     {
          struct
          {
               lref_t car;
               lref_t cdr;
          } cons;
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
               lref_t bindings;
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
               size_t dim;
               _TCHAR *data;
          } string;
          struct
          {
               size_t dim;
               lref_t *data;
               lref_t layout;
          } vector;
          struct
          {
               lref_t name;
               lref_t slots;
          } slayout;
          struct
          {
               struct port_class_t *klass;
               struct port_info_t *pinf;
               struct port_text_info_t *text_info;
          } port;
          struct
          {
               lref_t values;
          } values_tuple;
          struct
          {
               lref_t next;

               lref_t arg1;
               lref_t arg2;
          } fast_op;
          struct
          {
               struct hash_table_t *table;
          } hash;
          struct
          {
               lref_t name;
               enum subr_arity_t type;
               union
               {
                    f_0_t f_0;
                    f_1_t f_1;
                    f_2_t f_2;
                    f_3_t f_3;
                    f_4_t f_4;
                    f_argc_t f_argc;

                    void *ptr;
               } code;
          } subr;
          struct
          {
               lref_t port;
               struct fasl_stream_t *stream;
          } fasl_reader;

     } as;
};
#pragma pack(pop)

/*** NIL and primitive equality checks ***/

#define NIL ((lref_t)0)

INLINE bool EQ(lref_t x, lref_t y)
{
     return x == y;
};

INLINE bool NULLP(lref_t x)
{
     return EQ(x, NIL);
};

/*** Accessors for box header fields ***/

INLINE void SET_GC_MARK(lref_t object, int new_gc_mark_bit)
{
     checked_assert(!LREF_IMMEDIATE_P(object));

     object->header.gc_mark = new_gc_mark_bit;
}

INLINE int GC_MARK(lref_t object)
{
     return object->header.gc_mark;
}

INLINE bool REFTYPEP(lref_t object, enum typecode_t type)
{
     return (LREF1_TAG(object) == LREF1_REF)
          && (object != NULL)
          && (object->header.type == type);
}

INLINE enum typecode_t TYPE(lref_t object)
{
     if (NULLP(object))
          return TC_NIL;
     else if (LREF1_TAG(object) == LREF1_REF)
          return NULLP(object) ? TC_NIL : object->header.type;
     else if (LREF1_TAG(object) == LREF1_FIXNUM)
          return TC_FIXNUM;
     else
     {
          if (LREF2_TAG(object) == LREF2_BOOL)
               return TC_BOOLEAN;
          else if (LREF2_TAG(object) == LREF2_CHARACTER)
               return TC_CHARACTER;
          else if (LREF2_TAG(object) == LREF2_UNBOUND)
               return TC_UNBOUND_MARKER;
          else
               return TC_END_OF_FILE;
     }
}


/*** Type predicates ***/

/* full INLINE causes problems with gcc 3.4.4, due to prototype. */
inline  lref_t FLOIM(lref_t x);

INLINE bool FIXNUMP(lref_t x)            { return LREF1_TAG(x) == LREF1_FIXNUM;                                         }
INLINE bool CHARP(lref_t x)              { return (LREF1_TAG(x) == LREF1_SPECIAL) && (LREF2_TAG(x) == LREF2_CHARACTER); }
INLINE bool BOOLP(lref_t x)              { return (LREF1_TAG(x) == LREF1_SPECIAL) && (LREF2_TAG(x) == LREF2_BOOL);      }
INLINE bool EOFP(lref_t x)               { return (LREF1_TAG(x) == LREF1_SPECIAL) && (LREF2_TAG(x) == LREF2_EOF);       }
INLINE bool UNBOUND_MARKER_P(lref_t x)   { return EQ(x, UNBOUND_MARKER);                                                }
INLINE bool FREE_CELL_P(lref_t x)        { return REFTYPEP(x, TC_FREE_CELL);                                            }
INLINE bool CONSP(lref_t x)              { return REFTYPEP(x, TC_CONS);                                                 }
INLINE bool SYMBOLP(lref_t x)            { return REFTYPEP(x, TC_SYMBOL);                                               }
INLINE bool FLONUMP(lref_t x)            { return REFTYPEP(x, TC_FLONUM);                                               }
INLINE bool REALP(lref_t x)              { return (FIXNUMP(x) || (FLONUMP(x) && NULLP(FLOIM(x))));                      }
INLINE bool COMPLEXP(lref_t x)           { return (FLONUMP(x) && !NULLP(FLOIM(x)));                                     }
INLINE bool NUMBERP(lref_t x)            { return (FIXNUMP(x) || FLONUMP(x));                                           }
INLINE bool STRINGP(lref_t x)            { return REFTYPEP(x, TC_STRING);                                               }
INLINE bool PACKAGEP(lref_t x)           { return REFTYPEP(x, TC_PACKAGE);                                              }
INLINE bool PORTP(lref_t x)              { return REFTYPEP(x, TC_PORT);                                                 }
INLINE bool VECTORP(lref_t x)            { return REFTYPEP(x, TC_VECTOR);                                               }
INLINE bool STRUCTUREP(lref_t x)         { return REFTYPEP(x, TC_STRUCTURE);                                            }
INLINE bool SLAYOUTP(lref_t x)           { return REFTYPEP(x, TC_SLAYOUT);                                              }
INLINE bool HASHP(lref_t x)              { return REFTYPEP(x, TC_HASH);                                                 }
INLINE bool CLOSUREP(lref_t x)           { return REFTYPEP(x, TC_CLOSURE);                                              }
INLINE bool SUBRP(lref_t x)              { return REFTYPEP(x, TC_SUBR);                                                 }
INLINE bool PROCEDUREP(lref_t x)         { return CLOSUREP(x) || SUBRP(x);                                              }
INLINE bool MACROP(lref_t x)             { return REFTYPEP(x, TC_MACRO);                                                }
INLINE bool VALUES_TUPLE_P(lref_t x)     { return REFTYPEP(x, TC_VALUES_TUPLE);                                         }
INLINE bool FAST_OP_P(lref_t x)          { return REFTYPEP(x, TC_FAST_OP);                                              }
INLINE bool FASL_READER_P(lref_t x)      { return REFTYPEP(x, TC_FASL_READER);                                          }
INLINE bool TRUEP(lref_t x)              { return (x) != MAKE_LREF2(LREF2_BOOL, 0);                                     }
INLINE bool FALSEP(lref_t x)             { return !TRUEP(x);                                                            }

/*** Boxed data accessors ***/


/*** boolean ***/
INLINE lref_t boolcons(bool val)
{
     return MAKE_LREF2(LREF2_BOOL, val ? 1 : 0);
}

/*** cons/free-cell ***/

INLINE lref_t CAR(lref_t x)
{
     checked_assert(CONSP(x));
     return x->as.cons.car;
}

INLINE void SET_CAR(lref_t x, lref_t nv)
{
     checked_assert(CONSP(x));
     x->as.cons.car = nv;
}

INLINE lref_t CDR(lref_t x)
{
     checked_assert(CONSP(x));
     return x->as.cons.cdr;
}

INLINE void SET_CDR(lref_t x, lref_t nv)
{
     checked_assert(CONSP(x));
     x->as.cons.cdr = nv;
}


INLINE lref_t NEXT_FREE_LIST(lref_t x)
{
     checked_assert(FREE_CELL_P(x));
     return x->as.cons.car;
}

INLINE lref_t SET_NEXT_FREE_LIST(lref_t x, lref_t next)
{
     checked_assert(FREE_CELL_P(x));
     x->as.cons.car = next;

     return x;
}

INLINE lref_t NEXT_FREE_CELL(lref_t x)
{
     checked_assert(FREE_CELL_P(x));
     return x->as.cons.cdr;
}

INLINE lref_t SET_NEXT_FREE_CELL(lref_t x, lref_t next)
{
     checked_assert(TYPE(x) == TC_FREE_CELL);
     x->as.cons.cdr = next;

     return x;
}

/*** fix/flonum ***/

INLINE fixnum_t FIXNM(lref_t x)
{
     checked_assert(FIXNUMP(x));
     return LREF1_VAL(x);
}

INLINE flonum_t FLONM(lref_t x)
{
     checked_assert(FLONUMP(x));
     return x->as.flonum.data;
}


INLINE lref_t FLOIM(lref_t x)
{
     checked_assert(FLONUMP(x));

     return x->as.flonum.im_part;
}

INLINE flonum_t CMPLXIM(lref_t x)
{
     return FLONM(FLOIM(x));
}

/*** character ***/
INLINE _TCHAR CHARV(lref_t x)
{
     checked_assert(CHARP(x));

     return (_TCHAR) LREF2_VAL(x);
}

/*** structure ***/

INLINE size_t STRUCTURE_DIM(lref_t obj)
{
     checked_assert(STRUCTUREP(obj));
     return obj->as.vector.dim;
}

INLINE void SET_STRUCTURE_DIM(lref_t obj, size_t len)
{
     checked_assert(STRUCTUREP(obj));
     obj->as.vector.dim = len;
}

INLINE lref_t *STRUCTURE_DATA(lref_t obj)
{
     checked_assert(STRUCTUREP(obj));
     return obj->as.vector.data;
}

INLINE void SET_STRUCTURE_DATA(lref_t obj, lref_t * data)
{
     checked_assert(STRUCTUREP(obj));
     obj->as.vector.data = data;
}

INLINE lref_t STRUCTURE_LAYOUT(lref_t obj)
{
     checked_assert(STRUCTUREP(obj));
     return obj->as.vector.layout;
}

INLINE void SET_STRUCTURE_LAYOUT(lref_t obj, lref_t new_layout)
{
     checked_assert(STRUCTUREP(obj));
     obj->as.vector.layout = new_layout;
}

INLINE lref_t STRUCTURE_ELEM(lref_t obj, fixnum_t index)
{
     checked_assert(STRUCTUREP(obj));
     return obj->as.vector.data[index];
}

INLINE void SET_STRUCTURE_ELEM(lref_t obj, fixnum_t index, lref_t new_value)
{
     checked_assert(STRUCTUREP(obj));
     obj->as.vector.data[index] = new_value;
}

/*** structure layout ***/

INLINE lref_t SLAYOUT_NAME(lref_t obj)
{
     checked_assert(SLAYOUTP(obj));
     return obj->as.slayout.name;
}

INLINE void SET_SLAYOUT_NAME(lref_t obj, lref_t new_name)
{
     checked_assert(SLAYOUTP(obj));     
     obj->as.slayout.name = new_name;
}

INLINE lref_t SLAYOUT_SLOTS(lref_t obj)
{
     checked_assert(SLAYOUTP(obj));
     return obj->as.slayout.slots;
}

INLINE void SET_SLAYOUT_SLOTS(lref_t obj, lref_t new_slots)
{
     checked_assert(SLAYOUTP(obj));     
     obj->as.slayout.slots = new_slots;
}

/*** symbol ***/
INLINE lref_t SYMBOL_PNAME(lref_t sym)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).as.symbol.props));

     lref_t pname = NIL;

     if (STRINGP(sym->as.symbol.props))
          pname = sym->as.symbol.props;
     else
     {
          checked_assert(CONSP((*sym).as.symbol.props));
          pname = CAR(sym->as.symbol.props);
          checked_assert(STRINGP(pname));
     }

     return pname;
}

INLINE void SET_SYMBOL_PNAME(lref_t sym, lref_t pname)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(STRINGP(pname));

     sym->as.symbol.props = pname;
}

INLINE lref_t SYMBOL_PROPS(lref_t sym)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).as.symbol.props));

     if (STRINGP(sym->as.symbol.props))
          return NIL;

     checked_assert(CONSP((*sym).as.symbol.props));

     return CDR(sym->as.symbol.props);
}

lref_t lcons(lref_t x, lref_t y);     /*  Forward decl */

INLINE void SET_SYMBOL_PROPS(lref_t sym, lref_t props)
{
     checked_assert(SYMBOLP(sym));
     checked_assert(!NULLP((*sym).as.symbol.props));

     if (STRINGP(sym->as.symbol.props))
     {
          sym->as.symbol.props = lcons((*sym).as.symbol.props, props);
     }
     else
     {
          checked_assert(CONSP((*sym).as.symbol.props));
          SET_CDR(sym->as.symbol.props, props);
     }
}

INLINE lref_t SYMBOL_VCELL(lref_t sym)
{
     checked_assert(SYMBOLP(sym));
     return sym->as.symbol.vcell;
}

INLINE void SET_SYMBOL_VCELL(lref_t sym, lref_t value)
{
     checked_assert(SYMBOLP(sym));
     sym->as.symbol.vcell = value;
}

INLINE lref_t SYMBOL_HOME(lref_t x)
{
     checked_assert(SYMBOLP(x));
     return x->as.symbol.home;
}

INLINE void SET_SYMBOL_HOME(lref_t x, lref_t home)
{
     checked_assert(SYMBOLP(x));
     x->as.symbol.home = home;
}

/*** subr ***/
INLINE enum subr_arity_t SUBR_TYPE(lref_t x)
{
     checked_assert(SUBRP(x));
     return x->as.subr.type;
}

INLINE void SET_SUBR_TYPE(lref_t x, enum subr_arity_t type)
{
     checked_assert(SUBRP(x));
     x->as.subr.type = type;
}

INLINE lref_t SUBR_NAME(lref_t x)
{
     checked_assert(SUBRP(x));
     return x->as.subr.name;
}

INLINE void SET_SUBR_NAME(lref_t x, lref_t name)
{
     checked_assert(SUBRP(x));
     checked_assert(STRINGP(name));
     x->as.subr.name = name;
}

INLINE void SET_SUBR_CODE(lref_t x, void *code)
{
     x->as.subr.code.ptr = code;
}

INLINE void *SUBR_CODE(lref_t x)
{
     return (void *)x->as.subr.code.ptr;
}

INLINE f_0_t SUBR_F0(lref_t x)
{
     return x->as.subr.code.f_0;
}

INLINE f_1_t SUBR_F1(lref_t x)
{
     return x->as.subr.code.f_1;
}

INLINE f_2_t SUBR_F2(lref_t x)
{
     return x->as.subr.code.f_2;
}

INLINE f_3_t SUBR_F3(lref_t x)
{
     return x->as.subr.code.f_3;
}

INLINE f_4_t SUBR_F4(lref_t x)
{
     return x->as.subr.code.f_4;
}

INLINE f_argc_t SUBR_FARGC(lref_t x)
{
     return x->as.subr.code.f_argc;
}

/*** closure ***/
INLINE lref_t CLOSURE_CODE(lref_t x)
{
     checked_assert(CLOSUREP(x));
     return x->as.closure.code;
}

INLINE void SET_CLOSURE_CODE(lref_t x, lref_t code)
{
     checked_assert(CLOSUREP(x));
     x->as.closure.code = code;
}

INLINE lref_t CLOSURE_ENV(lref_t x)
{
     checked_assert(CLOSUREP(x));
     return x->as.closure.env;
}

INLINE void SET_CLOSURE_ENV(lref_t x, lref_t env)
{
     checked_assert(CLOSUREP(x));
     x->as.closure.env = env;
}

INLINE lref_t CLOSURE_PROPERTY_LIST(lref_t x)
{
     checked_assert(CLOSUREP(x));
     return x->as.closure.property_list;
}

INLINE void SET_CLOSURE_PROPERTY_LIST(lref_t x, lref_t plist)
{
     checked_assert(CLOSUREP(x));
     x->as.closure.property_list = plist;
}

/*** fasl-stream ***/

INLINE lref_t FASL_READER_PORT(lref_t obj)
{
     checked_assert(FASL_READER_P(obj));
     return obj->as.fasl_reader.port;
}

INLINE void SET_FASL_READER_PORT(lref_t obj, lref_t port)
{
     checked_assert(FASL_READER_P(obj));
     obj->as.fasl_reader.port = port;
}

INLINE struct fasl_stream_t *FASL_READER_STREAM(lref_t obj)
{
     checked_assert(FASL_READER_P(obj));
     return obj->as.fasl_reader.stream;
}

INLINE void SET_FASL_READER_STREAM(lref_t obj, struct fasl_stream_t *stream)
{
     checked_assert(FASL_READER_P(obj));
     obj->as.fasl_reader.stream = stream;
}

/*** port ***/
enum port_mode_t
{
     PORT_CLOSED = 0x00,
     PORT_INPUT = 0x01,
     PORT_OUTPUT = 0x02,
     PORT_TEXT = 0x04,

     PORT_DIRECTION = PORT_INPUT | PORT_OUTPUT
};

struct port_text_info_t
{
     /* peek-char buffer. */
     int pbuf;
     bool pbuf_valid;

     /* CR+LF translation */
     bool translate;
     bool needs_lf;

     /* Textual position within I/O stream. */
     fixnum_t col;
     fixnum_t row;
     fixnum_t pline_mcol; /* Ending col of previous row. */

     /* Position within input string. */
     size_t str_ofs;
};

struct fasl_stream_t
{
     lref_t table;
     lref_t stack[FAST_LOAD_STACK_DEPTH];
     size_t sp;
     lref_t accum;
};

struct port_info_t
{
     lref_t port_name;

     void *user_data;
     lref_t user_object;

     enum port_mode_t mode;

     size_t bytes_read;
};

struct port_class_t
{
     const _TCHAR *name;

     void   (* open)        (lref_t port);

     size_t (* read_bytes)  (lref_t port, void *buf, size_t size);
     size_t (* write_bytes) (lref_t port, const void *buf, size_t size);

     int    (* peek_char)   (lref_t port);
     size_t (* read_chars)  (lref_t port, _TCHAR *buf, size_t size);
     size_t (* write_chars) (lref_t port, const _TCHAR *buf, size_t size);

     bool   (* rich_write)  (lref_t port, lref_t obj, bool machine_readable);
     void   (* flush)       (lref_t port);
     void   (* close)       (lref_t port);
     void   (* gc_free)     (lref_t port);
     size_t (* length)      (lref_t port);
};

INLINE struct port_info_t *PORT_PINFO(lref_t x)
{
     checked_assert(PORTP(x));
     return x->as.port.pinf;
}

INLINE struct port_info_t *SET_PORT_PINFO(lref_t x, struct port_info_t * pinf)
{
     checked_assert(PORTP(x));
     return x->as.port.pinf = pinf;
}

INLINE struct port_class_t *PORT_CLASS(lref_t x)
{
     checked_assert(PORTP(x));
     return x->as.port.klass;
}

INLINE struct port_class_t *SET_PORT_CLASS(lref_t x, struct port_class_t * klass)
{
     checked_assert(PORTP(x));
     return x->as.port.klass = klass;
}

INLINE struct port_text_info_t *PORT_TEXT_INFO(lref_t x)
{
     checked_assert(PORTP(x));
     return x->as.port.text_info;
}

INLINE void SET_PORT_TEXT_INFO(lref_t x, struct port_text_info_t * text_info)
{
     checked_assert(PORTP(x));
     x->as.port.text_info = text_info;
}

INLINE enum port_mode_t PORT_MODE(lref_t x)
{
     checked_assert(PORTP(x));
     return PORT_PINFO(x)->mode;
}

INLINE void SET_PORT_MODE(lref_t x, enum port_mode_t mode)
{
     checked_assert(PORTP(x));
     PORT_PINFO(x)->mode = mode;
}

INLINE bool BINARY_PORTP(lref_t x)
{
     return PORTP(x) && ((PORT_MODE(x) & PORT_TEXT) != PORT_TEXT);
}

INLINE bool TEXT_PORTP(lref_t x)
{
     return PORTP(x) && ((PORT_MODE(x) & PORT_TEXT) == PORT_TEXT);
}

INLINE bool PORT_CLOSEDP(lref_t port)
{
     return (PORT_MODE(port) & PORT_DIRECTION) == PORT_CLOSED;
}

INLINE bool PORT_INPUTP(lref_t port)
{
     return (PORT_MODE(port) & PORT_INPUT);
}

INLINE bool PORT_OUTPUTP(lref_t port)
{
     return (PORT_MODE(port) & PORT_OUTPUT);
}

INLINE size_t PORT_BYTES_READ(lref_t x)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->bytes_read);
}

INLINE lref_t PORT_USER_OBJECT(lref_t x)
{
     checked_assert(PORTP(x));
     return (PORT_PINFO(x)->user_object);
}

INLINE void SET_PORT_USER_OBJECT(lref_t x, lref_t user_object)
{
     checked_assert(PORTP(x));
     PORT_PINFO(x)->user_object = user_object;
}


#endif
