
/*
 * fasl.cpp --
 *
 * This is the FASL loader: it reads FASL format objects from a binary port.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <string.h>
#include <memory.h>

#include "scan-private.h"

/* REVISIT: Possible improvement to FASL read/write
 *
 * Currently, the FASL write process works by accumulating
 * a list of all objects to be written, scanning it to check
 * for shared structure, and then writing it to the stream,
 * adding appropriate definition and reference ops to recreate
 * structure sharing. This works, but it's slow and has the
 * downwside that if an object is mutated before the FASL stream
 * is written to disk, the mutatation is what will be written.
 *
 * Switching to a single pass FASL write algorithm would solve
 * a number of these problems. The FASL writer would immediately
 * write objects to the output stream, making a note of each object
 * as it is written, along with its byte offset in the output stream.
 * If an object is written a second time, a reference to the byte
 * offset is written to the stream instead of the object itself.
 * The writer also makes a note of each of these back-references,
 * and, when the output stream is closed, goes back to fix up
 * reference targets with alternate FASL opcode that signals the
 * reader to keep a record of the object as it is read. This
 * would entail the following specific changes:
 *
 * 1) Create a duplicate set of FASL opcodes for each type. The
 *    new opcode signals the reader to make a record of the
 *    object as it is read.
 * 2) Add a stream-seek function to binary output streams, to
 *    allow for writer fixup.
 * 3) Replace the LIST and LISTD opcdes with a CONS opcode. (The
 *    back-reference fixup approach requires each individually
 *    referrable Lisp object to have a unique byte offset within
 *    the output stream.)
 *
 * I know for a fact that this would eliminate the object mutation
 * problem, and I'm guessing that the elimination of the explicit
 * circularity-checking pass would speed things up quite a bit.
 * However, this gain would be somewhat offset by the fact that
 * the writer still has to maintain a per-written-object hash table,
 * so it's worth more formal investigation.
 */

BEGIN_NAMESPACE(scan)

/* This code depends on using an output paramater, rather than a
 * function return code. For self-referential data structures, this
 * allows the reader definition table be updated before the definition
 * is referenced. However, if a return paramater were used, the definition
 * table wouldn't be updated until the object read was complete. Circular
 * structures therefore wouldn't pick up the correct reference.
 *
 * This has two implications for you:
 *
 * 1. Do not change this code to use ordinary function return values.
 * 2. When writing code that reads a composite object, be sure
 *    to update the return value prior to reading any component objects.
 */

/*  REVISIT: The use of reference paramaters breaks when the reference
 *  points into the _fasl_table and the FASL table is resized while the
 *  reference is still pending. This needs to be fixed, otherwise funky
 *  stuff happens. (I think this can be resolveb by storig conses in the
 *  fasl_table and using references to their CAR's to store table entries...
 *  currently, the CONS will stay put even if the enderlying table is resized. */
static void fast_read(lref_t port, lref_t * retval, bool allow_loader_ops = false);

static fasl_opcode_t fast_read_opcode(lref_t port)
{
     fixnum_t opcode = FASL_OP_EOF;

     if (read_binary_fixnum(1, false, port, opcode))
          return (fasl_opcode_t) opcode;
     else
          return FASL_OP_EOF;
}

static void fast_read_list(lref_t port, bool read_listd, lref_t * list)
{
     *list = NIL;
     lref_t list_bud = NIL;
     lref_t next_list_cell = NIL;

     lref_t list_length;
     fast_read(port, &list_length);

     if (!FIXNUMP(list_length))
          vmerror_fast_read("expected fixnum for list length", port, list_length);

     *list = NIL;

     for (fixnum_t ii = 0; ii < FIXNM(list_length); ii++)
     {
          next_list_cell = lcons(NIL, NIL);

          if (NULLP(*list))
               *list = next_list_cell;
          else
               SET_CDR(list_bud, next_list_cell);

          list_bud = next_list_cell;

          fast_read(port, &_CAR(next_list_cell));

          if (EOFP(CAR(next_list_cell)))
               vmerror_fast_read("incomplete list definition", port);
     }

     if (read_listd)
     {
          fast_read(port, &_CDR(list_bud));

          if (EOFP(CDR(list_bud)))
               vmerror_fast_read("incomplete list defintion, missing cdr", port);
     }
}

static void fast_read_character(lref_t port, lref_t * retval)
{
     fixnum_t data = 0;

     if (read_binary_fixnum(1, false, port, data))
     {
          assert((data >= _TCHAR_MIN) && (data <= _TCHAR_MAX));

          *retval = charcons((_TCHAR) data);
     }
     else
          *retval = lmake_eof();
}


static void fast_read_integer(lref_t port, size_t length, lref_t * retval)
{
     fixnum_t data = 0;

     if (read_binary_fixnum(length, true, port, data))
          *retval = fixcons(data);
     else
          *retval = lmake_eof();
}

static void fast_read_float(lref_t port, bool complex, lref_t * retval)
{
     flonum_t real_part = 0.0;
     flonum_t imag_part = 0.0;

     if (read_binary_flonum(port, real_part))
     {
          if (complex)
          {
               if (read_binary_flonum(port, imag_part))
                    *retval = cmplxcons(real_part, imag_part);
               else
                    vmerror_fast_read("incomplete complex number", port);
          }
          else
               *retval = flocons(real_part);
     }
     else
          *retval = lmake_eof();
}


static void fast_read_string(lref_t port, lref_t * retval)
{
     lref_t l;
     fast_read(port, &l);

     if (!FIXNUMP(l))
          vmerror_fast_read("strings must have a fixnum length", port);

     fixnum_t expected_length = FIXNM(l);

     _TCHAR *buf = (_TCHAR *) gc_malloc((size_t) (expected_length + 1));

     memset(buf, 0, (size_t) (expected_length + 1));

     fixnum_t actual_length = read_raw(buf, sizeof(_TCHAR), (size_t) expected_length, port);

     if (actual_length != expected_length)
          vmerror_fast_read("EOF during string data", port);

     *retval = strcons((size_t) actual_length, buf);
}


lref_t lset_fasl_package_list(lref_t packages)
{
     interp.fasl_package_list = packages;

     return NIL;
}

static lref_t find_package(lref_t name)
{
     _TCHAR *n = get_c_string(name);

     for (lref_t l = interp.fasl_package_list; CONSP(l); l = CDR(l))
     {
          lref_t p = CAR(l);

          if (!PACKAGEP(p))
               panic("damaged package list");

          if (_tcscmp(n, get_c_string(PACKAGE_NAME(p))) == 0)
               return p;
     }

     return boolcons(false);
}

static void fast_read_package(lref_t port, lref_t * package)
{
     lref_t name;
     fast_read(port, &name);

     if (!STRINGP(name))
          vmerror_fast_read("packages must have string names", port, name);

     *package = find_package(name);

     if (FALSEP(*package))
          vmerror_fast_read("package not found", port, name);
}

static void fast_read_symbol(lref_t port, lref_t * retval)
{
     lref_t print_name;
     fast_read(port, &print_name);

     if (!STRINGP(print_name))
          vmerror_fast_read("symbols must have string print names", port, print_name);

     lref_t home;
     fast_read(port, &home);

     if (!(PACKAGEP(home) || NULLP(home) || FALSEP(home)))
          vmerror_fast_read("a symbol must either have a package or NIL/#f for home", port, home);

     if (NULLP(home) || FALSEP(home))
          *retval = symcons(print_name, NIL);
     else
          *retval = simple_intern(print_name, home);

     if (*retval == NIL)
          vmerror_fast_read("internal error creating symbol", port, print_name);
}

static void fast_read_subr(lref_t port, lref_t * retval)
{
     lref_t subr_name;
     fast_read(port, &subr_name);

     if (!STRINGP(subr_name))
          vmerror_fast_read("subrs must have string names", port, subr_name);

     lref_t subr = find_subr_by_name(subr_name);

     if (NULLP(subr))
          vmerror_fast_read("subr not found", port, subr_name);

     *retval = subr;
}


static void fast_read_vector(lref_t port, lref_t * vec)
{
     lref_t vec_length;
     fast_read(port, &vec_length);

     if (!FIXNUMP(vec_length))
          vmerror_fast_read("Expected fixnum for vector length", port, vec_length);

     *vec = vectorcons(FIXNM(vec_length), NIL);

     for (fixnum_t ii = 0; ii < FIXNM(vec_length); ii++)
     {
          lref_t object;
          fast_read(port, &object);

          if (EOFP(object))
               vmerror_fast_read("incomplete vector definition", port, *vec);

          SET_VECTOR_ELEM(*vec, ii, object);
     }
}

static void fast_read_structure_layout(lref_t port, lref_t * st_layout)
{
     lref_t new_st_layout;
     fast_read(port, &new_st_layout);

     *st_layout = vmtrap(TRAP_RESOLVE_FASL_STRUCT_LAYOUT, VMT_MANDATORY_TRAP, 1, new_st_layout);
}

static void fast_read_fast_op(int fast_op_arity, bool has_next, lref_t port, lref_t * fop)
{
     assert((fast_op_arity >= 0) && (fast_op_arity <= 2));

     lref_t opcode_obj;
     fast_read(port, &opcode_obj);

     if (!FIXNUMP(opcode_obj))
          vmerror_fast_read("Expected fixnum for opcode.", port, opcode_obj);

     lref_t op_arg1 = NIL;
     lref_t op_arg2 = NIL;
     lref_t next = NIL;

     if (fast_op_arity > 0)
          fast_read(port, &op_arg1);

     if (fast_op_arity > 1)
          fast_read(port, &op_arg2);

     if (has_next)
          fast_read(port, &next);

     *fop = fast_op((int) FIXNM(opcode_obj), op_arg1, op_arg2, next);
}

static void fast_read_structure(lref_t port, lref_t * st)
{
     lref_t st_meta;
     fast_read(port, &st_meta); // REVISIT: This has to come from the structure layour resolution vmtrap. Find a way to enforce this.

     if (!CONSP(st_meta))
          vmerror_fast_read("Expected list for structure metadata", port, st_meta);

     lref_t st_length;
     fast_read(port, &st_length);

     if (!FIXNUMP(st_length))
          vmerror_fast_read("Expected fixnum for structure length", port, st_length);

     *st = lstructurecons(vectorcons(FIXNM(st_length), NIL), st_meta);

     for (fixnum_t ii = 0; ii < FIXNM(st_length); ii++)
     {
          lref_t object;
          fast_read(port, &object);

          if (EOFP(object))
               vmerror_fast_read("incomplete structure definition", port, *st);

          SET_STRUCTURE_ELEM(*st, ii, object);
     }
}


static void fast_read_instance(lref_t port, lref_t * instance)
{
     lref_t proto = NIL;
     fast_read(port, &proto);


     if (!(INSTANCEP(proto) || FALSEP(proto) || SYMBOLP(proto)))
          vmerror_fast_read("Bad prototype instance, must be #f, a symbol, or an instance", port,
                          proto);

     *instance = liinstancecons(proto);

     lref_t elements;
     fast_read(port, &elements);

     lref_t loc = NIL;
     for (loc = elements; CONSP(loc); loc = CDR(loc))
     {
          lref_t kv = CAR(loc);

          if (!CONSP(kv))
               vmerror_fast_read("malformed slot-name/value in instance", port, kv);

          if (!SYMBOLP(CAR(kv)))
               vmerror_fast_read("Bad instance slot name.", port, CAR(kv));

          lhash_set(INSTANCE_SLOTS(*instance), CAR(kv), CDR(kv));
     }

     if (!NULLP(loc))
          vmerror_fast_read("malformed slot list for instance", port, elements);
}

static void fast_read_hash(lref_t port, lref_t * hash)
{
     lref_t shallow;

     fast_read(port, &shallow);
     if (!BOOLP(shallow))
          vmerror_fast_read("expected boolean for hash table shallow", port, shallow);

     *hash = hashcons(BOOLV(shallow));

     lref_t elements;
     fast_read(port, &elements);

     lref_t loc = NIL;

     for (loc = elements; CONSP(loc); loc = CDR(loc))
     {
          lref_t kv = CAR(loc);

          if (!CONSP(kv))
               vmerror_fast_read("malformed key/value in hash table", port, kv);

          lhash_set(*hash, CAR(kv), CDR(kv));
     }

     if (!NULLP(loc))
          vmerror_fast_read("malformed key/value list for hash table", port, elements);
}

static void fast_read_closure(lref_t port, lref_t * retval)
{
     lref_t env;
     fast_read(port, &env);

     if (EOFP(env))
          vmerror_fast_read("incomplete closure, missing environment", port);

     if (!(NULLP(env) || CONSP(env)))
          vmerror_fast_read("malformed closure, bad environment", port, env);

     lref_t code;
     fast_read(port, &code);

     if (EOFP(code))
          vmerror_fast_read("Incomplete closure, missing code", port, NIL);

     if (!(NULLP(code) || CONSP(code)))
          vmerror_fast_read("malformed closure, bad code", port, code);

     lref_t props;
     fast_read(port, &props);

     if (EOFP(props))
          vmerror_fast_read("incomplete closure, missing property list", port);

     if (!(NULLP(props) || CONSP(props)))
          vmerror_fast_read("malformed closure, bad property list", port, props);

     *retval = lclosurecons(env, code, props);
}

void fast_read_to_newline(lref_t port)
{
     _TCHAR ch = _T('\0');

     while ((ch != _T('\n')) && (ch != _T('\r')))
          if (read_raw(&ch, sizeof(_TCHAR), 1, port) == 0)
               break;
}

void fast_read_macro(lref_t port, lref_t * retval)
{
     lref_t macro_transformer;
     fast_read(port, &macro_transformer);

     if (!CLOSUREP(macro_transformer))
          vmerror_fast_read("malformed macro, bad transformer", macro_transformer);

     *retval = macrocons(macro_transformer);
}

/* REVISIT: Fasl table entries move around upon resize, which can screw up FASL load if
 * the loader as a pointer into the fasl table during a resize. */

static void fasl_ensure_valid_table_index(lref_t port, size_t index)
{
     if (NULLP(PORT_PINFO(port)->_fasl_stream->_table))
     {
          PORT_PINFO(port)->_fasl_stream->_table =
              vectorcons((index >=
                          DEFAULT_FASL_TABLE_SIZE) ? index +
                         DEFAULT_FASL_TABLE_SIZE : DEFAULT_FASL_TABLE_SIZE, NIL);
     }
     else
     {
          assert(VECTORP(PORT_PINFO(port)->_fasl_stream->_table));

          size_t old_len = VECTOR_DIM(PORT_PINFO(port)->_fasl_stream->_table);

          if (index >= old_len)
          {
               size_t new_len =
                   (index >= old_len * 2) ? index + DEFAULT_FASL_TABLE_SIZE : (old_len * 2);

               PORT_PINFO(port)->_fasl_stream->_table =
                   vector_resize(PORT_PINFO(port)->_fasl_stream->_table,
                                 new_len > SIZE_MAX ? SIZE_MAX : (size_t) new_len, NIL);
          }
     }

     assert(VECTORP(PORT_PINFO(port)->_fasl_stream->_table));
     assert(index < VECTOR_DIM(PORT_PINFO(port)->_fasl_stream->_table));
}

static fixnum_t fast_read_table_index(lref_t port)
{
     lref_t index;
     fast_read(port, &index);

     if (!FIXNUMP(index))
          vmerror_fast_read("Expected fixnum for FASL table index", port, index);

     if (FIXNM(index) < 0)
          vmerror_fast_read("FASL table indicies must be >=0", port, index);

     fasl_ensure_valid_table_index(port, (size_t) FIXNM(index));

     return FIXNM(index);
}

void fast_read_loader_definition(lref_t port, fasl_opcode_t opcode)
{
     lref_t symbol_to_define;

     fast_read(port, &symbol_to_define);

     if (!SYMBOLP(symbol_to_define))
          vmerror_fast_read("Expected symbol for definition", port, symbol_to_define);

     lref_t definition;
     fast_read(port, &definition);

     dscwritef(DF_SHOW_FAST_LOAD_FORMS,
               (_T("; DEBUG: FASL defining ~s = ~s\n"), symbol_to_define, definition));

     switch (opcode)
     {
     case FASL_OP_LOADER_DEFINEQ:      /* quoted definition, do nothing. */
          break;
     case FASL_OP_LOADER_DEFINEA0:
          definition = apply1(definition, 0, NULL);
          break;
     default:
          panic("invalid opcode in fast_read_loader_definition");
          break;
     }

     lidefine_global(symbol_to_define, definition);
}

static void fast_loader_stack_push(lref_t port, lref_t val)
{
     assert(PORTP(port) && PORT_BINARYP(port));

     port_info_t *pinfo = PORT_PINFO(port);

     if (pinfo->_fasl_stream->_sp == FAST_LOAD_STACK_DEPTH - 1)
          vmerror_fast_read(_T("Fast loader stack overflow."), port, lport_location(port));

     pinfo->_fasl_stream->_stack[pinfo->_fasl_stream->_sp] = val;
     pinfo->_fasl_stream->_sp++;
}

static lref_t fast_loader_stack_pop(lref_t port)
{
     lref_t val = NIL;

     assert(PORTP(port) && PORT_BINARYP(port));

     port_info_t *pinfo = PORT_PINFO(port);

     if (pinfo->_fasl_stream->_sp == 0)
          vmerror_fast_read(_T("Fast loader stack underflow."), port, lport_location(port));

     pinfo->_fasl_stream->_sp--;

     val = pinfo->_fasl_stream->_stack[pinfo->_fasl_stream->_sp];
     pinfo->_fasl_stream->_stack[pinfo->_fasl_stream->_sp] = NULL;

     return val;
}

void fast_read_loader_application(lref_t port, fasl_opcode_t opcode)
{
     assert(PORTP(port) && PORT_BINARYP(port));

     port_info_t *pinfo = PORT_PINFO(port);

     size_t argc = 0;
     lref_t argv[FAST_LOAD_STACK_DEPTH];

     fast_read(port, &argv[0]);

     if (!(SUBRP(argv[0]) || CLOSUREP(argv[0])))
          vmerror_fast_read(_T("Invalid function to apply"), port, lport_location(port));

     if (opcode == FASL_OP_LOADER_APPLYN)
     {
          lref_t ac;

          fast_read(port, &ac);

          if (!FIXNUMP(ac))
               vmerror_fast_read("Expected fixnum for loader application argc", port, ac);

          argc = (size_t)FIXNM(ac);

          if (argc > FAST_LOAD_STACK_DEPTH) /* Assuming FAST_LOAD_STACK_DEPTH <= ARG_BUF_LEN - 2 */
               vmerror_fast_read("Loader application argc too high", port, ac);

          for(size_t ii = 0; ii < argc; ii++)
               argv[ii + 1] = fast_loader_stack_pop(port);

          /* Fake a final NIL argument so that we can pass in the argv arguments
           * as scalars rather than as a list. */
          argc++;
          argv[argc] = NIL;
     }
     else if (opcode != FASL_OP_LOADER_APPLY0)
          panic("invalid opcode in fast_read_loader_application");

     dscwritef(DF_SHOW_FAST_LOAD_FORMS, (_T("; DEBUG: FASL applying ~s (argc=~cd)\n"), argv[0], argc));

     pinfo->_fasl_stream->_accum = lapply(argc + 1, argv);
}

static void fast_read(lref_t port, lref_t * retval, bool allow_loader_ops /* = false */ )
{
     lref_t *fasl_table_entry = NULL;

     *retval = NIL;

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("cannot perform fast I/O on text ports."));


     port_info_t *pinfo = PORT_PINFO(port);

     assert(NULLP(pinfo->_fasl_stream->_table) || VECTORP(pinfo->_fasl_stream->_table));

     /* The core of this function is wrapped in a giant while loop to remove
      * tail recursive calls. Some opcodes don't directly return anything:
      * they just tail recursively read the next opcode after performing their
      * action via side effect. */
     bool current_read_complete = false;
     while (!current_read_complete)
     {
          /*  Assume we're going to complete the read unless we find out otherwise.. */
          current_read_complete = true;

          lref_t opcode_location = NIL;

          if (DEBUG_FLAG(DF_FASL_SHOW_OPCODES))
               opcode_location = lport_location(port);

          fasl_opcode_t opcode = fast_read_opcode(port);
          fixnum_t index = 0;
          lref_t name;

          if (DEBUG_FLAG(DF_FASL_SHOW_OPCODES))
          {
               const _TCHAR *opcode_name = fasl_opcode_name(opcode);

               dscwritef(DF_FASL_SHOW_OPCODES,
                         (_T("; DEBUG: fasl-opcode@~cx :~cS\n"),
                          get_c_long(opcode_location), opcode_name ? opcode_name : _T("<INVALID>")));
          }

          switch (opcode)
          {
          case FASL_OP_NIL:
               *retval = NIL;
               break;

          case FASL_OP_TRUE:
               *retval = boolcons(true);
               break;

          case FASL_OP_FALSE:
               *retval = boolcons(false);
               break;

          case FASL_OP_CHARACTER:
               fast_read_character(port, retval);
               break;

          case FASL_OP_LIST:
               fast_read_list(port, false, retval);
               break;

          case FASL_OP_LISTD:
               fast_read_list(port, true, retval);
               break;

          case FASL_OP_FIX8:
               fast_read_integer(port, 1, retval);
               break;

          case FASL_OP_FIX16:
               fast_read_integer(port, 2, retval);
               break;

          case FASL_OP_FIX32:
               fast_read_integer(port, 4, retval);
               break;

          case FASL_OP_FIX64:
               fast_read_integer(port, 8, retval);
               break;

          case FASL_OP_FLOAT:
               fast_read_float(port, false, retval);
               break;

          case FASL_OP_COMPLEX:
               fast_read_float(port, true, retval);
               break;

          case FASL_OP_STRING:
               fast_read_string(port, retval);
               break;

          case FASL_OP_PACKAGE:
               fast_read_package(port, retval);
               break;

          case FASL_OP_VECTOR:
               fast_read_vector(port, retval);
               break;

          case FASL_OP_INSTANCE:
               fast_read_instance(port, retval);
               break;

          case FASL_OP_HASH:
               fast_read_hash(port, retval);
               break;

          case FASL_OP_CLOSURE:
               fast_read_closure(port, retval);
               break;

          case FASL_OP_MACRO:
               fast_read_macro(port, retval);
               break;

          case FASL_OP_SYMBOL:
               fast_read_symbol(port, retval);
               break;

          case FASL_OP_SUBR:
               fast_read_subr(port, retval);
               break;

          case FASL_OP_STRUCTURE:
               fast_read_structure(port, retval);
               break;

          case FASL_OP_STRUCTURE_LAYOUT:
               fast_read_structure_layout(port, retval);
               break;

          case FASL_OP_FAST_OP_0:
               fast_read_fast_op(0, false, port, retval);
               break;

          case FASL_OP_FAST_OP_1:
               fast_read_fast_op(1, false, port, retval);
               break;

          case FASL_OP_FAST_OP_2:
               fast_read_fast_op(2, false, port, retval);
               break;

          case FASL_OP_FAST_OP_0N:
               fast_read_fast_op(0, true, port, retval);
               break;

          case FASL_OP_FAST_OP_1N:
               fast_read_fast_op(1, true, port, retval);
               break;

          case FASL_OP_FAST_OP_2N:
               fast_read_fast_op(2, true, port, retval);
               break;

          case FASL_OP_NOP_1:
          case FASL_OP_NOP_2:
          case FASL_OP_NOP_3:
               current_read_complete = false;
               break;

          case FASL_OP_COMMENT_1:
          case FASL_OP_COMMENT_2:
               fast_read_to_newline(port);
               current_read_complete = false;
               break;

          case FASL_OP_RESET_READER_DEFS:
               pinfo->_fasl_stream->_table = NIL;
               current_read_complete = false;
               break;

          case FASL_OP_READER_DEFINITION:
               index = fast_read_table_index(port);

               fasl_table_entry = &_VECTOR_ELEM(pinfo->_fasl_stream->_table, index);

               fast_read(port, fasl_table_entry, allow_loader_ops);

               /* REVISIT: This assert throws if the fasl table was resized during the reader definition. */
               assert(fasl_table_entry == &_VECTOR_ELEM(pinfo->_fasl_stream->_table, index));

               *retval = *fasl_table_entry;
               break;

          case FASL_OP_READER_REFERENCE:
               index = fast_read_table_index(port);

               *retval = VECTOR_ELEM(pinfo->_fasl_stream->_table, index);
               break;

          case FASL_OP_EOF:
               *retval = lmake_eof();
               break;

          case FASL_OP_LOADER_DEFINEQ:
          case FASL_OP_LOADER_DEFINEA0:
               if (!allow_loader_ops)
                    vmerror_fast_read(_T("loader definitions not allowed outside loader"), port,
                                    lport_location(port));

               fast_read_loader_definition(port, opcode);
               current_read_complete = false;
               break;

          case FASL_OP_LOADER_APPLY0:
          case FASL_OP_LOADER_APPLYN:
               if (!allow_loader_ops)
                    vmerror_fast_read(_T("loader function applications not allowed outside loader"),
                                    port, lport_location(port));

               fast_read_loader_application(port, opcode);
               break;

          case FASL_OP_BEGIN_LOAD_UNIT:
               if (!allow_loader_ops)
                    vmerror_fast_read(_T("load units are not allowed outside loader"), port,
                                    lport_location(port));

               fast_read(port, &name, allow_loader_ops);

               dscwritef(DF_SHOW_FAST_LOAD_UNITS, ("; DEBUG: FASL entering unit ~s\n", name));
               break;

          case FASL_OP_END_LOAD_UNIT:
               if (!allow_loader_ops)
                    vmerror_fast_read(_T("load units are not allowed outside loader"), port,
                                    lport_location(port));

               fast_read(port, &name, allow_loader_ops);

               dscwritef(DF_SHOW_FAST_LOAD_UNITS, ("; DEBUG: FASL leaving unit ~s\n", name));
               break;

          case FASL_OP_LOADER_PUSH:
               fast_loader_stack_push(port, pinfo->_fasl_stream->_accum);
               break;

          case FASL_OP_LOADER_DROP:
               fast_loader_stack_pop(port);
               break;

               
          default:
               vmerror_fast_read("invalid opcode", port, fixcons(opcode));
          }
     }
}

lref_t lfast_read(lref_t port)
{
     lref_t retval;

     fast_read(port, &retval);

     return retval;
}

lref_t liifasl_load(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     dscwritef(DF_SHOW_FAST_LOAD_FORMS, (_T("; DEBUG: FASL from : ~a\n"), port));
     
     lref_t form = NIL;

     while (!EOFP(form))
          fast_read(port, &form, true);

     dscwritef(DF_SHOW_FAST_LOAD_FORMS, (_T("; DEBUG: done FASL from port: ~a\n"), port));

     return NIL;
}

END_NAMESPACE
