
/* fasl.cpp
 * Februrary 26th, 2006
 *
 * This is the FASL loader: it reads FASL format objects from a binary port.
 */

#include "scan.h"

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
static void fast_read(LRef port, LRef * retval, bool allow_loader_ops = false);

static LRef fast_read_error(const _TCHAR * message, LRef port, LRef details = NIL)
{
     /*  REVISIT: fast_read_errors not always displayed */
     /*  REVISIT: fast_read_errors don't always show valid port locations */
     assert(PORTP(port));

     LRef location = lport_location(port);

     assert(FIXNUMP(location));

     _TCHAR buf[STACK_STRBUF_LEN];
     _sntprintf(buf, STACK_STRBUF_LEN, "Error Reading FASL File: %s @ 0x%08x.",
                message, (unsigned int) get_c_fixnum(location));

     return vmerror(buf, lcons(port, details));
}

static fasl_opcode_t fast_read_opcode(LRef port)
{
     fixnum_t opcode = FASL_OP_EOF;

     if (read_binary_fixnum(1, false, port, opcode))
          return (fasl_opcode_t) opcode;
     else
          return FASL_OP_EOF;
}

static void fast_read_list(LRef port, bool read_listd, LRef * list)
{
     *list = NIL;
     LRef list_bud = NIL;
     LRef next_list_cell = NIL;

     LRef list_length;
     fast_read(port, &list_length);

     if (!FIXNUMP(list_length))
          fast_read_error("expected fixnum for list length", port, list_length);

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
               fast_read_error("incomplete list definition", port);
     }

     if (read_listd)
     {
          fast_read(port, &_CDR(list_bud));

          if (EOFP(CDR(list_bud)))
               fast_read_error("incomplete list defintion, missing cdr", port);
     }
}

static void fast_read_character(LRef port, LRef * retval)
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


static void fast_read_integer(LRef port, size_t length, LRef * retval)
{
     fixnum_t data = 0;

     if (read_binary_fixnum(length, true, port, data))
          *retval = fixcons(data);
     else
          *retval = lmake_eof();
}

static void fast_read_float(LRef port, bool complex, LRef * retval)
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
                    fast_read_error("incomplete complex number", port);
          }
          else
               *retval = flocons(real_part);
     }
     else
          *retval = lmake_eof();
}


static void fast_read_string(LRef port, LRef * retval)
{
     LRef l;
     fast_read(port, &l);

     if (!FIXNUMP(l))
          vmerror("strings must have a fixnum length", l);

     fixnum_t expected_length = FIXNM(l);

     _TCHAR *buf = (_TCHAR *) safe_malloc((size_t) (expected_length + 1));

     memset(buf, 0, (size_t) (expected_length + 1));

     fixnum_t actual_length = read_raw(buf, sizeof(_TCHAR), (size_t) expected_length, port);

     if (actual_length != expected_length)
          fast_read_error("EOF during string data", port);

     *retval = strcons((size_t) actual_length, buf);
}

static void fast_read_package(LRef port, LRef * package)
{
     LRef name;
     fast_read(port, &name);

     if (!STRINGP(name))
          fast_read_error("packages must have string names", port, name);

     *package = lfind_package(name);

     if (FALSEP(*package))
          fast_read_error("package not found", port, name);
}

static void fast_read_symbol(LRef port, LRef * retval)
{
     LRef print_name;
     fast_read(port, &print_name);

     if (!STRINGP(print_name))
          fast_read_error("symbols must have string print names", port, print_name);

     LRef home;
     fast_read(port, &home);

     if (!(PACKAGEP(home) || NULLP(home) || FALSEP(home)))
          fast_read_error("a symbol must either have a package or NIL/#f for home", port, home);

     if (NULLP(home) || FALSEP(home))
          *retval = symcons(print_name, NIL);
     else
          *retval = simple_intern(print_name, home);

     if (*retval == NIL)
          fast_read_error("internal error creating symbol", port, print_name);
}

static void fast_read_subr(LRef port, LRef * retval)
{
     LRef subr_name;
     fast_read(port, &subr_name);

     if (!STRINGP(subr_name))
          fast_read_error("subrs must have string names", port, subr_name);

     LRef subr = find_subr_by_name(subr_name);

     if (NULLP(subr))
          fast_read_error("subr not found", port, subr_name);

     *retval = subr;
}


static void fast_read_vector(LRef port, LRef * vec)
{
     LRef vec_length;
     fast_read(port, &vec_length);

     if (!FIXNUMP(vec_length))
          fast_read_error("Expected fixnum for vector length", port, vec_length);

     *vec = vectorcons(FIXNM(vec_length), NIL);

     for (fixnum_t ii = 0; ii < FIXNM(vec_length); ii++)
     {
          LRef object;
          fast_read(port, &object);

          if (EOFP(object))
               fast_read_error("incomplete vector definition", port, *vec);

          SET_VECTOR_ELEM(*vec, ii, object);
     }
}

static void fast_read_structure_layout(LRef port, LRef * st_layout)
{
     LRef new_st_layout;
     fast_read(port, &new_st_layout);

     if (!CONSP(new_st_layout))
          fast_read_error("Expected list for structure layout", port, new_st_layout);

     LRef structure_type_name = CAR(new_st_layout);

     LRef existing_st_meta = lassq(simple_intern(_T("structure-meta"), interp.scheme_package),
                                   lproperty_list(structure_type_name));

     LRef existing_st_layout = NIL;

     if (TRUEP(existing_st_meta))
          existing_st_layout = lcar(lcdr(existing_st_meta));

     bool obsolete_structure = !equalp(new_st_layout, existing_st_layout);

     if (obsolete_structure)
          SET_CAR(new_st_layout, listn(2, keyword_intern(_T("orphaned")), CAR(new_st_layout)));
     else
          new_st_layout = existing_st_layout;

     *st_layout = new_st_layout;
}

static void fast_read_fast_op(int fast_op_arity, LRef port, LRef * fop)
{
     assert((fast_op_arity >= 0) && (fast_op_arity <= 3));

     LRef opcode_obj;
     fast_read(port, &opcode_obj);

     if (!FIXNUMP(opcode_obj))
          fast_read_error("Expected fixnum for opcode.", port, opcode_obj);

     *fop = fast_op((int) FIXNM(opcode_obj), NIL, NIL, NIL);

     LRef op_arg;

     if (fast_op_arity > 0)
     {
          fast_read(port, &op_arg);
          SET_FAST_OP_ARG1(*fop, op_arg);
     }

     if (fast_op_arity > 1)
     {
          fast_read(port, &op_arg);
          SET_FAST_OP_ARG2(*fop, op_arg);
     }

     if (fast_op_arity > 2)
     {
          fast_read(port, &op_arg);
          SET_FAST_OP_ARG3(*fop, op_arg);
     }
}

static void fast_read_structure(LRef port, LRef * st)
{
     LRef st_meta;
     fast_read(port, &st_meta);

     if (!CONSP(st_meta))
          fast_read_error("Expected list for structure metadata", port, st_meta);

     LRef structure_type_name = CAR(st_meta);

     LRef existing_st_meta = lassq(simple_intern(_T("structure-meta"), interp.scheme_package),
                                   lproperty_list(structure_type_name));

     if (TRUEP(existing_st_meta))
          existing_st_meta = lcar(lcdr(existing_st_meta));

     bool obsolete_structure = !equalp(st_meta, existing_st_meta);


     if (obsolete_structure)
          SET_CAR(st_meta, listn(2, keyword_intern(_T("orphaned")), CAR(st_meta)));
     else
          st_meta = existing_st_meta;

     LRef st_length;
     fast_read(port, &st_length);

     if (!FIXNUMP(st_length))
          fast_read_error("Expected fixnum for structure length", port, st_length);

     *st = lstructurecons(vectorcons(FIXNM(st_length), NIL), st_meta);

     for (fixnum_t ii = 0; ii < FIXNM(st_length); ii++)
     {
          LRef object;
          fast_read(port, &object);

          if (EOFP(object))
               fast_read_error("incomplete structure definition", port, *st);

          SET_STRUCTURE_ELEM(*st, ii, object);
     }
}

static void fast_read_instance_map(LRef port, LRef * new_instance)
{
     LRef proto = NIL;
     fast_read(port, &proto);

     if (!(INSTANCEP(proto) || FALSEP(proto) || SYMBOLP(proto)))
          fast_read_error("Bad prototype instance, must be #f, a symbol, or an instance", port,
                          proto);

     *new_instance = instancecons(proto);

     LRef slot_names = NIL;
     fast_read(port, &slot_names);

     if (!(CONSP(slot_names) || NULLP(slot_names)))
          fast_read_error("Bad instance map slot names, must be a list", port, slot_names);

     for (; CONSP(slot_names); slot_names = CDR(slot_names))
     {
          if (!SYMBOLP(CAR(slot_names)))
               fast_read_error("Bad instance map slot name.", port, CAR(slot_names));

          lislot_set(*new_instance, CAR(slot_names), NIL);
     }

     if (!NULLP(slot_names))
          fast_read_error("Malformed instance map slot name list.", port, slot_names);
}

static void fast_read_instance(LRef port, LRef * instance)
{
     LRef base_instance = NIL;
     fast_read(port, &base_instance);

     if (!INSTANCEP(base_instance))
          fast_read_error("Bad base instance.", port, base_instance);

     *instance = lclone_instance(base_instance);

     LRef vals;
     fast_read(port, &vals);

     for (size_t ii = 1; CONSP(vals); ii++, vals = CDR(vals))
          SET_INSTANCE_ELEM(*instance, ii, CAR(vals));

     if (!NULLP(vals))
          fast_read_error("Bad slot value list, must be a proper list.", port, vals);
}

static void fast_read_hash(LRef port, LRef * hash)
{
     LRef shallow;

     fast_read(port, &shallow);
     if (!BOOLP(shallow))
          fast_read_error("expected boolean for hash table shallow", port, shallow);

     *hash = hashcons(BOOLV(shallow));

     LRef elements;
     fast_read(port, &elements);

     LRef loc = NIL;

     for (loc = elements; CONSP(loc); loc = CDR(loc))
     {
          LRef kv = CAR(loc);

          if (!CONSP(kv))
               fast_read_error("malformed key/value in hash table", port, kv);

          lhash_set(*hash, CAR(kv), CDR(kv));
     }

     if (!NULLP(loc))
          fast_read_error("malformed key/value list for hash table", port, elements);
}

static void fast_read_closure(LRef port, LRef * retval)
{
     LRef env;
     fast_read(port, &env);

     if (EOFP(env))
          fast_read_error("incomplete closure, missing environment", port);

     if (!(NULLP(env) || CONSP(env)))
          fast_read_error("malformed closure, bad environment", port, env);

     LRef code;
     fast_read(port, &code);

     if (EOFP(code))
          fast_read_error("Incomplete closure, missing code", port, NIL);

     if (!(NULLP(code) || CONSP(code)))
          fast_read_error("malformed closure, bad code", port, code);

     LRef props;
     fast_read(port, &props);

     if (EOFP(props))
          fast_read_error("incomplete closure, missing property list", port);

     if (!(NULLP(props) || CONSP(props)))
          fast_read_error("malformed closure, bad property list", port, props);

     *retval = lclosurecons(env, code, props);
}

void fast_read_to_newline(LRef port)
{
     _TCHAR ch = _T('\0');

     while ((ch != _T('\n')) && (ch != _T('\r')))
          if (read_raw(&ch, sizeof(_TCHAR), 1, port) == 0)
               break;
}

void fast_read_macro(LRef port, LRef * retval)
{
     LRef macro_transformer;
     fast_read(port, &macro_transformer);

     if (!CLOSUREP(macro_transformer))
          fast_read_error("malformed macro, bad transformer", macro_transformer);

     *retval = macrocons(macro_transformer);
}

/* TODO: Fasl table entries move around upon resize, which can screw up FASL load if
 * the loader as a pointer into the fasl table during a resize. */

static void fasl_ensure_valid_table_index(LRef port, size_t index)
{
     if (NULLP(PORT_PINFO(port)->_fasl_table))
     {
          PORT_PINFO(port)->_fasl_table =
              vectorcons((index >=
                          DEFAULT_FASL_TABLE_SIZE) ? index +
                         DEFAULT_FASL_TABLE_SIZE : DEFAULT_FASL_TABLE_SIZE, NIL);
     }
     else
     {
          assert(VECTORP(PORT_PINFO(port)->_fasl_table));

          size_t old_len = VECTOR_DIM(PORT_PINFO(port)->_fasl_table);

          if (index >= old_len)
          {
               size_t new_len =
                   (index >= old_len * 2) ? index + DEFAULT_FASL_TABLE_SIZE : (old_len * 2);

               PORT_PINFO(port)->_fasl_table =
                   vector_resize(PORT_PINFO(port)->_fasl_table,
                                 new_len > SIZE_MAX ? SIZE_MAX : (size_t) new_len, NIL);
          }
     }

     assert(VECTORP(PORT_PINFO(port)->_fasl_table));
     assert(index < VECTOR_DIM(PORT_PINFO(port)->_fasl_table));
}

static fixnum_t fast_read_table_index(LRef port)
{
     LRef index;
     fast_read(port, &index);

     if (!FIXNUMP(index))
          fast_read_error("Expected fixnum for FASL table index", port, index);

     if (FIXNM(index) < 0)
          fast_read_error("FASL table indicies must be >=0", port, index);

     fasl_ensure_valid_table_index(port, (size_t) FIXNM(index));

     return FIXNM(index);
}

void fast_read_loader_definition(LRef port, fasl_opcode_t opcode)
{
     LRef symbol_to_define;

     fast_read(port, &symbol_to_define);

     if (!SYMBOLP(symbol_to_define))
          fast_read_error("Expected symbol for definition", port, symbol_to_define);

     LRef definition;
     fast_read(port, &definition);

     dscwritef(DF_SHOW_FAST_LOAD_FORMS,
               _T("; DEBUG: FASL defining ~s = ~s\n"), symbol_to_define, definition);

     switch (opcode)
     {
     case FASL_OP_LOADER_DEFINEQ:      /* quoted definition, do nothing. */
          break;
     case FASL_OP_LOADER_DEFINEA0:
          definition = napply(definition, 0);
          break;
     default:
          panic("invalid opcode in fast_read_loader_definition");
          break;
     }

     lidefine_global(symbol_to_define, definition, NIL);
}

void fast_read_loader_application(LRef port)
{
     LRef fn;
     fast_read(port, &fn);

     dscwritef(DF_SHOW_FAST_LOAD_FORMS, _T("; DEBUG: FASL applying ~s\n"), fn);

     napply(fn, 0);
}

static void fast_read(LRef port, LRef * retval, bool allow_loader_ops /* = false */ )
{
     LRef *fasl_table_entry = NULL;

     *retval = NIL;

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (!PORT_BINARYP(port))
          vmerror("Fast I/O requires a binary port", port);

     port_info_t *pinfo = PORT_PINFO(port);

     assert(NULLP(pinfo->_fasl_table) || VECTORP(pinfo->_fasl_table));

     /* The core of this function is wrapped in a giant while loop to remove
      * tail recursive calls. Some opcodes don't directly return anything:
      * they just tail recursively read the next opcode after performing their
      * action via side effect. */
     bool current_read_complete = false;
     while (!current_read_complete)
     {
          /*  Assume we're going to complete the read unless we find out otherwise.. */
          current_read_complete = true;

          LRef opcode_location = NIL;

          if (DEBUG_FLAG(DF_FASL_SHOW_OPCODES))
               opcode_location = lport_location(port);

          fasl_opcode_t opcode = fast_read_opcode(port);
          fixnum_t index = 0;
          LRef name;

          if (DEBUG_FLAG(DF_FASL_SHOW_OPCODES))
          {
               const _TCHAR *opcode_name = fasl_opcode_name(opcode);

               dscwritef(DF_FASL_SHOW_OPCODES,
                         _T("; DEBUG: fasl-opcode@~cx :~cS\n"),
                         get_c_long(opcode_location), opcode_name ? opcode_name : _T("<INVALID>"));
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
               fast_read_fast_op(0, port, retval);
               break;

          case FASL_OP_FAST_OP_1:
               fast_read_fast_op(1, port, retval);
               break;

          case FASL_OP_FAST_OP_2:
               fast_read_fast_op(2, port, retval);
               break;

          case FASL_OP_FAST_OP_3:
               fast_read_fast_op(3, port, retval);
               break;

          case FASL_OP_NOP_1:
          case FASL_OP_NOP_2:
          case FASL_OP_NOP_3:
               current_read_complete = false;
               break;

          case FASL_OP_INSTANCE_MAP:
               fast_read_instance_map(port, retval);
               break;

          case FASL_OP_COMMENT_1:
          case FASL_OP_COMMENT_2:
               fast_read_to_newline(port);
               current_read_complete = false;
               break;

          case FASL_OP_RESET_READER_DEFS:
               pinfo->_fasl_table = NIL;
               current_read_complete = false;
               break;

          case FASL_OP_READER_DEFINITION:
               index = fast_read_table_index(port);

               fasl_table_entry = &_VECTOR_ELEM(pinfo->_fasl_table, index);

               fast_read(port, fasl_table_entry, allow_loader_ops);

               /* TODO: This assert throws if the fasl table was resized during the reader definition. */
               assert(fasl_table_entry == &_VECTOR_ELEM(pinfo->_fasl_table, index));

               *retval = *fasl_table_entry;
               break;

          case FASL_OP_READER_REFERENCE:
               index = fast_read_table_index(port);

               *retval = VECTOR_ELEM(pinfo->_fasl_table, index);
               break;

          case FASL_OP_EOF:
               *retval = lmake_eof();
               break;

          case FASL_OP_LOADER_DEFINEQ:
          case FASL_OP_LOADER_DEFINEA0:
               if (!allow_loader_ops)
                    fast_read_error(_T("loader definitions not allowed outside loader"), port,
                                    lport_location(port));

               fast_read_loader_definition(port, opcode);
               current_read_complete = false;
               break;

          case FASL_OP_LOADER_APPLY0:
               if (!allow_loader_ops)
                    fast_read_error(_T("loader function applications not allowed outside loader"),
                                    port, lport_location(port));

               fast_read_loader_application(port);
               break;

          case FASL_OP_LOADER_APPLYN:
               panic("FASL_OP_APPLYN unimplemented"); /* XXX */
               break;

          case FASL_OP_BEGIN_LOAD_UNIT:
               if (!allow_loader_ops)
                    fast_read_error(_T("load units are not allowed outside loader"), port,
                                    lport_location(port));

               fast_read(port, &name, allow_loader_ops);

               dscwritef(DF_SHOW_FAST_LOAD_UNITS, "; DEBUG: FASL entering unit ~s\n", name);

               if (pinfo->_fasl_stack_ptr == FAST_LOAD_STACK_DEPTH - 1)
                    fast_read_error(_T("Package stack overflow."), port, lport_location(port));

               pinfo->_fasl_stack[pinfo->_fasl_stack_ptr] = CURRENT_PACKAGE();
               pinfo->_fasl_stack_ptr++;
               break;

          case FASL_OP_END_LOAD_UNIT:
               if (!allow_loader_ops)
                    fast_read_error(_T("load units are not allowed outside loader"), port,
                                    lport_location(port));

               fast_read(port, &name, allow_loader_ops);

               dscwritef(DF_SHOW_FAST_LOAD_UNITS, "; DEBUG: FASL leaving unit ~s\n", name);

               if (pinfo->_fasl_stack_ptr == 0)
                    fast_read_error(_T("Package stack underflow."), port, lport_location(port));

               pinfo->_fasl_stack_ptr--;

               if (!PACKAGEP(pinfo->_fasl_stack[pinfo->_fasl_stack_ptr]))
                    fast_read_error(_T("Non-package on package stack."), port,
                                    lport_location(port));

               SET_CURRENT_PACKAGE(pinfo->_fasl_stack[pinfo->_fasl_stack_ptr]);
               pinfo->_fasl_stack[pinfo->_fasl_stack_ptr] = NULL;
               break;

          case FASL_OP_PUSH:
               panic("FASL_OP_PUSH unimplemented"); /* XXX */
               break;

          case FASL_OP_DROP:
               panic("FASL_OP_DROP unimplemented"); /* XXX */
               break;

               
          default:
               fast_read_error("invalid opcode", port, fixcons(opcode));
          }
     }
}

LRef lfast_read(LRef port)
{
     LRef retval;

     fast_read(port, &retval);

     return retval;
}

LRef lifasl_load(LRef fname_or_port)
{
     LRef result = NIL;
     LRef port = NIL;

     LRef previous_package = CURRENT_PACKAGE();

     if (STRINGP(fname_or_port))
          port = lopen_input_file(fname_or_port, keyword_intern(_T("binary")));
     else if (PORTP(fname_or_port))
          port = fname_or_port;
     else
          vmerror_wrong_type(1, fname_or_port);

     ENTER_UNWIND_PROTECT()
     {
          dscwritef(DF_SHOW_FAST_LOAD_FORMS,
                    _T("; DEBUG: FASL from : ~a, *package* = ~a\n"), port, CURRENT_PACKAGE);

          LRef form = NIL;

          while (!EOFP(form))
          {
               fast_read(port, &form, true);
          }
     }
     ON_UNWIND()
     {
          SET_CURRENT_PACKAGE(previous_package);

          dscwritef(DF_SHOW_FAST_LOAD_FORMS, _T("; DEBUG: done FASL from port: ~a\n"), port);
          dscwritef(DF_SHOW_FAST_LOAD_FORMS, _T("; DEBUG: FASL result: ~s\n"), result);

          lclose_port(port);
     }
     LEAVE_UNWIND_PROTECT();

     return result;
}

END_NAMESPACE
