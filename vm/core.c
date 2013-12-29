
/*
 * core.cpp --
 *
 * A few core primitives.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */


#include <float.h>
#include <math.h>

#include "scan-private.h"


lref_t liimmediate_p(lref_t obj)
{
     return boolcons(LREF_IMMEDIATE_P(obj) || NULLP(obj));
}

/***** Boolean *****/

lref_t lbooleanp(lref_t x)
{
     return boolcons(BOOLP(x));
}

lref_t lnotp(lref_t x)
{
     return boolcons(!TRUEP(x));
}

/***** Equality tests *****/

lref_t leq(lref_t x, lref_t y)
{
     return boolcons(EQ(x, y));
}

lref_t leql(lref_t x, lref_t y)
{
     bool rc = false;

     if (EQ(x, y))
          rc = true;
     else if (!NUMBERP(x) || !NUMBERP(y))
          rc = false;
     else if (FLONUMP(x) && FLONUMP(y))
          rc = (FLONM(x) == FLONM(y));
     else if (FIXNUMP(x) && FIXNUMP(y))
          rc = (FIXNM(x) == FIXNM(y));

     return boolcons(rc);
}

bool equalp(lref_t a, lref_t b)
{
     enum typecode_t atype;

     STACK_CHECK(&a);

     if (EQ(a, b))
          return true;

     atype = TYPE(a);

     if (atype != TYPE(b))
          return false;

     switch (atype)
     {
     case TC_CONS:
          for (;;)
          {
               if (equalp(lcar(a), lcar(b)))
               {
                    a = lcdr(a);
                    b = lcdr(b);

                    if (!CONSP(a) || !CONSP(b))
                         return equalp(a, b);
               }
               else
                    return false;
          }
          break;

     case TC_FIXNUM:
          return (FIXNM(a) == FIXNM(b));

     case TC_FLONUM:
          /*  equal? considers NaN to be equal to itself. This is different
           *  from =, which uses the more mathematical approach that NaN
           *  is equal to nothing. */
          if (isnan(FLONM(a)) && isnan(FLONM(b)))
               return equalp(FLOIM(a), FLOIM(b));
          else
               return (FLONM(a) == FLONM(b)) && equalp(FLOIM(a), FLOIM(b));

     case TC_SYMBOL:
          return a == b;
     case TC_VECTOR:
          return vector_equal(a, b);
     case TC_STRUCTURE:
          return structure_equal(a, b);
     case TC_STRING:
          return string_equal(a, b);
     case TC_HASH:
          return hash_equal(a, b);
     case TC_INSTANCE:
          return instance_equal(a, b);
     case TC_FAST_OP:
          return fast_op_equal(a, b);
     default:
          return false;
     }
}

lref_t lequal(lref_t a, lref_t b)
{
     return boolcons(equalp(a, b));
}

lref_t lnullp(lref_t x)
{
     return boolcons(NULLP(x));
}

lref_t litypecode(lref_t obj)
{
     return fixcons(TYPE(obj));
}

/***** subrs *****/

lref_t lsubr_type_code(lref_t subr)
{
     if (!SUBRP(subr))
          vmerror_wrong_type_n(1, subr);

     return fixcons(SUBR_TYPE(subr));
}

lref_t lsubr_name(lref_t subr)
{
     if (!SUBRP(subr))
          vmerror_wrong_type_n(1, subr);

     return SUBR_NAME(subr);
}

lref_t subrcons(enum subr_arity_t type, lref_t name, void *implementation)
{
     lref_t z = new_cell(TC_SUBR);

     SET_SUBR_TYPE(z, type);
     SET_SUBR_NAME(z, name);
     SET_SUBR_CODE(z, implementation);

     return (z);
}

/*
 * This group of functions is responsible for registering subrs
 * with the current package
 */

void register_subr(const _TCHAR * name, enum subr_arity_t arity, void *implementation)
{
     assert(HASHP(interp.subr_table));
     assert(name != NULL);

     if (implementation == NULL)
          dscwritef(DF_ALWAYS, (";;;; NULL SUBR IMPLEMENTATION: \"~cs\"!\n", name));

     lref_t subr_name = strconsbuf(name);

     lref_t subr = subrcons(arity, subr_name, implementation);

     lhash_set(interp.subr_table, subr_name, subr);
}

lref_t find_subr_by_name(lref_t subr_name)
{
     assert(STRINGP(subr_name));
     assert(HASHP(interp.subr_table)); /*  REVISIT: Lisp-visible: rebind *subr-table* and invoke the fasl loader */

     lref_t argv[2];
     argv[0] = interp.subr_table;
     argv[1] = subr_name;

     return lhash_ref(2, argv);
}

lref_t lisubr_table()
{
     return interp.subr_table;
}

/***** closures *****/

lref_t lclosurecons(lref_t env, lref_t code, lref_t property_list)
{
     lref_t z = new_cell(TC_CLOSURE);

     if (!(CONSP(code) || NULLP(code)))
          vmerror_wrong_type_n(2, code);

     SET_CLOSURE_ENV(z, env);
     SET_CLOSURE_CODE(z, code);
     SET_CLOSURE_PROPERTY_LIST(z, property_list);

     return z;
}

lref_t lset_closure_code(lref_t exp, lref_t code)
{
     if (!CLOSUREP(exp))
          vmerror_wrong_type(exp);

     SET_CLOSURE_CODE(exp, code);

     return exp;
}

lref_t lclosure_code(lref_t exp)
{
     if (!CLOSUREP(exp))
          return boolcons(false);
     else
          return (CLOSURE_CODE(exp));
}

lref_t lset_closure_env(lref_t exp, lref_t env)
{
     if (!CLOSUREP(exp))
          vmerror_wrong_type(exp);

     SET_CLOSURE_ENV(exp, env);

     return exp;
}

lref_t lclosure_env(lref_t exp)
{
     if (!CLOSUREP(exp))
          return boolcons(false);
     else
          return (CLOSURE_ENV(exp));
}

lref_t lset_property_list(lref_t exp, lref_t property_list)
{
     if (CLOSUREP(exp))
          SET_CLOSURE_PROPERTY_LIST(exp, property_list);
     else if (SYMBOLP(exp))
          SET_SYMBOL_PROPS(exp, property_list);
     else
     {
          vmerror_wrong_type_n(1, exp);
          return NIL;           /*  unreached. */
     }

     return property_list;
}

lref_t lproperty_list(lref_t exp)
{
     if (CLOSUREP(exp))
          return CLOSURE_PROPERTY_LIST(exp);
     else if (SUBRP(exp))
          return NIL;
     else if (SYMBOLP(exp))
          return SYMBOL_PROPS(exp);
     else
          return NIL;
}

lref_t lprimitivep(lref_t obj)
{
     if (SUBRP(obj))
          return obj;
     else
          return boolcons(false);
}

lref_t lclosurep(lref_t obj)
{
     if (CLOSUREP(obj))
          return obj;
     else
          return boolcons(false);
}

lref_t lprocedurep(lref_t exp)
{
     if (PROCEDUREP(exp))
          return exp;
     else
          return boolcons(false);
}


/***** Control Fields *****/

static size_t get_control_field_id(lref_t control_field_id)
{
     if (!FIXNUMP(control_field_id))
          vmerror_wrong_type_n(1, control_field_id);

     size_t id = (size_t)FIXNM(control_field_id);

     if ((id < 0) || (id > VMCTRL_LAST))
          vmerror_arg_out_of_range(control_field_id, _T("[0,VMCTRL_LAST"));

     return id;
}

lref_t liset_control_field(lref_t control_field_id, lref_t new_value)
{
     interp.control_fields[get_control_field_id(control_field_id)] = new_value;

     return new_value;
}

lref_t licontrol_field(lref_t control_field_id)
{
     return interp.control_fields[get_control_field_id(control_field_id)];
}

/***** Values tuples *****/

lref_t lvalues(lref_t values)
{
     lref_t z = new_cell(TC_VALUES_TUPLE);

     SET_VALUES_TUPLE_VALUES(z, values);

     return z;
}

lref_t lvalues2list(lref_t obj)
{
     if (VALUES_TUPLE_P(obj))
          return VALUES_TUPLE_VALUES(obj);

     return lcons(obj, NIL);
}

/***** Fast-Ops *****/

lref_t fast_op(int opcode, lref_t arg1, lref_t arg2, lref_t next)
{
     lref_t z = new_cell(TC_FAST_OP);

     SET_FAST_OP_OPCODE(z, opcode);
     SET_FAST_OP_ARG1(z, arg1);
     SET_FAST_OP_ARG2(z, arg2);
     SET_FAST_OP_NEXT(z, next);

     return z;
}

lref_t lfast_op(lref_t opcode, lref_t arg1, lref_t arg2, lref_t next)
{
     if (!FIXNUMP(opcode))
          vmerror_wrong_type_n(1, opcode);
     if (!FAST_OP_P(next) && !NULLP(next))
          vmerror_wrong_type_n(2, next);

     return fast_op(FIXNM(opcode), arg1, arg2, next);
}

lref_t lfast_op_opcode(lref_t fastop)
{
     if (!FAST_OP_P(fastop))
          vmerror_wrong_type_n(1, fastop);

     return fixcons(FAST_OP_OPCODE(fastop));
}

lref_t lfast_op_args(lref_t fast_op)
{
     if (!FAST_OP_P(fast_op))
          vmerror_wrong_type_n(1, fast_op);

     return listn(2, FAST_OP_ARG1(fast_op), FAST_OP_ARG2(fast_op));
}

lref_t lfast_op_next(lref_t fast_op)
{
     if (!FAST_OP_P(fast_op))
          vmerror_wrong_type_n(1, fast_op);

     return FAST_OP_NEXT(fast_op);
}

bool fast_op_equal(lref_t a, lref_t b)
{
     assert(FAST_OP_P(a));
     assert(FAST_OP_P(b));

     if (FAST_OP_OPCODE(a) != FAST_OP_OPCODE(b))
          return false;

     if (FAST_OP_ARG1(a) != FAST_OP_ARG1(b))
          return false;

     if (FAST_OP_ARG2(a) != FAST_OP_ARG2(b))
          return false;

     if (FAST_OP_NEXT(a) != FAST_OP_NEXT(b))
          return false;

     return true;
}


