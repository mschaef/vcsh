
/* main.cpp
 *
 * The core scan interpreter
 */

#include <float.h>

#include "scan.h"

BEGIN_NAMESPACE(scan)
LRef liimmediate_p(LRef obj)
{
     return boolcons(LREF_IMMEDIATE_P(obj) || NULLP(obj));
}

/***** Boolean *****/

LRef lbooleanp(LRef x)
{
     return boolcons(BOOLP(x));
}

LRef lnotp(LRef x)
{
     return boolcons(!TRUEP(x));
}

/***** Equality tests *****/

LRef leq(LRef x, LRef y)
{
     return boolcons(EQ(x, y));
}

LRef leql(LRef x, LRef y)
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

bool equalp(LRef a, LRef b)
{
     typecode_t atype;

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
     case TC_GENV:
          return a == b;
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

LRef lequal(LRef a, LRef b)
{
     return boolcons(equalp(a, b));
}

LRef lnullp(LRef x)
{
     return boolcons(NULLP(x));
}

LRef litypecode(LRef obj)
{
     return fixcons(TYPE(obj));
}

/***** subrs *****/

LRef lsubr_type_code(LRef subr)
{
     if (!SUBRP(subr))
          vmerror_wrong_type(1, subr);

     return fixcons(SUBR_TYPE(subr));
}

LRef lsubr_name(LRef subr)
{
     if (!SUBRP(subr))
          vmerror_wrong_type(1, subr);

     return SUBR_NAME(subr);
}

LRef subrcons(subr_arity_t type, LRef name, void *implementation)
{
     LRef z = new_cell(TC_SUBR);

     SET_SUBR_TYPE(z, type);
     SET_SUBR_NAME(z, name);
     SET_SUBR_CODE(z, implementation);

     return (z);
}

/*
 * This group of functions is responsible for registering subrs
 * with the current package
 */

void register_subr(const _TCHAR * name, subr_arity_t arity, void *implementation)
{
     assert(HASHP(interp.subr_table));
     assert(name != NULL);

     if (implementation == NULL)
          dscwritef(";;;; NULL SUBR IMPLEMENTATION: \"~cs\"!\n", name);

     LRef subr_name = strcons(name);

     LRef subr = subrcons(arity, subr_name, implementation);

     lhash_set(interp.subr_table, subr_name, subr);
}

LRef find_subr_by_name(LRef subr_name)
{
     assert(STRINGP(subr_name));
     assert(HASHP(interp.subr_table)); /*  REVISIT: Lisp-visible: rebind *subr-table* and invoke the fasl loader */

     LRef argv[2];
     argv[0] = interp.subr_table;
     argv[1] = subr_name;

     return lhash_ref(2, argv);
}

LRef lisubr_table()
{
     return interp.subr_table;
}

/***** closures *****/

LRef lclosurecons(LRef env, LRef code, LRef property_list)
{
     LRef z = new_cell(TC_CLOSURE);

     if (!(CONSP(code) || NULLP(code)))
          vmerror_wrong_type(2, code);

     SET_CLOSURE_ENV(z, env);
     SET_CLOSURE_CODE(z, code);
     SET_CLOSURE_PROPERTY_LIST(z, property_list);

     return z;
}

LRef lset_closure_code(LRef exp, LRef code)
{
     if (!CLOSUREP(exp))
          vmerror_wrong_type(exp);

     SET_CLOSURE_CODE(exp, code);

     return exp;
}

LRef lclosure_code(LRef exp)
{
     if (!CLOSUREP(exp))
          return boolcons(false);
     else
          return (CLOSURE_CODE(exp));
}

LRef lset_closure_env(LRef exp, LRef env)
{
     if (!CLOSUREP(exp))
          vmerror_wrong_type(exp);

     SET_CLOSURE_ENV(exp, env);

     return exp;
}

LRef lclosure_env(LRef exp)
{
     if (!CLOSUREP(exp))
          return boolcons(false);
     else
          return (CLOSURE_ENV(exp));
}

LRef lset_property_list(LRef exp, LRef property_list)
{
     if (CLOSUREP(exp))
          SET_CLOSURE_PROPERTY_LIST(exp, property_list);
     else if (SYMBOLP(exp))
          SET_SYMBOL_PROPS(exp, property_list);
     else
     {
          vmerror_wrong_type(1, exp);
          return NIL;           /*  unreached. */
     }

     return property_list;
}

LRef lproperty_list(LRef exp)
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

LRef lprimitivep(LRef obj)
{
     if (SUBRP(obj))
          return obj;
     else
          return boolcons(false);
}

LRef lclosurep(LRef obj)
{
     if (CLOSUREP(obj))
          return obj;
     else
          return boolcons(false);
}

LRef lprocedurep(LRef exp)
{
     if (PROCEDUREP(exp))
          return exp;
     else
          return boolcons(false);
}


/***** Control Fields *****/

static size_t get_control_field_id(LRef control_field_id)
{
     if (!FIXNUMP(control_field_id))
          vmerror_wrong_type(1, control_field_id);

     size_t id = (size_t)FIXNM(control_field_id);

     if ((id < 0) || (id > VMCTRL_LAST))
          vmerror_arg_out_of_range(control_field_id, _T("[0,VMCTRL_LAST"));

     return id;
}

LRef liset_control_field(LRef control_field_id, LRef new_value)
{
     interp.control_fields[get_control_field_id(control_field_id)] = new_value;

     return new_value;
}

LRef licontrol_field(LRef control_field_id)
{
     return interp.control_fields[get_control_field_id(control_field_id)];
}

/***** Values tuples *****/

LRef lvalues(LRef values)
{
     LRef z = new_cell(TC_VALUES_TUPLE);

     SET_VALUES_TUPLE_VALUES(z, values);

     return z;
}

LRef valuesn(long n, ...)
{
     va_list args;

     va_start(args, n);

     LRef result = lvalues(listv(n, args));

     va_end(args);

     return result;
}

LRef lvalues2list(LRef obj)
{
     if (VALUES_TUPLE_P(obj))
          return VALUES_TUPLE_VALUES(obj);

     return lcons(obj, NIL);
}

/***** Fast-Ops *****/

LRef fast_op(int opcode, LRef arg1, LRef arg2, LRef arg3)
{
     LRef z = new_cell(TC_FAST_OP);

     SET_FAST_OP_OPCODE(z, opcode);
     SET_FAST_OP_ARG1(z, arg1);
     SET_FAST_OP_ARG2(z, arg2);
     SET_FAST_OP_ARG3(z, arg3);

     return (z);
}

LRef lfast_op(LRef opcode, LRef arg1, LRef arg2, LRef arg3)
{
     if (!FIXNUMP(opcode))
          vmerror_wrong_type(1, opcode);

     return fast_op(FIXNM(opcode), arg1, arg2, arg3);
}

LRef lfast_op_opcode(LRef fastop)
{
     if (!FAST_OP_P(fastop))
          vmerror_wrong_type(1, fastop);

     return fixcons(FAST_OP_OPCODE(fastop));
}

LRef lfast_op_args(LRef fastop)
{
     if (!FAST_OP_P(fastop))
          vmerror_wrong_type(1, fastop);

     return listn(3, FAST_OP_ARG1(fastop), FAST_OP_ARG2(fastop), FAST_OP_ARG3(fastop));
}

bool fast_op_equal(LRef a, LRef b)
{
     assert(FAST_OP_P(a));
     assert(FAST_OP_P(b));

     if (FAST_OP_OPCODE(a) != FAST_OP_OPCODE(b))
          return false;

     if (FAST_OP_ARG1(a) != FAST_OP_ARG1(b))
          return false;

     if (FAST_OP_ARG2(a) != FAST_OP_ARG2(b))
          return false;

     if (FAST_OP_ARG3(a) != FAST_OP_ARG3(b))
          return false;

     return true;
}


END_NAMESPACE
