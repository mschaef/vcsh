
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

LRef boolcons(bool val)
{
     return LREF2_CONS(LREF2_BOOL, val ? 1 : 0);
}

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

LRef lrepresentation_of(LRef obj)
{
     if (COMPLEXP(obj))
          return simple_intern(_T("complex"), interp.scheme_package);

     if (INSTANCEP(obj))
          return simple_intern(_T("instance"), interp.scheme_package);

     return make_type_name(TYPE(obj));
}

LRef litypecode(LRef obj)
{
     return fixcons(TYPE(obj));
}

END_NAMESPACE
