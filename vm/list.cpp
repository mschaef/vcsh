
/* list.cpp
 *
 * SIOD list support
 */


#include "scan.h"

BEGIN_NAMESPACE(scan)
LRef lcons(LRef x, LRef y)
{
     LRef z = new_cell(TC_CONS);

     SET_CAR(z, x);
     SET_CDR(z, y);

     return z;
}

LRef lconsp(LRef x)
{
     if (CONSP(x))
          return x;
     else
          return boolcons(false);
}


LRef lcar(LRef x)
{
     if (NULLP(x))
          return NIL;

     if (!CONSP(x))
          vmerror_wrong_type(1, x);

     return CAR(x);
};

LRef lcdr(LRef x)
{
     if (NULLP(x))
          return NIL;

     if (!CONSP(x))
          vmerror_wrong_type(1, x);

     return CDR(x);
};


LRef lsetcar(LRef cell, LRef value)
{
     if (!CONSP(cell))
          vmerror_wrong_type(1, cell);

     SET_CAR(cell, value);
     return value;
}

LRef lsetcdr(LRef cell, LRef value)
{
     if (!CONSP(cell))
          vmerror_wrong_type(1, cell);

     SET_CDR(cell, value);
     return value;
}


static size_t list_length(LRef xs)      /*  REVISIT: extend to tolerate circularity? */
{
     size_t len = 0;

     while (CONSP(xs))
     {
          xs = CDR(xs);
          len++;
     }

     return len;
}

size_t object_length(LRef obj)  /*  REVISIT: Is it really necessary to be this generic in C? */
{
     switch (TYPE(obj))
     {
     case TC_NIL:
          return 0;
     case TC_CONS:
          return list_length(obj);
     case TC_STRING:
          return STRING_DIM(obj);
     case TC_VECTOR:
          return VECTOR_DIM(obj);
     case TC_GENV:
          return GENV_DIM(obj);
     case TC_HASH:
          return hash_length(obj);
     case TC_PORT:
          return port_length(obj);
     default:
          return 0;
     }
}

LRef llength(LRef obj)
{
     return fixcons(object_length(obj));
}

LRef listn(long n, ...)
{
     va_list args;

     va_start(args, n);

     LRef result = listv(n, args);

     va_end(args);

     return result;
}

LRef listv(long n, va_list args)
{
     LRef result, obj;
     long jj;

     for (jj = 0, result = NIL; jj < n; ++jj)
          result = lcons(NIL, result);

     for (jj = 0, obj = result; jj < n; obj = lcdr(obj), ++jj)
          lsetcar(obj, va_arg(args, LRef));

     return result;
}

LRef lista(size_t n, LRef args[])
{
     LRef result, obj;
     size_t jj;

     for (jj = 0, result = NIL; jj < n; ++jj)
          result = lcons(NIL, result);

     for (jj = 0, obj = result; jj < n; obj = lcdr(obj), ++jj)
          lsetcar(obj, args[jj]);

     return result;
}

END_NAMESPACE
