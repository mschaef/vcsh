
/*
 * vector.cpp --
 *
 * Homogeneous vectors of Lisp objects.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)
lref_t vectorcons(fixnum_t n, lref_t initial)
{
     lref_t vec = new_cell(TC_VECTOR);

     SET_VECTOR_DIM(vec, (size_t) n);
     SET_VECTOR_DATA(vec, (lref_t *) safe_malloc(((size_t) n) * sizeof(lref_t)));

     for (fixnum_t ii = 0; ii < n; ii++)
          SET_VECTOR_ELEM(vec, ii, initial);

     return vec;
}


bool vector_equal(lref_t veca, lref_t vecb)
{
     assert(VECTORP(veca));
     assert(VECTORP(vecb));

     size_t len = VECTOR_DIM(veca);

     if (len != VECTOR_DIM(vecb))
          return FALSE;

     for (size_t ii = 0; ii < len; ii++)
     {
          if (!equalp(VECTOR_ELEM(veca, ii), VECTOR_ELEM(vecb, ii)))
               return FALSE;
     }

     return TRUE;
}

lref_t lmake_vector(lref_t dim, lref_t initial)
{
     if (!NUMBERP(dim))
          vmerror_wrong_type(1, dim);

     fixnum_t d = get_c_fixnum(dim);

     if ((d < 0) || ((size_t)d > SIZE_MAX))
          vmerror_arg_out_of_range(dim, _T("[0,SIZE_MAX]"));

     return vectorcons(d, initial);
}

lref_t lvectorp(lref_t obj)
{
     if (VECTORP(obj))
          return obj;
     else
          return boolcons(false);
}

lref_t lvector(size_t argc, lref_t argv[])
{
     assert(argc >= 0);

     lref_t result = vectorcons(argc);

     for (size_t ii = 0; ii < argc; ii++)
          SET_VECTOR_ELEM(result, ii, argv[ii]);

     return result;
}



lref_t lvector_ref(lref_t vec, lref_t i, lref_t default_value)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);
     
     size_t index = 0;

     if (NUMBERP(i))
          index = get_c_fixnum(i);
     else if (CHARP(i))
          index = (fixnum_t) (CHARV(i));
     else
          vmerror_wrong_type(2, i);

     if ((index >= 0) && (index < VECTOR_DIM(vec)))
          return VECTOR_ELEM(vec, index);

     if (NULLP(default_value))
          vmerror_index_out_of_bounds(i, vec);

     return default_value;
}


lref_t lvector_set(lref_t vec, lref_t i, lref_t v)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     size_t index = 0;

     if (NUMBERP(i))
          index = get_c_fixnum(i);
     else if (CHARP(i))
          index = (fixnum_t) (CHARV(i));
     else
          vmerror_wrong_type(2, i);

     if ((index >= 0) && (index < VECTOR_DIM(vec)))
     {
          SET_VECTOR_ELEM(vec, index, v);
          return vec;
     }

     vmerror_index_out_of_bounds(i, vec);

     return NIL; // unreached
}

lref_t lvector2list(lref_t vec)
{
     lref_t list = NIL;
     lref_t tail = NIL;

     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     fixnum_t length = object_length(vec);

     for (fixnum_t jj = 0; jj < length; jj++)
     {
          lref_t cell = lcons(VECTOR_ELEM(vec, jj), NIL);

          if (NULLP(list))
          {
               list = cell;
               tail = list;
          }
          else
          {
               SET_CDR(tail, cell);
               tail = cell;
          }
     }

     return list;
}

lref_t llist2vector(lref_t xs)
{
     fixnum_t length = object_length(xs);

     lref_t result = vectorcons(length);

     lref_t l = xs;

     for (fixnum_t ii = 0; CONSP(l); l = lcdr(l), ii++)
     {
          assert(ii < length);

          SET_VECTOR_ELEM(result, ii, lcar(l));
     }

     if (!NULLP(l))
          vmerror_wrong_type(1, xs);

     return result;
}

lref_t lvector_fill(lref_t vec, lref_t v)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     for (size_t ii = 0; ii < VECTOR_DIM(vec); ii++)
          SET_VECTOR_ELEM(vec, ii, v);

     return vec;
}


lref_t lvector_copy(lref_t vec)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     lref_t result = vectorcons(VECTOR_DIM(vec));

     for (size_t ii = 0; ii < VECTOR_DIM(vec); ii++)
          SET_VECTOR_ELEM(result, ii, VECTOR_ELEM(vec, ii));

     return result;
}

lref_t vector_resize(lref_t vec, size_t new_size, lref_t new_element)
{
     assert(VECTORP(vec));

     lref_t result = vectorcons(new_size);

     for (size_t ii = 0; ii < new_size; ii++)
     {
          if (ii < VECTOR_DIM(vec))
               SET_VECTOR_ELEM(result, ii, VECTOR_ELEM(vec, ii));
          else
               SET_VECTOR_ELEM(result, ii, new_element);
     }

     return result;
}
lref_t lvector_resize(lref_t vec, lref_t ns, lref_t new_element)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);
     if (!FIXNUMP(ns))
          vmerror_wrong_type(2, ns);

     fixnum_t new_size = get_c_fixnum(ns);

     if ((new_size < 0) || ((size_t)new_size > SIZE_MAX))
          vmerror_arg_out_of_range(ns, _T("[0,SIZE_MAX]"));

     return vector_resize(vec, (size_t)new_size, new_element);
}

END_NAMESPACE
