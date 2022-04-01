/*
 * vector.c --
 *
 * Homogeneous vectors of Lisp objects.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

lref_t vectorcons(fixnum_t dim, lref_t initial)
{
     lref_t vec = new_cell(TC_VECTOR);

     vec->as.vector.dim = dim;
     vec->as.vector.data = (lref_t *)gc_malloc((size_t)dim * sizeof(lref_t));

     for (fixnum_t ii = 0; ii < dim; ii++)
          vec->as.vector.data[ii] = initial;

     return vec;
}


bool vector_equal(lref_t veca, lref_t vecb)
{
     assert(VECTORP(veca));
     assert(VECTORP(vecb));

     size_t dim = veca->as.vector.dim;

     if (dim != vecb->as.vector.dim)
          return FALSE;

     for (size_t ii = 0; ii < dim; ii++)
     {
          if (!equalp(veca->as.vector.data[ii],
                      vecb->as.vector.data[ii]))
               return FALSE;
     }

     return TRUE;
}

lref_t lmake_vector(lref_t dim, lref_t initial)
{
     if (!NUMBERP(dim))
          vmerror_wrong_type_n(1, dim);

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

     lref_t result = vectorcons(argc, NIL);

     for (size_t ii = 0; ii < argc; ii++)
          result->as.vector.data[ii] = argv[ii];

     return result;
}

lref_t lvector_ref(lref_t vec, lref_t i, lref_t default_value)
{
     if (!VECTORP(vec))
          vmerror_wrong_type_n(1, vec);

     size_t index = 0;

     if (NUMBERP(i))
          index = get_c_fixnum(i);
     else if (CHARP(i))
          index = (fixnum_t) (CHARV(i));
     else
          vmerror_wrong_type_n(2, i);

     if (index < vec->as.vector.dim)
          return vec->as.vector.data[index];

     if (NULLP(default_value))
          vmerror_index_out_of_bounds(i, vec);

     return default_value;
}


lref_t lvector_set(lref_t vec, lref_t i, lref_t v)
{
     if (!VECTORP(vec))
          vmerror_wrong_type_n(1, vec);

     size_t index = 0;

     if (NUMBERP(i))
          index = get_c_fixnum(i);
     else if (CHARP(i))
          index = (fixnum_t) (CHARV(i));
     else
          vmerror_wrong_type_n(2, i);

     if (index < vec->as.vector.dim)
     {
          vec->as.vector.data[index] = v;
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
          vmerror_wrong_type_n(1, vec);

     for (fixnum_t jj = 0; jj < vec->as.vector.dim; jj++)
     {
          lref_t cell = lcons(vec->as.vector.data[jj], NIL);

          if (NULLP(list)) {
               list = cell;
               tail = list;
          } else {
               SET_CDR(tail, cell);
               tail = cell;
          }
     }

     return list;
}

lref_t llist2vector(lref_t xs)
{
     fixnum_t length = object_length(xs);

     lref_t result = vectorcons(length, NIL);

     lref_t l = xs;

     for (fixnum_t ii = 0; CONSP(l); l = lcdr(l), ii++)
     {
          assert(ii < length);

          result->as.vector.data[ii] = lcar(l);
     }

     if (!NULLP(l))
          vmerror_wrong_type_n(1, xs);

     return result;
}

lref_t lvector_fill(lref_t vec, lref_t v)
{
     if (!VECTORP(vec))
          vmerror_wrong_type_n(1, vec);

     for (size_t ii = 0; ii < vec->as.vector.dim; ii++)
          vec->as.vector.data[ii] = v;

     return vec;
}


lref_t lvector_copy(lref_t vec)
{
     if (!VECTORP(vec))
          vmerror_wrong_type_n(1, vec);

     lref_t result = vectorcons(vec->as.vector.dim, NIL);

     for (size_t ii = 0; ii < vec->as.vector.dim; ii++)
          result->as.vector.data[ii] = vec->as.vector.data[ii];

     return result;
}

lref_t vector_resize(lref_t vec, size_t new_size, lref_t new_element)
{
     assert(VECTORP(vec));

     lref_t result = vectorcons(new_size, NIL);

     for (size_t ii = 0; ii < new_size; ii++)
     {
          lref_t elem_value;

          if (ii < vec->as.vector.dim)
               elem_value = vec->as.vector.data[ii];
          else
               elem_value = new_element;

          result->as.vector.data[ii] = elem_value;
     }

     return result;
}
lref_t lvector_resize(lref_t vec, lref_t ns, lref_t new_element)
{
     if (!VECTORP(vec))
          vmerror_wrong_type_n(1, vec);
     if (!FIXNUMP(ns))
          vmerror_wrong_type_n(2, ns);

     fixnum_t new_size = get_c_fixnum(ns);

     if ((new_size < 0) || ((size_t)new_size > SIZE_MAX))
          vmerror_arg_out_of_range(ns, _T("[0,SIZE_MAX]"));

     return vector_resize(vec, (size_t)new_size, new_element);
}

