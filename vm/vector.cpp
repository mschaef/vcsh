
/* vector.cpp
 *
 * The implementation of the vector type
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)
LRef vectorcons(fixnum_t n, LRef initial)
{
     LRef vec = new_cell(TC_VECTOR);

     SET_VECTOR_DIM(vec, (size_t) n);
     SET_VECTOR_DATA(vec, (LRef *) safe_malloc(((size_t) n) * sizeof(LRef)));

     for (fixnum_t ii = 0; ii < n; ii++)
          SET_VECTOR_ELEM(vec, ii, initial);

     return vec;
}


bool vector_equal(LRef veca, LRef vecb)
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

LRef lmake_vector(LRef dim, LRef initial)
{
     if (!NUMBERP(dim))
          vmerror_wrong_type(1, dim);

     fixnum_t d = get_c_fixnum(dim);

     if ((d < 0) || ((size_t)d > SIZE_MAX))
          vmerror_arg_out_of_range(dim, _T("[0,SIZE_MAX]"));

     return vectorcons(d, initial);
}

LRef lvectorp(LRef obj)
{
     if (VECTORP(obj))
          return obj;
     else
          return boolcons(false);
}

LRef lvector(size_t argc, LRef argv[])
{
     assert(argc >= 0);

     LRef result = vectorcons(argc);

     for (size_t ii = 0; ii < argc; ii++)
          SET_VECTOR_ELEM(result, ii, argv[ii]);

     return result;
}



LRef lvector_ref(LRef vec, LRef i, LRef default_value)
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


LRef lvector_set(LRef vec, LRef i, LRef v)
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

LRef lvector2list(LRef vec)
{
     LRef list = NIL;
     LRef tail = NIL;

     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     fixnum_t length = object_length(vec);

     for (fixnum_t jj = 0; jj < length; jj++)
     {
          LRef cell = lcons(VECTOR_ELEM(vec, jj), NIL);

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

LRef llist2vector(LRef xs)
{
     fixnum_t length = object_length(xs);

     LRef result = vectorcons(length);

     LRef l = xs;

     for (fixnum_t ii = 0; CONSP(l); l = lcdr(l), ii++)
     {
          assert(ii < length);

          SET_VECTOR_ELEM(result, ii, lcar(l));
     }

     if (!NULLP(l))
          vmerror_wrong_type(1, xs);

     return result;
}

LRef lvector_fill(LRef vec, LRef v)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     for (size_t ii = 0; ii < VECTOR_DIM(vec); ii++)
          SET_VECTOR_ELEM(vec, ii, v);

     return vec;
}


LRef lvector_copy(LRef vec)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     LRef result = vectorcons(VECTOR_DIM(vec));

     for (size_t ii = 0; ii < VECTOR_DIM(vec); ii++)
          SET_VECTOR_ELEM(result, ii, VECTOR_ELEM(vec, ii));

     return result;
}

LRef vector_resize(LRef vec, size_t new_size, LRef new_element)
{
     assert(VECTORP(vec));

     LRef result = vectorcons(new_size);

     for (size_t ii = 0; ii < new_size; ii++)
     {
          if (ii < VECTOR_DIM(vec))
               SET_VECTOR_ELEM(result, ii, VECTOR_ELEM(vec, ii));
          else
               SET_VECTOR_ELEM(result, ii, new_element);
     }

     return result;
}

LRef vector_reallocate_in_place(LRef vec, size_t new_size, LRef new_element)
{
     assert(VECTORP(vec));

     LRef *new_vector_data = (LRef *) safe_malloc(new_size * sizeof(LRef));

     assert(new_vector_data);

     for (size_t ii = 0; ii < new_size; ii++)
     {
          if (ii < VECTOR_DIM(vec))
               new_vector_data[ii] = VECTOR_ELEM(vec, ii);
          else
               new_vector_data[ii] = new_element;
     }

     safe_free(VECTOR_DATA(vec));

     SET_VECTOR_DIM(vec, new_size);
     SET_VECTOR_DATA(vec, new_vector_data);

     return vec;
}

LRef lvector_resize(LRef vec, LRef ns, LRef new_element)
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

LRef lvector_resized(LRef vec, LRef ns, LRef new_element)
{
     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);
     if (!FIXNUMP(ns))
          vmerror_wrong_type(2, ns);

     fixnum_t new_size = get_c_fixnum(ns);

     if ((new_size < 0) || ((size_t)new_size > SIZE_MAX))
          vmerror_arg_out_of_range(ns, _T("[0,SIZE_MAX]"));

     return vector_reallocate_in_place(vec, (size_t)new_size, new_element);
}


END_NAMESPACE
