
/* byte-vector.cpp
 *
 * The implementation of the byte vector type
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

LRef byteveccons(size_t dim)
{
     LRef z = new_cell(TC_BYTE_VECTOR);

     SET_BYTE_VECTOR_DIM(z, dim);
     SET_BYTE_VECTOR_DATA(z, (u8_t *) safe_malloc(dim));

     return z;
}

LRef lbyte_vector_p(LRef x)
{
     if (BYTE_VECTOR_P(x))
          return x;
     else
          return boolcons(false);
}

LRef lvector2byte_vector(LRef vec)
{

     if (!VECTORP(vec))
          vmerror_wrong_type(1, vec);

     size_t dim = VECTOR_DIM(vec);

     LRef bytevec = byteveccons(dim);

     for (size_t ii = 0; ii < dim; ii++)
     {
          if (!FIXNUMP(VECTOR_ELEM(vec, ii)))
               vmerror("non integer to vector->byte-vector", vec);

          fixnum_t x = get_c_fixnum(VECTOR_ELEM(vec, ii));

          if ((x < 0) || (x > 255))
               vmerror("integer out of range [0..255] to vector->byte-vetor", vec);

          BYTE_VECTOR_DATA(bytevec)[ii] = (u8_t) x;
     }

     return bytevec;
}

LRef lbyte_vector2vector(LRef bytevec)
{
     if (!BYTE_VECTOR_P(bytevec))
          vmerror_wrong_type(1, bytevec);

     size_t dim = BYTE_VECTOR_DIM(bytevec);

     LRef vec = vectorcons(dim, NIL);

     for (size_t ii = 0; ii < dim; ii++)
          SET_VECTOR_ELEM(vec, ii, fixcons(BYTE_VECTOR_DATA(bytevec)[ii]));

     return vec;
}

END_NAMESPACE
