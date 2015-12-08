/*
 * number.c --
 *
 * Primitive support for fixnum, flonum, and complex math.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <float.h>

#include "scan-private.h"
#include "mt19937.h"

/*** Number constructors ***/

lref_t fixcons(fixnum_t x)
{
     if ((x <= FIXNUM_MAX) && (x >= FIXNUM_MIN))
          return MAKE_LREF1(LREF1_FIXNUM, (intptr_t) x);

     unsigned_fixnum_t ux = (unsigned_fixnum_t)x;

     return vmtrap(TRAP_OVERFLOW_FIXNUM_FIXCONS, VMT_MANDATORY_TRAP,
                   2,
                   fixcons(ux >> (FIXNUM_BITS - LREF1_TAG_SHIFT)),
                   fixcons(ux >> LREF1_TAG_SHIFT));
}

lref_t flocons(flonum_t re)
{
     lref_t val = new_cell(TC_FLONUM);

     val->as.flonum.data = re;
     val->as.flonum.im_part = NIL;

     return val;
}

lref_t cmplxcons(flonum_t re, flonum_t im)
{
     lref_t val = new_cell(TC_FLONUM);

     val->as.flonum.data = re;
     val->as.flonum.im_part = flocons(im);

     return val;
}

/* Number accessors *******************************************/

fixnum_t get_c_fixnum(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FIXNUMP(x))
          return FIXNM(x);
     else
          return (fixnum_t)FLONM(x);
}

long get_c_long(lref_t x)
{
     return (long)get_c_fixnum(x);
}

flonum_t get_c_flonum(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FLONUMP(x))
          return FLONM(x);
     else
          return (double)FIXNM(x);
}

bool get_c_complex(lref_t x, flonum_t *re, flonum_t *im)
{
     if (FIXNUMP(x)) {
          *re = (double)FIXNM(x);
          *im = 0.0;
          return false;
     }

     if (FLONUMP(x)) {
          *re = FLONM(x);

          if (COMPLEXP(x)) {
               *im = CMPLXIM(x);
               return true;
          } else {
               *im = 0.0;
               return false;
          }
     }

     vmerror_wrong_type(x);
     return false;
}

/* Number predicates ******************************************
 *
 * The only unique types we implement are fixnum (integer, exact)
 * and flonum (real, inexact).  */

lref_t lnumberp(lref_t x)
{
     if (NUMBERP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lcomplexp(lref_t x)
{
     if (COMPLEXP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lrealp(lref_t x)
{
     if ((FIXNUMP(x) || REALP(x)))
          return x;
     else
          return boolcons(false);
}

lref_t lrationalp(lref_t x)
{
     if (FIXNUMP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lintegerp(lref_t x)
{
     if (FIXNUMP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lexactp(lref_t x)
{
     if (FIXNUMP(x))
          return x;
     else
          return boolcons(false);
}

lref_t linexactp(lref_t x)
{
     if (FLONUMP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lnanp(lref_t x)
{
     if (FLONUMP(x))
     {
          if (COMPLEXP(x))
               return (isnan(FLONM(x)) || isnan(FLONM(FLOIM(x)))) ? x : boolcons(false);
          else
               return isnan(FLONM(x)) ? x : boolcons(false);
     }

     return boolcons(false);
}

lref_t linfinitep(lref_t x)
{
     if (FLONUMP(x))
     {
          if (COMPLEXP(x))
               return (!isfinite(FLONM(x)) || !isfinite(FLONM(FLOIM(x)))) ? x : boolcons(false);
          else
               return !isfinite(FLONM(x)) ? x : boolcons(false);
     }

     return boolcons(false);
}


/* Exactness conversion ***************************************/

lref_t lexact2inexact(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     return FIXNUMP(x) ? flocons((flonum_t) FIXNM(x)) : x;
}

lref_t linexact2exact(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return x;
     else if ((FLONM(x) >= FIXNUM_MIN) && (FLONM(x) <= FIXNUM_MAX))
          return FLONUMP(x) ? fixcons((fixnum_t) FLONM(x)) : x;
     else
          return boolcons(false);
}


/* Comparisons ************************************************/

static enum typecode_t common_number_type(size_t argc, lref_t argv[])
{
     enum typecode_t type = TC_NIL;
     
     for (size_t ii = 0; ii < argc; ii++)
     {
          lref_t arg = argv[ii];
          
          if (CHARP(arg)) {
               if (type == TC_NIL)
                    type = TC_CHARACTER;
               
          } else if (FIXNUMP(arg)) {
               if ((type == TC_NIL) || (type == TC_CHARACTER))
                    type = TC_FIXNUM;
               
          } else if (FLONUMP(arg)) {
               if ((type == TC_NIL) || (type == TC_CHARACTER) || (type == TC_FIXNUM))
                    type = TC_FLONUM;
               
          } else 
               vmerror_wrong_type_n(ii, arg);
     }
     
     return type;
}

#define MAKE_NUMBER_COMPARISON_FN(fn_name, op, op_string)                           \
lref_t fn_name(size_t argc, lref_t argv[])                                          \
{                                                                                   \
    double flo_prev;                                                                \
    fixnum_t fix_prev;                                                              \
    _TCHAR char_prev;                                                               \
                                                                                    \
    switch (common_number_type(argc, argv))                                         \
    {                                                                               \
    case TC_NIL:                                                                    \
       break;                                                                       \
                                                                                    \
    case TC_CHARACTER:                                                              \
      char_prev = CHARV(argv[0]);                                                   \
                                                                                    \
      for (size_t ii = 1; ii < argc; ii++)                                          \
        {                                                                           \
         if(!(char_prev op CHARV(argv[ii])))                                        \
            return boolcons(false);                                                 \
                                                                                    \
          char_prev = CHARV(argv[ii]);                                              \
        }                                                                           \
      break;                                                                        \
                                                                                    \
    case TC_FIXNUM:                                                                 \
      fix_prev = FIXNM(argv[0]);                                                    \
                                                                                    \
      for (size_t ii = 1; ii < argc; ii++)                                          \
        {                                                                           \
         if (!(fix_prev op FIXNM(argv[ii])))                                        \
              return boolcons(false);                                               \
                                                                                    \
          fix_prev = FIXNM(argv[ii]);                                               \
        }                                                                           \
      break;                                                                        \
                                                                                    \
    case TC_FLONUM:                                                                 \
      flo_prev = get_c_flonum(argv[0]);                                             \
                                                                                    \
      for (size_t ii = 1; ii < argc; ii++)                                          \
        {                                                                           \
          if (!(flo_prev op get_c_flonum(argv[ii])))                                \
             return boolcons(false);                                                \
                                                                                    \
          flo_prev = get_c_flonum(argv[ii]);                                        \
        }                                                                           \
      break;                                                                        \
                                                                                    \
    default:                                                                        \
       panic(_T("Unknown numeric type in number comparison function"));             \
    }                                                                               \
                                                                                    \
    return boolcons(true);                                                          \
}

MAKE_NUMBER_COMPARISON_FN(lnum_eq, ==, "=");
MAKE_NUMBER_COMPARISON_FN(lnum_ge, >=, ">=");
MAKE_NUMBER_COMPARISON_FN(lnum_gt, >, ">");
MAKE_NUMBER_COMPARISON_FN(lnum_le, <=, "<=");
MAKE_NUMBER_COMPARISON_FN(lnum_lt, <, "<");

/* The basic four operations **********************************/

lref_t ladd(lref_t x, lref_t y)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y))
          return x;

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     if (FLONUMP(x) || FLONUMP(y)) {
          flonum_t xre, xim;
          flonum_t yre, yim;

          bool x_complex = get_c_complex(x, &xre, &xim);
          bool y_complex = get_c_complex(y, &yre, &yim);

          if (x_complex || y_complex) {
               return cmplxcons(xre + yre, xim + yim);
          } else {
               return flocons(xre + yre);
          }
     }

     fixnum_t xf = FIXNM(x);
     fixnum_t yf = FIXNM(y);

     if (((yf > 0) && (xf > (FIXNUM_MAX-yf))) || ((yf < 0) && (xf < (FIXNUM_MIN-yf))))
          return vmtrap(TRAP_OVERFLOW_FIXNUM_ADD, VMT_MANDATORY_TRAP, 2, x, y);

     return fixcons(xf + yf);
}

lref_t lsubtract(lref_t x, lref_t y)
{

     flonum_t xre, xim;
     flonum_t yre, yim;
     bool x_complex, y_complex;

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y)) {
          if (FIXNUMP(x)) {
               fixnum_t xf = FIXNM(x);

               if (xf == FIXNUM_MIN)
                    return vmtrap(TRAP_OVERFLOW_FIXNUM_NEGATE, VMT_MANDATORY_TRAP, 1, x);

               return fixcons(-xf);
          } else {
               assert(FLONUMP(x));

               x_complex = get_c_complex(x, &xre, &xim);

               if (x_complex)
                    return cmplxcons(-xre, -xim);
               else
                    return flocons(-xre);
          }
     }

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     if (FLONUMP(x) || FLONUMP(y)) {
          x_complex = get_c_complex(x, &xre, &xim);
          y_complex = get_c_complex(y, &yre, &yim);

          if (x_complex || y_complex)
               cmplxcons(xre - yre, xim - yim);
          else
               return flocons(xre - yre);
     }

     fixnum_t xf = FIXNM(x);
     fixnum_t yf = FIXNM(y);

     if ((yf > 0 && xf < FIXNUM_MIN + yf) || (yf < 0 && xf > FIXNUM_MAX + yf))
          return vmtrap(TRAP_OVERFLOW_FIXNUM_SUBTRACT, VMT_MANDATORY_TRAP, 2, x, y);

     return fixcons(xf - yf);
}

lref_t lmultiply(lref_t x, lref_t y)
{

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y))
          return x;

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     if (FLONUMP(x) || FLONUMP(y)) {
          flonum_t xre, xim;
          flonum_t yre, yim;
          bool x_complex, y_complex;

          x_complex = get_c_complex(x, &xre, &xim);
          y_complex = get_c_complex(y, &yre, &yim);

          if (x_complex || y_complex)
               return cmplxcons(xre * yre - xim * yim, xre * yim + xim * yre);
          else
               return flocons(xre * yre);
     }

     fixnum_t xf = FIXNM(x);
     fixnum_t yf = FIXNM(y);

     if (xf > 0) {
          if (yf > 0) {
               if (xf > (FIXNUM_MAX / yf)) {
                    return vmtrap(TRAP_OVERFLOW_FIXNUM_MULTIPLY, VMT_MANDATORY_TRAP, 2, x, y);
               }
          } else {
               if (yf < (FIXNUM_MIN / xf)) {
                    return vmtrap(TRAP_OVERFLOW_FIXNUM_MULTIPLY, VMT_MANDATORY_TRAP, 2, x, y);
               }
          }
     } else {
          if (yf > 0) {
               if (xf < (FIXNUM_MIN / yf)) {
                    return vmtrap(TRAP_OVERFLOW_FIXNUM_MULTIPLY, VMT_MANDATORY_TRAP, 2, x, y);
               }
          } else {
               if ( (xf != 0) && (yf < (FIXNUM_MAX / xf))) {
                    return vmtrap(TRAP_OVERFLOW_FIXNUM_MULTIPLY, VMT_MANDATORY_TRAP, 2, x, y);
               }
          }
     }

     return fixcons(xf * yf);
}


lref_t ldivide(lref_t x, lref_t y)
{

     flonum_t xre, xim;
     flonum_t yre, yim;
     bool x_complex, y_complex;

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y)) {
          x_complex = get_c_complex(x, &xre, &xim);

          if (x_complex) {
               flonum_t d = xre * xre + xim * xim;

               return cmplxcons(xre / d, -xim / d);
          } else {
               return flocons(1 / xre);
          }
     }

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     x_complex = get_c_complex(x, &xre, &xim);
     y_complex = get_c_complex(y, &yre, &yim);

     if (x_complex || y_complex) {
          flonum_t d = yre * yre + yim * yim;

          return cmplxcons((xre * yre + xim * yim) / d, (xim * yre - xre * yim) / d);
     } else {
          return flocons(xre / yre);
     }
}

/* Number-theoretic division **********************************/

static flonum_t truncate(flonum_t x)
{
     return (x < 0) ? ceil(x) : floor(x);
}

lref_t lquotient(lref_t x, lref_t y)
{
     if (!REALP(x))
          vmerror_wrong_type_n(1, x);

     if (!REALP(y))
          vmerror_wrong_type_n(2, y);

     if (FIXNUMP(x) && FIXNUMP(y))
     {
          fixnum_t xf = FIXNM(x);
          fixnum_t yf = FIXNM(y);

          if (yf == 0)
               vmerror_divide_by_zero();

          if ((xf == FIXNUM_MIN) && (yf == -1))
               return vmtrap(TRAP_OVERFLOW_FIXNUM_DIVIDE, VMT_MANDATORY_TRAP, 2, x, y);

          return fixcons(xf / yf);
     }

     flonum_t yf = get_c_flonum(y);

     if (yf == 0.0)
          vmerror_divide_by_zero();

     return flocons(truncate(get_c_flonum(x) / yf));
}

lref_t lremainder(lref_t x, lref_t y)
{
     if (!REALP(x))
          vmerror_wrong_type_n(1, x);

     if (!REALP(y))
          vmerror_wrong_type_n(2, y);

     if (FIXNUMP(x) && FIXNUMP(y))
     {
          fixnum_t xf = FIXNM(x);
          fixnum_t yf = fixabs(FIXNM(y));

          if (yf == 0)
               vmerror_divide_by_zero();

          return fixcons(xf - ((xf / yf) * yf));
     }

     flonum_t xf = get_c_flonum(x);
     flonum_t yf = get_c_flonum(y);

     if (yf == 0.0)
          vmerror_divide_by_zero();

     return flocons(xf - (truncate(xf / yf) * yf));
}

lref_t lmodulo(lref_t x, lref_t y)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type_n(2, y);

     fixnum_t yf = FIXNM(y);
     fixnum_t xf = FIXNM(x);

     if (yf == 0)
          vmerror_divide_by_zero();

     if ((xf == FIXNUM_MIN) && (yf == -1))
          return vmtrap(TRAP_OVERFLOW_FIXNUM_MODULO, VMT_MANDATORY_TRAP, 2, x, y);

     fixnum_t mod = fixabs(xf) % fixabs(yf);

     if ((yf > 0) != (xf > 0))
          mod = fixabs(yf) - mod;

     return fixcons(yf > 0 ? mod : -mod);
}


  /* Truncation and Rounding *********************************** */

lref_t lfloor(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(floor(FLONM(x)));
}

lref_t lceiling(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(ceil(FLONM(x)));
}

double round(double n)
{
     if (n > 0)
          return ((n - floor(n)) >= 0.5) ? (floor(n) + 1.0) : floor(n);
     else
          return ((ceil(n) - n) >= 0.5) ? (ceil(n) - 1.0) : ceil(n);
}

lref_t lroundnum(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(round(FLONM(x)));
}

lref_t ltruncate(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(truncate(FLONM(x)));
}

/* Bitwise operations *****************************************/

lref_t lbitwise_and(lref_t x, lref_t y)
{
     if (NULLP(y))
          return x;

     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type_n(2, x);

     return fixcons(FIXNM(x) & FIXNM(y));

}

lref_t lbitwise_or(lref_t x, lref_t y)
{
     if (NULLP(y))
          return x;

     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type_n(2, x);

     return fixcons(FIXNM(x) | FIXNM(y));
}

lref_t lbitwise_xor(lref_t x, lref_t y)
{
     if (NULLP(y))
          return x;

     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type_n(2, x);

     return fixcons(FIXNM(x) ^ FIXNM(y));
}

lref_t lbitwise_not(lref_t x)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     return fixcons(~FIXNM(x));
}

lref_t lbitwise_shl(lref_t x, lref_t n)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(n))
          vmerror_wrong_type_n(2, n);

     fixnum_t bits = FIXNM(n);

     if (bits >= FIXNUM_BITS)
          return fixcons(0);

     fixnum_t xf = FIXNM(x);

     if ((xf < 0)
         || (bits < 0)
         || (bits >= (fixnum_t)(sizeof(int)*CHAR_BIT))
         || (xf > (INT_MAX >> bits)))
          return vmtrap(TRAP_OVERFLOW_FIXNUM_SHL, VMT_MANDATORY_TRAP, 2, x, n);

     return fixcons(xf << bits);
}

lref_t lbitwise_shr(lref_t x, lref_t n)
{
     unsigned_fixnum_t ux;
     fixnum_t sx;

     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(n))
          vmerror_wrong_type_n(2, n);

     fixnum_t bits = FIXNM(n);

     if (bits >= FIXNUM_BITS)
          return fixcons(0);

     ux = (unsigned_fixnum_t) FIXNM(x);

     ux = ux >> bits;

     sx = (fixnum_t) ux;

     return fixcons(sx);
}

lref_t lbitwise_ashr(lref_t x, lref_t n)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     if (!FIXNUMP(n))
          vmerror_wrong_type_n(2, n);

     fixnum_t bits = FIXNM(n);

     if (bits >= FIXNUM_BITS)
          return fixcons(0);

     return fixcons(FIXNM(x) >> bits);
}

/* Transcenendal ***********************************************/

lref_t lexp(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x) {
          /*  e^(y+xi) =  e^y (cos x + i sin x) */
          return cmplxcons(xre * cos(xim), xre * sin(xim));
     } else {
          return flocons(exp(xre));
     }
}

lref_t llog(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if ((!complex_x || FIXNUMP(x)) && (xre >= 0.0)) {
          return flocons(log(xre));
     } else {
          return cmplxcons(log(sqrt(xre * xre + xim * xim)), atan2(xim, xre));
     }
}


lref_t lsin(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return flocons(sin(xre));
}

lref_t lcos(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return flocons(cos(xre));
}

lref_t ltan(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return flocons(tan(xre));
}

lref_t lasin(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return flocons(asin(xre));
}

lref_t lacos(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return flocons(acos(xre));
}

lref_t latan(lref_t x, lref_t y)
{
     bool complex_x;
     flonum_t xre, xim;
     bool complex_y;
     flonum_t yre, yim;

     complex_x = get_c_complex(x, &xre, &xim);
     complex_y = !NULLP(y) && get_c_complex(y, &yre, &yim);

     if (complex_x || complex_y)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     if (NULLP(y))
          return flocons(atan(xre));
     else
          return flocons(atan2(xre, yre));
}

lref_t lsqrt(lref_t x)
{
     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     if (complex_x) {
          flonum_t c = pow((xre * xre) + (xim * xim), 0.25);
          flonum_t a = 0.5 * atan2(xim, xre);

          return cmplxcons(c * cos(a), c * sin(a));
     } else if (xre < 0.0) {
          return cmplxcons(0.0, sqrt(-xre));
     } else {
          return flocons(sqrt(xre));
     }
}


lref_t lexpt(lref_t x, lref_t y) {

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     bool complex_x;
     flonum_t xre, xim;
     bool complex_y;
     flonum_t yre, yim;

     complex_x = get_c_complex(x, &xre, &xim);
     complex_y = get_c_complex(y, &yre, &yim);

     if (complex_x || complex_y)
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return flocons(pow(xre, yre));
}

/* Complex Number Accessors **********************************/

lref_t lmake_rectangular(lref_t re, lref_t im)
{
     if (!(REALP(re) || FIXNUMP(re)))
          vmerror_wrong_type_n(1, re);

     if (!(REALP(im) || FIXNUMP(im)))
          vmerror_wrong_type_n(2, im);

     return cmplxcons(get_c_flonum(re), get_c_flonum(im));
}

lref_t lmake_polar(lref_t r, lref_t theta)
{
     if (!(REALP(r) || FIXNUMP(r)))
          vmerror_wrong_type_n(1, r);

     if (!(REALP(theta) || FIXNUMP(theta)))
          vmerror_wrong_type_n(2, theta);

     flonum_t fr = get_c_flonum(r);
     flonum_t thetar = get_c_flonum(theta);

     return cmplxcons(fr * cos(thetar), fr * sin(thetar));
}

lref_t lreal_part(lref_t x)
{
     if (FIXNUMP(x))
          return x;

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     flonum_t xre, xim;

     get_c_complex(x, &xre, &xim);

     return flocons(xre);
}

lref_t limag_part(lref_t cmplx)
{
     if (FIXNUMP(cmplx))
          return fixcons(0);

     if (!NUMBERP(cmplx))
          vmerror_wrong_type_n(1, cmplx);

     flonum_t xre, xim;

     get_c_complex(cmplx, &xre, &xim);

     return flocons(xim);
}

lref_t langle(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     return flocons(atan2(xim, xre));
}

lref_t lmagnitude(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return (FIXNM(x) < 0) ? fixcons(-FIXNM(x)) : x;

     bool complex_x;
     flonum_t xre, xim;

     complex_x = get_c_complex(x, &xre, &xim);

     if (complex_x) {
          return flocons(sqrt(xre * xre + xim * xim));
     } else {
          return (FLONM(x) < 0) ? flocons(-FLONM(x)) : x;
     }
}


/* Random number generator ************************************/

lref_t lrandom(lref_t x)            /*  TESTTHIS */
{
     if (NULLP(x))
          return flocons(mt19937_real2());

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (FIXNUMP(x))
          return fixcons(mt19937_int64() % FIXNM(x));

     bool x_complex;
     flonum_t xre, xim;

     x_complex = get_c_complex(x, &xre, &xim);

     if (x_complex) {
          return cmplxcons(mt19937_real2() * xre, mt19937_real2() * xim);
     } else {
          return flocons(mt19937_real2() * xre);
     }
}


lref_t lset_random_seed(lref_t s)
{
     fixnum_t seed = 1;

     if (NULLP(s)) {
          seed = time(NULL);
     } else if (FIXNUMP(s)) {
          seed = FIXNM(s);
     } else {
          vmerror_wrong_type_n(1, s);
     }

     init_mt19937((unsigned long) seed);

     return fixcons(seed);
}

