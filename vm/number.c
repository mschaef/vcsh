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
     if ((x <= MAX_LREF_FIXNUM) && (x >= MIN_LREF_FIXNUM))
          return MAKE_LREF1(LREF1_FIXNUM, (intptr_t) x);

     lref_t retval = new_cell(TC_FIXNUM);
     *_FIXNM(retval) = x;

     return retval;
}

lref_t flocons(flonum_t x)
{
     lref_t z = new_cell(TC_FLONUM);

     SET_FLONM(z, x);
     SET_FLOIM(z, NIL);

     return (z);
}

lref_t cmplxcons(flonum_t re, flonum_t im)
{
     lref_t z = new_cell(TC_FLONUM);

     SET_FLONM(z, re);
     SET_FLOIM(z, flocons(im));

     return (z);
}

/* Number accessors *******************************************/

long get_c_long(lref_t x)
{
     return (long) get_c_fixnum(x);
}

double get_c_double(lref_t x)
{
     return (double) get_c_flonum(x);
}

fixnum_t get_c_fixnum(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FIXNUMP(x))
          return FIXNM(x);
     else
          return (fixnum_t) FLONM(x);
}

flonum_t get_c_flonum(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FLONUMP(x))
          return (FLONM(x));
     else
          return (double) (FIXNM(x));
}

flonum_t get_c_flonum_im(lref_t x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FIXNUMP(x))
          return 0.0;
     else if (COMPLEXP(x))
          return CMPLXIM(x);
     else
          return 0.0;
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
     if (NUMBERP(x))
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
               return (!finite(FLONM(x)) || !finite(FLONM(FLOIM(x)))) ? x : boolcons(false);
          else
               return !finite(FLONM(x)) ? x : boolcons(false);
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

enum NumericArgumentType
{
     INVALID,
     CHARACTER,
     EXACT,
     INEXACT
};

enum NumericArgumentType validate_numeric_arguments(size_t argc, lref_t argv[])
{
     bool exact = TRUE;
     bool character = TRUE;
     bool valid = TRUE;


     for (size_t ii = 0; ii < argc; ii++)
     {
          character = character && CHARP(argv[ii]);
          valid = valid && (CHARP(argv[ii]) || NUMBERP(argv[ii]));
          exact = exact && FIXNUMP(argv[ii]);
     }

     if (valid)
     {
          if (character)
               return CHARACTER;
          else if (exact)
               return EXACT;
          else
               return INEXACT;
     }

     return INVALID;
}

#define MAKE_NUMBER_COMPARISON_FN(fn_name, op, op_string)                           \
lref_t fn_name(size_t argc, lref_t argv[])                                          \
{                                                                                   \
    if (argc == 0)                                                                  \
      return boolcons(true);                                                        \
                                                                                    \
    bool current_result = true;                                                     \
    double flo_prev;                                                                \
    fixnum_t fix_prev;                                                              \
    _TCHAR char_prev;                                                               \
                                                                                    \
    enum NumericArgumentType type = validate_numeric_arguments(argc, argv);         \
                                                                                    \
    switch (type)                                                                   \
    {                                                                               \
    case INVALID:                                                                   \
      vmerror_wrong_type(lista(argc, argv));                                        \
      break;                                                                        \
                                                                                    \
    case CHARACTER:                                                                 \
      char_prev = CHARV(argv[0]);                                                   \
                                                                                    \
      for (size_t ii = 1; ii < argc; ii++)                                          \
        {                                                                           \
          current_result = current_result && (char_prev op CHARV(argv[ii]));        \
          char_prev = CHARV(argv[ii]);                                              \
          if (!current_result)                                                      \
            break;                                                                  \
        }                                                                           \
      break;                                                                        \
                                                                                    \
    case EXACT:                                                                     \
      fix_prev = FIXNM(argv[0]);                                                    \
                                                                                    \
      for (size_t ii = 1; ii < argc; ii++)                                          \
        {                                                                           \
          current_result = current_result && (fix_prev op FIXNM(argv[ii]));         \
          fix_prev = FIXNM(argv[ii]);                                               \
          if (!current_result)                                                      \
            break;                                                                  \
        }                                                                           \
      break;                                                                        \
                                                                                    \
    case INEXACT:                                                                   \
      flo_prev = get_c_flonum(argv[0]);                                             \
                                                                                    \
      for (size_t ii = 1; ii < argc; ii++)                                          \
        {                                                                           \
          current_result = current_result && (flo_prev op get_c_flonum(argv[ii]));  \
          flo_prev = get_c_flonum(argv[ii]);                                        \
          if (!current_result)                                                      \
            break;                                                                  \
        }                                                                           \
      break;                                                                        \
    }                                                                               \
                                                                                    \
    return boolcons(current_result);                                                \
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

     if (COMPLEXP(x) || COMPLEXP(y))
          return cmplxcons(get_c_flonum(x) + get_c_flonum(y),
                           get_c_flonum_im(x) + get_c_flonum_im(y));

     if (FLONUMP(x) || FLONUMP(y))
          return flocons(get_c_flonum(x) + get_c_flonum(y));

     fixnum_t xf = FIXNM(x);
     fixnum_t yf = FIXNM(y);

     if (((yf>0) && (xf > (FIXNUM_MAX-yf))) || ((yf<0) && (xf < (FIXNUM_MIN-yf))))
          return vmtrap(TRAP_OVERFLOW_FIXNUM_ADD, VMT_MANDATORY_TRAP, 2, x, y);

     return fixcons(xf + yf);
}

lref_t lmultiply(lref_t x, lref_t y)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y))
          return x;

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     if (COMPLEXP(x) || COMPLEXP(y))
     {
          flonum_t xr = get_c_flonum(x);
          flonum_t yr = get_c_flonum(y);
          flonum_t xi = get_c_flonum_im(x);
          flonum_t yi = get_c_flonum_im(y);

          return cmplxcons(xr * yr - xi * yi, xr * yi + xi * yr);
     }

     if (FLONUMP(x) || FLONUMP(y))
          return flocons(get_c_flonum(x) * get_c_flonum(y));

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

lref_t lsubtract(lref_t x, lref_t y)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y))
     {
          if (COMPLEXP(x))
               return cmplxcons(-FLONM(x), -CMPLXIM(x));
          else if (FLONUMP(x))
               return flocons(-FLONM(x));

          fixnum_t xf = FIXNM(x);

          if (xf == FIXNUM_MIN)
               return vmtrap(TRAP_OVERFLOW_FIXNUM_NEGATE, VMT_MANDATORY_TRAP, 1, x);

          return fixcons(-xf);
     }

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     if (COMPLEXP(x) || COMPLEXP(y))
          return cmplxcons(get_c_flonum(x) - get_c_flonum(y),
                           get_c_flonum_im(x) - get_c_flonum_im(y));

     if (FLONUMP(x) || FLONUMP(y))
          return flocons(get_c_flonum(x) - get_c_flonum(y));

     fixnum_t xf = FIXNM(x);
     fixnum_t yf = FIXNM(y);

     if ((yf > 0 && xf < FIXNUM_MIN + yf)
         || (yf < 0 && xf > FIXNUM_MAX + yf))
          return vmtrap(TRAP_OVERFLOW_FIXNUM_SUBTRACT, VMT_MANDATORY_TRAP, 2, x, y);

     return fixcons(xf - yf);
}

lref_t ldivide(lref_t x, lref_t y)
{
     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);

     if (NULLP(y))
     {
          if (COMPLEXP(x))
          {
               flonum_t xr = get_c_flonum(x);
               flonum_t xi = get_c_flonum_im(x);

               flonum_t d = xr * xr + xi * xi;

               return cmplxcons(xr / d, -xi / d);
          }
          else
               return flocons(1 / get_c_flonum(x));
     }

     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     if (COMPLEXP(x) || COMPLEXP(y))
     {
          flonum_t xr = get_c_flonum(x);
          flonum_t yr = get_c_flonum(y);
          flonum_t xi = get_c_flonum_im(x);
          flonum_t yi = get_c_flonum_im(y);

          flonum_t d = yr * yr + yi * yi;

          return cmplxcons((xr * yr + xi * yi) / d, (xi * yr - xr * yi) / d);
     }

     return flocons(get_c_flonum(x) / get_c_flonum(y));
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

/* IEEE-754 bit conversion *************************************/

lref_t lto_ieee754_bits(lref_t x)
{
     if (!REALP(x))
          vmerror_wrong_type_n(1, x);

     double value = get_c_double(x);

     unsigned_fixnum_t bits = 0;

     for (uint8_t * loc = (uint8_t *) & value; loc < (uint8_t *) & ((&value)[1]); loc++)
     {
          bits <<= 8;
          bits |= *loc;
     }

     return fixcons((fixnum_t) (bits));
}

lref_t lieee754_bits_to(lref_t x)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type_n(1, x);

     unsigned_fixnum_t bits = FIXNM(x);

     double value = 0;

     for (uint8_t * loc = ((uint8_t *) & ((&value)[1])) - 1; loc >= (uint8_t *) & value; loc--)
     {
          *loc = (uint8_t) (bits & 0xFF);
          bits >>= 8;
     }

     return flocons(value);
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
     if (REALP(x))
          return (flocons(exp(get_c_double(x))));

     /*  e^(y+xi) =  e^y (cos x + i sin x) */

     flonum_t ere = exp(get_c_flonum(x));
     flonum_t im = get_c_flonum_im(x);

     return cmplxcons(ere * cos(im), ere * sin(im));
}

lref_t llog(lref_t x)
{
     flonum_t xr = get_c_flonum(x);

     if ((REALP(x) || FIXNUMP(x)) && (xr >= 0.0))
          return (flocons(log(xr)));

     flonum_t xi = get_c_flonum_im(x);

     return cmplxcons(log(sqrt(xr * xr + xi * xi)), atan2(xi, xr));
}


lref_t lsin(lref_t x)
{
     if (COMPLEXP(x))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return (flocons(sin(get_c_double(x))));
}

lref_t lcos(lref_t x)
{
     if (COMPLEXP(x))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return (flocons(cos(get_c_double(x))));
}

lref_t ltan(lref_t x)
{
     if (COMPLEXP(x))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return (flocons(tan(get_c_double(x))));
}

lref_t lasin(lref_t x)
{
     if (COMPLEXP(x))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return (flocons(asin(get_c_double(x))));
}

lref_t lacos(lref_t x)
{
     if (COMPLEXP(x))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     return (flocons(acos(get_c_double(x))));
}

lref_t latan(lref_t x, lref_t y)
{
     if (COMPLEXP(x) || COMPLEXP(y))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     if (!NULLP(y))
          return (flocons(atan2(get_c_double(x), get_c_double(y))));
     else
          return (flocons(atan(get_c_double(x))));
}

lref_t lsqrt(lref_t x)
{
     flonum_t xr = get_c_double(x);

     if (COMPLEXP(x))
     {
          flonum_t xr = get_c_flonum(x);
          flonum_t xi = get_c_flonum_im(x);

          flonum_t c = pow((xr * xr) + (xi * xi), 0.25);
          flonum_t a = 0.5 * atan2(xi, xr);

          return cmplxcons(c * cos(a), c * sin(a));
     }
     else if (xr < 0.0)
          return cmplxcons(0.0, sqrt(-xr));
     else
          return flocons(sqrt(xr));
}


lref_t lexpt(lref_t x, lref_t y)
{
     if (COMPLEXP(x) || COMPLEXP(y))
          vmerror_unimplemented(_T("unimplemented for complex numbers"));

     if (!NUMBERP(x))
          vmerror_wrong_type_n(1, x);
     if (!NUMBERP(y))
          vmerror_wrong_type_n(2, y);

     return (flocons(pow(get_c_double(x), get_c_double(y))));
}

/* Complex Number Accessors **********************************/

lref_t lmake_rectangular(lref_t re, lref_t im)
{
     if (!(REALP(re) || FIXNUMP(re)))
          vmerror_wrong_type_n(1, re);

     if (!(REALP(im) || FIXNUMP(im)))
          vmerror_wrong_type_n(2, im);

     return cmplxcons(get_c_double(re), get_c_double(im));
}

lref_t lmake_polar(lref_t r, lref_t theta)
{
     if (!(REALP(r) || FIXNUMP(r)))
          vmerror_wrong_type_n(1, r);

     if (!(REALP(theta) || FIXNUMP(theta)))
          vmerror_wrong_type_n(2, theta);

     flonum_t fr = get_c_double(r);
     flonum_t thetar = get_c_double(theta);

     return cmplxcons(fr * cos(thetar), fr * sin(thetar));
}

lref_t lreal_part(lref_t cmplx)
{
     if (FIXNUMP(cmplx) || REALP(cmplx))
          return cmplx;

     if (!COMPLEXP(cmplx))
          vmerror_wrong_type_n(1, cmplx);

     return flocons(FLONM(cmplx));
}

lref_t limag_part(size_t argc, lref_t argv[])
{
     if (argc < 1)
          vmerror_wrong_type(NIL);

     lref_t cmplx = argv[0];

     if (FIXNUMP(cmplx))
          return (argc > 1) ? argv[1] : fixcons(0);

     if (!FLONUMP(cmplx))
          vmerror_wrong_type(cmplx);

     if (NULLP(FLOIM(cmplx)))
          return (argc > 1) ? argv[1] :flocons(0.0);
     else
          return FLOIM(cmplx);
}


lref_t langle(lref_t cmplx)
{
     if (!NUMBERP(cmplx))
          vmerror_wrong_type_n(1, cmplx);

     return flocons(atan2(get_c_flonum_im(cmplx), get_c_flonum(cmplx)));
}

lref_t lmagnitude(lref_t cmplx)
{
     if (FIXNUMP(cmplx))
          return (FIXNM(cmplx) < 0) ? fixcons(-FIXNM(cmplx)) : cmplx;
     else if (REALP(cmplx))
          return (FLONM(cmplx) < 0) ? flocons(-FLONM(cmplx)) : cmplx;
     else if (!COMPLEXP(cmplx))
          vmerror_wrong_type_n(1, cmplx);

     flonum_t xr = get_c_flonum(cmplx);
     flonum_t xi = get_c_flonum_im(cmplx);

     return flocons(sqrt(xr * xr + xi * xi));
}

/* Random number generator ************************************/

lref_t lrandom(lref_t n)            /*  TESTTHIS */
{
     if (NULLP(n))
          return flocons(mt19937_real2());

     if (FIXNUMP(n))
     {
          fixnum_t range = FIXNM(n);

          if (range == 0)
               vmerror_arg_out_of_range(n, _T(">0"));

          return fixcons(mt19937_int64() % range);
     }
     else if (!FLONUMP(n))
          vmerror_wrong_type_n(1, n);

     flonum_t re_range = get_c_flonum(n);
     flonum_t im_range = get_c_flonum_im(n);

     if (re_range == 0.0)
          vmerror_arg_out_of_range(n, _T("<>0.0"));

     if (im_range == 0.0)
          return flocons(mt19937_real2() * re_range);
     else
          return cmplxcons(mt19937_real2() * re_range, mt19937_real2() * im_range);
}


lref_t lset_random_seed(lref_t s)
{
     fixnum_t seed = 1;

     if (NULLP(s))
          seed = time(NULL);
     else if (FIXNUMP(s))
          seed = FIXNM(s);
     else
          vmerror_wrong_type_n(1, s);

     init_mt19937((unsigned long) seed);

     return fixcons(seed);
}

