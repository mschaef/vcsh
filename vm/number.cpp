
/* number.cpp */

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

#include "scan.h"
#include "mt19937.h"

BEGIN_NAMESPACE(scan)

/**************************************************************
 * Numeric data type implementations
 */

/**************************************************************
 * Number constructors
 */
LRef fixcons(u32_t high, u32_t low)
{
     return fixcons(((fixnum_t) high << 32) + (fixnum_t) low);
}

LRef fixcons(fixnum_t x)
{
     if ((x <= MAX_LREF_FIXNUM) && (x >= MIN_LREF_FIXNUM))
          return LREF1_CONS(LREF1_FIXNUM, (iptr_t) x);

     LRef retval = new_cell(TC_FIXNUM);
     _FIXNM(retval) = x;

     return retval;
}

LRef flocons(flonum_t x)
{
     LRef z = new_cell(TC_FLONUM);

     SET_FLONM(z, x);
     SET_FLOIM(z, NIL);

     return (z);
}

LRef cmplxcons(flonum_t re, flonum_t im)
{
     LRef z = new_cell(TC_FLONUM);

     SET_FLONM(z, re);
     SET_FLOIM(z, flocons(im));

     return (z);
}

/* Number accessors *******************************************/

long get_c_long(LRef x)
{
     return (long) get_c_fixnum(x);
}

double get_c_double(LRef x)
{
     return (double) get_c_flonum(x);
}

fixnum_t get_c_fixnum(LRef x)   /*  REVISIT: how should this handle inan, ineginf, & iposinf */
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FIXNUMP(x))
          return FIXNM(x);
     else
          return (fixnum_t) FLONM(x);
}

flonum_t get_c_flonum(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(x);

     if (FLONUMP(x))
          return (FLONM(x));
     else
          return (double) (FIXNM(x));
}

flonum_t get_c_flonum_im(LRef x)
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

LRef lnumberp(LRef x)
{
     if (NUMBERP(x))
          return x;
     else
          return boolcons(false);
}

LRef lcomplexp(LRef x)
{
     if (NUMBERP(x))
          return x;
     else
          return boolcons(false);
}

LRef lrealp(LRef x)
{
     if ((FIXNUMP(x) || REALP(x)))
          return x;
     else
          return boolcons(false);
}

LRef lrationalp(LRef x)
{
     if (FIXNUMP(x))
          return x;
     else
          return boolcons(false);
}

LRef lintegerp(LRef x)
{
     if (FIXNUMP(x))
          return x;
     else
          return boolcons(false);
}

LRef lexactp(LRef x)
{
     if (FIXNUMP(x))
          return x;
     else
          return boolcons(false);
}

LRef linexactp(LRef x)
{
     if (FLONUMP(x))
          return x;
     else
          return boolcons(false);
}

LRef lnanp(LRef x)
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

LRef linfinitep(LRef x)
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

LRef lexact2inexact(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);

     return FIXNUMP(x) ? flocons((flonum_t) FIXNM(x)) : x;
}

LRef linexact2exact(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);

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

NumericArgumentType validate_numeric_arguments(size_t argc, LRef argv[])
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
LRef fn_name(size_t argc, LRef argv[])                                              \
{                                                                                   \
    if (argc == 0)                                                                  \
      return boolcons(true);                                                        \
                                                                                    \
    bool current_result = true;                                                     \
    double flo_prev;                                                                \
    fixnum_t fix_prev;                                                              \
    _TCHAR char_prev;                                                               \
                                                                                    \
    NumericArgumentType type = validate_numeric_arguments(argc, argv);              \
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

LRef ladd(LRef x, LRef y)
{
     if (!NUMBERP(x))
          return vmerror_wrong_type(1, x);
     else if (NULLP(y))
          return x;
     else if (!NUMBERP(y))
          return vmerror_wrong_type(2, y);
     else if (COMPLEXP(x) || COMPLEXP(y))
          return cmplxcons(get_c_flonum(x) + get_c_flonum(y),
                           get_c_flonum_im(x) + get_c_flonum_im(y));
     else if (FLONUMP(x) || FLONUMP(y))
          return flocons(get_c_flonum(x) + get_c_flonum(y));
     else
          return fixcons(get_c_fixnum(x) + get_c_fixnum(y));

     /*  TODO add overflow */
};

LRef lmultiply(LRef x, LRef y)
{
     if (!NUMBERP(x))
          return vmerror_wrong_type(1, x);
     else if (NULLP(y))
          return x;
     else if (!NUMBERP(y))
          return vmerror_wrong_type(2, y);
     else if (COMPLEXP(x) || COMPLEXP(y))
     {
          flonum_t xr = get_c_flonum(x);
          flonum_t yr = get_c_flonum(y);
          flonum_t xi = get_c_flonum_im(x);
          flonum_t yi = get_c_flonum_im(y);

          return cmplxcons(xr * yr - xi * yi, xr * yi + xi * yr);
     }
     else if (FLONUMP(x) || FLONUMP(y))
          return flocons(get_c_flonum(x) * get_c_flonum(y));
     else
          return fixcons(get_c_fixnum(x) * get_c_fixnum(y));

     /*  TODO multiply overflow */
}

LRef lsubtract(LRef x, LRef y)
{
     if (!NUMBERP(x))
          return vmerror_wrong_type(1, x);
     else if (NULLP(y))
     {
          if (COMPLEXP(x))
               return cmplxcons(-get_c_flonum(x), -get_c_flonum_im(x));
          else if (FLONUMP(x))
               return flocons(-get_c_flonum(x));
          else
               return fixcons(-get_c_fixnum(x));
     }
     else if (!NUMBERP(y))
          return vmerror_wrong_type(2, y);
     else if (COMPLEXP(x) || COMPLEXP(y))
          return cmplxcons(get_c_flonum(x) - get_c_flonum(y),
                           get_c_flonum_im(x) - get_c_flonum_im(y));
     else if (FLONUMP(x) || FLONUMP(y))
          return flocons(get_c_flonum(x) - get_c_flonum(y));
     else
          return fixcons(get_c_fixnum(x) - get_c_fixnum(y));

     /*  TODO subtract overflow */
}

LRef ldivide(LRef x, LRef y)
{
     if (!NUMBERP(x))
          return vmerror_wrong_type(1, x);
     else if (NULLP(y))
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
     else if (!NUMBERP(y))
          return vmerror_wrong_type(2, y);
     else if (COMPLEXP(x) || COMPLEXP(y))
     {
          flonum_t xr = get_c_flonum(x);
          flonum_t yr = get_c_flonum(y);
          flonum_t xi = get_c_flonum_im(x);
          flonum_t yi = get_c_flonum_im(y);

          flonum_t d = yr * yr + yi * yi;

          return cmplxcons((xr * yr + xi * yi) / d, (xi * yr - xr * yi) / d);
     }
     else
          return flocons(get_c_flonum(x) / get_c_flonum(y));

     /*  TODO divide overflow */
}

/* Number-theoretic division **********************************/

static flonum_t truncate(flonum_t x)
{
     return (x < 0) ? ceil(x) : floor(x);
}

LRef lquotient(LRef x, LRef y)
{
     if (!REALP(x))
          vmerror_wrong_type(1, x);

     if (!REALP(y))
          vmerror_wrong_type(2, y);

     if (FIXNUMP(x) && FIXNUMP(y))
     {
          fixnum_t yf = get_c_fixnum(y);

          if (yf == 0)
               vmerror("Division by zero", y);

          return fixcons(get_c_fixnum(x) / yf);
     }

     flonum_t yf = get_c_flonum(y);

     if (yf == 0.0)
          vmerror("Divison by zero", y);

     return flocons(truncate(get_c_flonum(x) / yf));
}

LRef lremainder(LRef x, LRef y)
{
     if (!REALP(x))
          vmerror_wrong_type(1, x);

     if (!REALP(y))
          vmerror_wrong_type(2, y);

     if (FIXNUMP(x) && FIXNUMP(y))
     {
          fixnum_t xf = get_c_fixnum(x);
          fixnum_t yf = fixabs(get_c_fixnum(y));

          if (yf == 0)
               vmerror("Division by zero", y);

          return fixcons(xf - ((xf / yf) * yf));
     }

     flonum_t xf = get_c_flonum(x);
     flonum_t yf = get_c_flonum(y);

     if (yf == 0.0)
          vmerror("Divison by zero", y);

     return flocons(xf - (truncate(xf / yf) * yf));
}

LRef lmodulo(LRef x, LRef y)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type(2, y);

     fixnum_t yf = get_c_fixnum(y);
     fixnum_t xf = get_c_fixnum(x);

     if (yf == 0)
          vmerror("Division by zero", y);

     fixnum_t mod = fixabs(xf) % fixabs(yf);

     if ((yf > 0) != (xf > 0))
          mod = fixabs(yf) - mod;

     return fixcons(yf > 0 ? mod : -mod);
}


  /* Truncation and Rounding *********************************** */

LRef lfloor(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(floor(FLONM(x)));
}

LRef lceiling(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);

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

LRef lround(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(round(FLONM(x)));
}

LRef ltruncate(LRef x)
{
     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);

     if (FIXNUMP(x))
          return x;
     else
          return flocons(truncate(FLONM(x)));
}

/* IEEE-754 bit conversion *************************************/

LRef lto_ieee754_bits(LRef x)
{
     if (!REALP(x))
          vmerror_wrong_type(1, x);

     double value = get_c_double(x);

     unsigned_fixnum_t bits = 0;

     for (u8_t * loc = (u8_t *) & value; loc < (u8_t *) & ((&value)[1]); loc++)
     {
          bits <<= 8;
          bits |= *loc;
     }

     return fixcons((fixnum_t) (bits));
}

LRef lieee754_bits_to(LRef x)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     unsigned_fixnum_t bits = FIXNM(x);

     double value = 0;

     for (u8_t * loc = ((u8_t *) & ((&value)[1])) - 1; loc >= (u8_t *) & value; loc--)
     {
          *loc = (u8_t) (bits & 0xFF);
          bits >>= 8;
     }

     return flocons(value);
}

/* Bitwise operations *****************************************/

LRef lbitwise_and(LRef x, LRef y)
{
     if (NULLP(y))
          return x;

     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type(2, x);

     return fixcons(FIXNM(x) & FIXNM(y));

}

LRef lbitwise_or(LRef x, LRef y)
{
     if (NULLP(y))
          return x;

     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type(2, x);

     return fixcons(FIXNM(x) | FIXNM(y));
}

LRef lbitwise_xor(LRef x, LRef y)
{
     if (NULLP(y))
          return x;

     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(y))
          vmerror_wrong_type(2, x);

     return fixcons(FIXNM(x) ^ FIXNM(y));
}

LRef lbitwise_not(LRef x)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     return fixcons(~FIXNM(x));
}

LRef lbitwise_shl(LRef x, LRef n)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(n))
          vmerror_wrong_type(2, n);

     fixnum_t bits = FIXNM(n);

     if (bits >= FIXNUM_BITS)
          return fixcons(0);

     return fixcons(FIXNM(x) << bits);
}

LRef lbitwise_shr(LRef x, LRef n)
{
     unsigned_fixnum_t ux;
     fixnum_t sx;

     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(n))
          vmerror_wrong_type(2, n);

     fixnum_t bits = FIXNM(n);

     if (bits >= FIXNUM_BITS)
          return fixcons(0);

     ux = (unsigned_fixnum_t) FIXNM(x);

     ux = ux >> bits;

     sx = (fixnum_t) ux;

     return fixcons(sx);
}

LRef lbitwise_ashr(LRef x, LRef n)
{
     if (!FIXNUMP(x))
          vmerror_wrong_type(1, x);

     if (!FIXNUMP(n))
          vmerror_wrong_type(2, n);

     fixnum_t bits = FIXNM(n);

     if (bits >= FIXNUM_BITS)
          return fixcons(0);

     return fixcons(FIXNM(x) >> bits);
}

/* Transcenendal ***********************************************/

LRef lexp(LRef x)
{
     if (REALP(x))
          return (flocons(exp(get_c_double(x))));
     else
     {
          /*  e^(y+xi) =  e^y (cos x + i sin x) */

          flonum_t ere = exp(get_c_flonum(x));
          flonum_t im = get_c_flonum_im(x);

          return cmplxcons(ere * cos(im), ere * sin(im));
     }
}

LRef llog(LRef x)
{
     flonum_t xr = get_c_flonum(x);

     if ((REALP(x) || FIXNUMP(x)) && (xr >= 0.0))
          return (flocons(log(xr)));
     else
     {
          flonum_t xi = get_c_flonum_im(x);

          return cmplxcons(log(sqrt(xr * xr + xi * xi)), atan2(xi, xr));
     }
}


/*  TODO Complex trancendental */

LRef lsin(LRef x)
{
     if (COMPLEXP(x))
          vmerror("sin Unimplemented for complex numbers.", x);

     return (flocons(sin(get_c_double(x))));
}

LRef lcos(LRef x)
{
     if (COMPLEXP(x))
          vmerror("cos Unimplemented for complex numbers.", x);

     return (flocons(cos(get_c_double(x))));
}

LRef ltan(LRef x)
{
     if (COMPLEXP(x))
          vmerror("tan Unimplemented for complex numbers.", x);

     return (flocons(tan(get_c_double(x))));
}

LRef lasin(LRef x)
{
     if (COMPLEXP(x))
          vmerror("asin Unimplemented for complex numbers.", x);

     return (flocons(asin(get_c_double(x))));
}

LRef lacos(LRef x)
{
     if (COMPLEXP(x))
          vmerror("acos Unimplemented for complex numbers.", x);

     return (flocons(acos(get_c_double(x))));
}

LRef latan(LRef x, LRef y)
{
     if (COMPLEXP(x) || COMPLEXP(y))
          vmerror("atan Unimplemented for complex numbers.", lcons(x, y));

     if (!NULLP(y))
          return (flocons(atan2(get_c_double(x), get_c_double(y))));
     else
          return (flocons(atan(get_c_double(x))));
}

LRef lsqrt(LRef x)
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


LRef lexpt(LRef x, LRef y)
{
     if (COMPLEXP(x) || COMPLEXP(y))
          vmerror("expt Unimplemented for complex numbers.", lcons(x, y));

     if (!NUMBERP(x))
          vmerror_wrong_type(1, x);
     if (!NUMBERP(y))
          vmerror_wrong_type(2, y);

     return (flocons(pow(get_c_double(x), get_c_double(y))));
}

/* Complex Number Accessors **********************************/

LRef lmake_rectangular(LRef re, LRef im)
{
     if (!(REALP(re) || FIXNUMP(re)))
          vmerror_wrong_type(1, re);

     if (!(REALP(im) || FIXNUMP(im)))
          vmerror_wrong_type(2, im);

     return cmplxcons(get_c_double(re), get_c_double(im));
}

LRef lmake_polar(LRef r, LRef theta)
{
     if (!(REALP(r) || FIXNUMP(r)))
          vmerror_wrong_type(1, r);

     if (!(REALP(theta) || FIXNUMP(theta)))
          vmerror_wrong_type(2, theta);

     flonum_t fr = get_c_double(r);
     flonum_t thetar = get_c_double(theta);

     return cmplxcons(fr * cos(thetar), fr * sin(thetar));
}

LRef lreal_part(LRef cmplx)
{
     if (FIXNUMP(cmplx) || REALP(cmplx))
          return cmplx;

     if (COMPLEXP(cmplx))
          return flocons(FLONM(cmplx));
     else
          return vmerror_wrong_type(1, cmplx);


}

LRef limag_part(LRef cmplx)
{
     if (FIXNUMP(cmplx))
          return fixcons(0);

     if (!FLONUMP(cmplx))
          vmerror_wrong_type(cmplx);

     if (NULLP(FLOIM(cmplx)))
          return flocons(0.0);
     else
          return FLOIM(cmplx);
}


LRef langle(LRef cmplx)
{
     if (NUMBERP(cmplx))
          return flocons(atan2(get_c_flonum_im(cmplx), get_c_flonum(cmplx)));
     else
          return vmerror_wrong_type(1, cmplx);
}

LRef lmagnitude(LRef cmplx)
{
     if (FIXNUMP(cmplx))
          return (FIXNM(cmplx) < 0) ? fixcons(-FIXNM(cmplx)) : cmplx;
     else if (REALP(cmplx))
          return (FLONM(cmplx) < 0) ? flocons(-FLONM(cmplx)) : cmplx;
     else if (COMPLEXP(cmplx))
     {
          flonum_t xr = get_c_flonum(cmplx);
          flonum_t xi = get_c_flonum_im(cmplx);

          return flocons(sqrt(xr * xr + xi * xi));
     }
     else
          return vmerror_wrong_type(1, cmplx);

}


/* Random number generator ************************************/

LRef lrandom(LRef n)            /*  TESTTHIS */
{
     if (NULLP(n))
     {
          return flocons(mt19937_real2());
     }
     if (FIXNUMP(n))
     {
          fixnum_t range = FIXNM(n);

          if (range == 0)
               vmerror("Invalid random range", n);

          return fixcons(mt19937_int64() % range);
     }
     else if (FLONUMP(n))
     {
          flonum_t re_range = get_c_flonum(n);
          flonum_t im_range = get_c_flonum_im(n);

          if (re_range == 0.0)
               vmerror("Invalid random range", n);

          if (im_range == 0.0)
               return flocons(mt19937_real2() * re_range);
          else
               return cmplxcons(mt19937_real2() * re_range, mt19937_real2() * im_range);
     }

     return vmerror_wrong_type(1, n);
}


LRef lset_random_seed(LRef s)
{
     fixnum_t seed = 1;

     if (NULLP(s))
          seed = time(NULL);
     else if (FIXNUMP(s))
          seed = FIXNM(s);
     else
          vmerror_wrong_type(1, s);

     init_mt19937((unsigned long) seed);

     return fixcons(seed);
}

END_NAMESPACE
