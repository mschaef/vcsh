/*
 * number-format.c --
 *
 * Format numbers.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <errno.h>
#include <math.h>
#include <stdlib.h>

#include "scan-private.h"

bool parse_string_as_fixnum(_TCHAR * string, int radix, fixnum_t *result)
{
     bool overflow = false;

     _TCHAR *endobj = NULL;

     assert((radix >= 2) && (radix <= 36));


#ifdef SCAN_64BIT
     *result = strtoll(string, &endobj, radix);

     if (((*result == INT64_MIN) || (*result == INT64_MAX)) && (errno == ERANGE))
          overflow = true;
#else
     *result = strtol(string, &endobj, radix);

     if (((*result == LONG_MIN) || (*result == LONG_MAX)) && (errno == ERANGE))
          overflow = true;
#endif

     if (overflow || (endobj == NULL) || (endobj == string) || (*endobj != _T('\0')))
          return false;
     else
          return true;
}

lref_t lstring2number(lref_t s, lref_t r)
{
     _TCHAR *endobj;
     long radix = 10;
     fixnum_t fix_result = 0;
     flonum_t flo_result = 0;

     if (!STRINGP(s))
          vmerror_wrong_type_n(1, s);

     if (!NULLP(r))
     {
          if (!FIXNUMP(r))
               vmerror_wrong_type_n(2, r);

          radix = (long) get_c_fixnum(r);
     }

     if ((radix > 36) || (radix < 2))
          vmerror_arg_out_of_range(r, _T("[2,36]"));

     _TCHAR string[STACK_STRBUF_LEN];
     if (get_c_string(s, STACK_STRBUF_LEN, string) < 0)
          vmerror_arg_out_of_range(s, _T("number string too long"));


     /* string->number doesn't do any sort of 'nice' processing
      * of the string it accepts. In other words, the string must
      * only contain a valid number to be parsed. No spaces or
      * anything else is tolerated in the parse. */
     if (parse_string_as_fixnum(string, radix, &fix_result))
          return fixcons(fix_result);

     /* If the fixnum parse failed, we need to have a radix of
      * 10 to continue onward to the flonum parse. If not, we
      * fail out. */
     if (radix != 10)
          return boolcons(false);

     flo_result = strtod(string, &endobj);

     if ((*endobj != '\0') || (endobj == string))
          return boolcons(false);

     return flocons(flo_result);
}

lref_t lnumber2string(lref_t x, lref_t r, lref_t s, lref_t p)
{
     _TCHAR buffer[STACK_STRBUF_LEN];
     fixnum_t radix = 10;
     bool signedp = false;

     if (!NULLP(r))
     {
          if (FIXNUMP(r))
               radix = get_c_fixnum(r);
          else
               vmerror_wrong_type_n(2, r);
     }

     if (!NULLP(s))
          signedp = TRUEP(s);

     int digits = DEBUG_FLONUM_PRINT_PRECISION;

     if (!NULLP(p))
     {
          if (!FIXNUMP(p))
               vmerror_wrong_type_n(4, p);

          digits = (int) get_c_fixnum(p);

          if ((digits < 0) || (digits > 16))
               vmerror_arg_out_of_range(p, _T("[0,16]"));
     }

     if (FLONUMP(x))
     {
          if (radix != 10)
               vmerror_arg_out_of_range(r, _T("=10 (with inexact arg)"));

          /* Nothing is as easy as it seems...
           *
           * The sprintf 'g' format code will drop the decimal
           * point if all following digits are zero. That causes
           * the reader to read such numbers as exact, rather than
           * inexact. As a result, we need to implement our own
           * switching between scientific and conventional notation.
           */
          double scale = 0.0;

          if (FLONM(x) != 0.0)
               scale = log10(fabs(FLONM(x)));

          if (fabs(scale) >= digits)
               _sntprintf(buffer, STACK_STRBUF_LEN, _T("%.*e"), digits, FLONM(x));
          else
          {
               /* Prevent numbers on the left of the decimal point from
                * adding to the number of digits we print. */
               if ((scale > 0) && (scale <= digits))
                    digits -= (int) scale;

               _sntprintf(buffer, STACK_STRBUF_LEN, _T("%.*f"), digits, FLONM(x));
          }
     }
     else if (FIXNUMP(x))
     {
          switch (radix)
          {
          case 10:
               if (signedp)
                    _sntprintf(buffer, STACK_STRBUF_LEN, _T("%" SCAN_PRIiFIXNUM), FIXNM(x));
               else
                    _sntprintf(buffer, STACK_STRBUF_LEN, _T("%" SCAN_PRIuFIXNUM), FIXNM(x));
               break;

          case 16:
               if (!signedp || (FIXNM(x) > 0))
                    _sntprintf(buffer, STACK_STRBUF_LEN, _T("%" SCAN_PRIxFIXNUM), FIXNM(x));
               else
                    _sntprintf(buffer, STACK_STRBUF_LEN, _T("-%" SCAN_PRIxFIXNUM), -FIXNM(x));
               break;

          case 8:
               if (!signedp || (FIXNM(x) > 0))
                    _sntprintf(buffer, STACK_STRBUF_LEN, _T("%" SCAN_PRIoFIXNUM), FIXNM(x));
               else
                    _sntprintf(buffer, STACK_STRBUF_LEN, _T("-%" SCAN_PRIoFIXNUM), -FIXNM(x));
               break;

          default:
               vmerror_unimplemented(_T("unimplemented radix (8, 10, and 16 are allowed)"));
               break;
          }
     }
     else
          vmerror_wrong_type_n(1, x);

     return strconsbuf(buffer);
}


typedef enum
{
     NO_SEPERATOR,
     US_SEPERATOR,
     EURO_SEPERATOR
} float_seperator;

size_t float_format(_TCHAR * buf,
                    size_t buf_len,
                    double nd,
                    int sigfigs,
                    bool round,
                    bool scientific,
                    float_seperator sep)
{
     /* First things first, get the easy cases out of the way. */
     if (isnan(nd))
          return _sntprintf(buf, buf_len, _T("<not-a-number>"));
     else if (!isfinite(nd))
     {
          if (nd > 0)
               return _sntprintf(buf, buf_len, _T("<positive-infinity>"));
          else
               return _sntprintf(buf, buf_len, _T("<negative-infinity>"));
     }

     /* Handle rounding */
     if (round)
     {
          int places;
          double int_part;      /*  Unused */
          double frac_part;

          double adjust_amount = 0.5;

          frac_part = modf(nd, &int_part);

          for (places = 0; places < sigfigs; places++)
          {
               frac_part *= 10;
               adjust_amount /= 10;
          }

          frac_part = modf(frac_part, &int_part);

          if (nd > 0)
          {
               if (frac_part > 0.5)
                    nd += adjust_amount;
          }
          else
          {
               if (frac_part < -0.5)
                    nd -= adjust_amount;
          }
     }

     /* _ecvt paramaters */
     int ecvt_decimal;          /* position of decimal point */
     int sign;                  /* sign of number (1 = - ) */
     _TCHAR *ecvt_result = NULL;        /* result text */
     int precision = 16;        /* Input precision */

     /* working variables */
     int decimal;               /* count of numerals to the decimal */
     _TCHAR *result_loc = buf;  /* location in result buffer */
     bool first = true;         /* True on the first numeric character */

     /* Most of the interesting numerical work is done by _ecvt. _ecvt
      * gives us most of the information we need to print the number. */

     ecvt_result = ecvt(nd, precision, &ecvt_decimal, &sign);

     /* _ecvt formally leaves the return value of ecvt_decimal
      * undefined (either 0 or 1) if nd==0.0. Since we assume
      * it to be 1 in that case, we fix it up here. */
     if (nd == 0.0)
          ecvt_decimal = 1;

     /* We 'fail' over into scientific notation if we're printing a number
      * either too large for our precision or too small for the requested
      * number of significant digits. We also allow for the case the caller
      * has explicitly requested scientific notation. */
     if ((ecvt_decimal >= precision) || (ecvt_decimal <= -sigfigs) || scientific)
     {
          decimal = 1;
          scientific = true;
     }
     else
          decimal = ecvt_decimal;


     /* Negative numbers of any stripe get a leading '-' */
     if (sign)
          *result_loc++ = '-';

     /* As long as we have characters to print and we're on the left side
      * of the decimal, we copy over characters placing a ',' every
      * third (except for the first). */
     while (*ecvt_result && (decimal > 0))
     {
          if (((decimal % 3) == 0) && !first)
          {
               switch (sep)
               {
               case NO_SEPERATOR:
                    break;

               case US_SEPERATOR:
                    *result_loc++ = ',';
                    break;

               case EURO_SEPERATOR:
                    *result_loc++ = '.';
                    break;
               }
          }

          *result_loc++ = *ecvt_result++;
          decimal--;
          first = FALSE;
     }

     /* At this point, we're at the decimal. If first is TRUE, we haven't
      * copied over any characters yet and our number needs a leading '0',
      * before the decimal. */
     if (first)
          *result_loc++ = '0';


     switch (sep)
     {
     case NO_SEPERATOR:
     case US_SEPERATOR:
          *result_loc++ = '.';
          break;

     case EURO_SEPERATOR:
          *result_loc++ = ',';
          break;
     }

     while ((decimal < 0) && (sigfigs > 0))
     {
          *result_loc++ = '0';
          decimal++;
          sigfigs--;
     }


     /* This character copy loop is like the left-of-the-decimal loop
      * except that it 1) doesn't insert group seperators and 2) assumes
      * that any unspecified character is a '0'. */
     while (sigfigs)
     {
          if (*ecvt_result)
               *result_loc++ = *ecvt_result++;
          else
               *result_loc++ = '?';

          sigfigs--;
     }

     /* In scientific notation, we have to add the exponent */
     if (scientific) {
          _stprintf(result_loc, _T("e%i"), ecvt_decimal - 1);
     } else {
          *result_loc = '\0';
     }

     return result_loc - buf;
}

lref_t linexact2display_string(lref_t n, lref_t sf, lref_t sci, lref_t s)
{
     _TCHAR buf[STACK_STRBUF_LEN];
     float_seperator sep = NO_SEPERATOR;

     if (!NUMBERP(n))
          vmerror_wrong_type_n(1, n);
     if (!FIXNUMP(sf))
          vmerror_wrong_type_n(2, sf);
     if (!SYMBOLP(s))
          vmerror_wrong_type_n(4, s);

     if (FIXNM(sf) < 0)
          vmerror_arg_out_of_range(sf, _T(">=0"));

     if (keyword_intern(_T("none")) == s)
          sep = NO_SEPERATOR;
     else if (keyword_intern(_T("us")) == s)
          sep = US_SEPERATOR;
     else if (keyword_intern(_T("euro")) == s)
          sep = EURO_SEPERATOR;
     else
          vmerror_arg_out_of_range(s, _T(":none, :us, or :euro"));

     float_format(buf, STACK_STRBUF_LEN, get_c_flonum(n), (int) get_c_fixnum(sf), true, TRUEP(sci), sep);

     return strconsbuf(buf);
}
