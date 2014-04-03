/*
 * text-data.c --
 *
 * Text data primitive structures: strings and characters.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <limits.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#include "scan-private.h"

/**************************************************************
 * Character
 */
lref_t charcons(_TCHAR ch)
{
     return MAKE_LREF2(LREF2_CHARACTER, ch);
}

lref_t lchar2integer(lref_t s)
{
     if (!CHARP(s))
          vmerror_wrong_type_n(1, s);

     return fixcons(CHARV(s));
}

lref_t lcharp(lref_t x)
{
     if (CHARP(x))
          return x;
     else
          return boolcons(false);
}

lref_t linteger2char(lref_t s)
{
     if (!NUMBERP(s))
          vmerror_wrong_type_n(1, s);

     fixnum_t c = get_c_fixnum(s);

     if ((c < 0) || (c > 255))
          vmerror_arg_out_of_range(s, _T("[0,255]"));

     return charcons((_TCHAR) c);
}

/**************************************************************
 * String data type implementation
 */

bool string_equal(lref_t a, lref_t b)
{
     size_t len;

     assert(TYPE(a) == TC_STRING);

     len = STRING_DIM(a);

     if (len != STRING_DIM(b))
          return false;

     if (memcmp(STRING_DATA(a), STRING_DATA(b), len) == 0)
          return true;
     else
          return false;
}

/* String Constructor *****************************************/

static size_t string_storage_size(size_t bytes_to_be_stored)
{
     /* Strings are stored in the smallest power-of-2 sized block
      * of memory in which they (plus a pad byte) will fit. The
      * minimum block size is 16 bytes, everything smaller is rounded
      * up. */
     size_t storage_size = 16;

     size_t i = bytes_to_be_stored;

     while (i >= 16)
     {
          i >>= 1;

          storage_size <<= 1;
     }

     assert(bytes_to_be_stored <= storage_size);

     return storage_size;
}

lref_t strrecons(lref_t obj, size_t new_length)
{
     size_t space_needed;
     size_t space_already_allocated = 0;

     space_needed = string_storage_size(new_length + 1);

     if (STRING_DATA(obj) != NULL)
          space_already_allocated = string_storage_size(STRING_DIM(obj));

     if (space_already_allocated != space_needed)
     {
          _TCHAR *new_buffer = (_TCHAR *) gc_malloc(space_needed);

          memset(new_buffer, 0, space_needed);

          if (STRING_DATA(obj))
          {
               memcpy(new_buffer, STRING_DATA(obj), STRING_DIM(obj) + 1);

               gc_free(STRING_DATA(obj));
          }

          SET_STRING_DATA(obj, new_buffer);
     }

     SET_STRING_DIM(obj, new_length);

     return obj;
}


lref_t strcons()
{
     return strconsbufn(0, (const _TCHAR *) NULL);
}

lref_t strconsch(_TCHAR ch)
{
     return strconsbufn(1, &ch);
}

lref_t strconsbuf(const _TCHAR * buffer)
{
     assert(buffer);

     return strconsbufn(_tcslen(buffer), buffer);
}


lref_t strconsbuf1(const _TCHAR * buffer, _TCHAR trailing)
{
     assert(buffer);

     size_t len = _tcslen(buffer);

     lref_t retval = strconsbufn(len + 1, buffer);

     STRING_DATA(retval)[len] = trailing;

     return retval;
}

lref_t strconsdup(lref_t str)
{
     assert(STRINGP(str));

     return strconsbufn(STRING_DIM(str), STRING_DATA(str));
}

lref_t strconsbufn(size_t length, const _TCHAR * buffer)
{
     lref_t new_string = new_cell(TC_STRING);

     SET_STRING_DATA(new_string, NULL);
     SET_STRING_DIM(new_string, 0);

     strrecons(new_string, length);

     if (buffer)
          memcpy(STRING_DATA(new_string), buffer, length);

     return new_string;
}

lref_t strcons_transfer_buffer(size_t length, _TCHAR * buffer)
{
     /* This variant of strcons is used to transfer ownership
      * of <buffer> to the interpreter, so that the interpreter
      * will free the buffer when the cell is reclaimed. <buffer>
      * must be dynamically allocated with gc_malloc for this
      * to work. */

     lref_t new_string = new_cell(TC_STRING);

     assert(buffer[length] == _T('\0'));        /*  String buffers must be null terminated. */

     SET_STRING_DATA(new_string, buffer);
     SET_STRING_DIM(new_string, length);

     return new_string;
}

/* string-ref *************************************************
 * string-set! */

lref_t lstring_ref(lref_t a, lref_t i)
{
     if (!STRINGP(a))
          vmerror_wrong_type_n(1, a);

     if (!FIXNUMP(i))
          vmerror_wrong_type_n(2, i);

     fixnum_t k = get_c_fixnum(i);

     if ((k < 0) || ((size_t) k >= STRING_DIM(a)))
          vmerror_index_out_of_bounds(i, a);

     return charcons(STRING_DATA(a)[k]);
}

lref_t lstring_set(lref_t a, lref_t i, lref_t v)
{
     if (!STRINGP(a))
          vmerror_wrong_type_n(1, a);

     if (!FIXNUMP(i))
          vmerror_wrong_type_n(2, i);

     fixnum_t k = get_c_fixnum(i);

     if ((k < 0) || ((size_t) k >= STRING_DIM(a)))
          vmerror_index_out_of_bounds(i, a);

     if (FIXNUMP(v))
          STRING_DATA(a)[k] = (_TCHAR) FIXNM(v);
     else if (CHARP(v))
          STRING_DATA(a)[k] = CHARV(v);
     else
          vmerror_wrong_type_n(3, v);

     return (a);
}

/* string-append **********************************************/

lref_t lstring_append(size_t argc, lref_t argv[])
{
     fixnum_t size = 0;
     lref_t current_string;

     for (size_t ii = 0; ii < argc; ii++)
     {
          current_string = argv[ii];

          if (SYMBOLP(current_string))
               current_string = SYMBOL_PNAME(current_string);

          if (STRINGP(current_string))
               size += STRING_DIM(current_string);
          else if (CHARP(current_string))
               size += 1;
          else if (FIXNUMP(current_string))
          {
               if ((FIXNM(current_string) < 0x00) || (FIXNM(current_string) > 0xFF))
                    vmerror_arg_out_of_range(current_string, _T("[0,255]"));
               else
                    size += 1;
          }
          else
               vmerror_wrong_type_n(ii, argv[ii]);
     }

     lref_t s = strconsbufn((size_t) size, NULL);

     _TCHAR *data = STRING_DATA(s);
     size_t pos = 0;

     for (size_t ii = 0; ii < argc; ii++)
     {
          current_string = argv[ii];

          if (SYMBOLP(current_string))
               current_string = SYMBOL_PNAME(current_string);

          if (STRINGP(current_string))
          {
               memcpy(data + pos, STRING_DATA(current_string), STRING_DIM(current_string));
               pos += STRING_DIM(current_string);
          }
          else if (CHARP(current_string))
          {
               data[pos] = CHARV(current_string);
               pos += 1;
          }
          else if (FIXNUMP(current_string))
          {
               data[pos] = (_TCHAR) (get_c_fixnum(current_string));
               pos += 1;
          }
          else
               panic("Unexpected string type in string-append concatenation phase.");
     }

     return (s);
}

/* substring **************************************************/

lref_t lsubstring(lref_t str, lref_t start, lref_t end)
{
     size_t s, e;

     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     s = get_c_long(start);
     e = NULLP(end) ? STRING_DIM(str) : get_c_long(end);

     if (s > STRING_DIM(str))
          vmerror_index_out_of_bounds(start, str);

     if (e > STRING_DIM(str))
          vmerror_index_out_of_bounds(end, str);

     if (s > e)
          vmerror_arg_out_of_range(start, _T("start<=end"));

     return strconsbufn(e - s, &(STRING_DATA(str)[s]));
}

/* string-search **********************************************/

size_t get_string_offset(lref_t maybe_ofs)
{
     if (NULLP(maybe_ofs))
          return 0;

     if (!NUMBERP(maybe_ofs))
          vmerror_wrong_type(maybe_ofs);

     long ofs = get_c_long(maybe_ofs);

     if (ofs < 0)
          vmerror_arg_out_of_range(maybe_ofs, _T(">=0"));

     return (size_t) ofs;
}

lref_t lstring_search(lref_t tok, lref_t str, lref_t maybe_initial_ofs)
{
     if (!STRINGP(tok) && !CHARP(tok))
          vmerror_wrong_type_n(1, tok);
     if (!STRINGP(str))
          vmerror_wrong_type_n(2, str);

     if (CHARP(tok))
          tok = strconsch(CHARV(tok));

     _TCHAR *tok_data = STRING_DATA(tok);
     _TCHAR *str_data = STRING_DATA(str);

     size_t str_loc = get_string_offset(maybe_initial_ofs);

     while (str_loc < STRING_DIM(str))
     {
          size_t tok_loc = 0;

          while ((tok_data[tok_loc] == str_data[str_loc + tok_loc])
                 && (tok_loc < STRING_DIM(tok)) && (str_loc + tok_loc < STRING_DIM(str)))
               tok_loc++;

          if (tok_loc == STRING_DIM(tok))
               return fixcons(str_loc);

          str_loc++;
     }

     return boolcons(false);
}

lref_t lstring_search_from_right(lref_t tok, lref_t str, lref_t maybe_from)
{

     if (!STRINGP(tok) && !CHARP(tok))
          vmerror_wrong_type_n(1, tok);
     if (!STRINGP(str))
          vmerror_wrong_type_n(2, str);

     if (CHARP(tok))
          tok = strconsch(CHARV(tok));

     _TCHAR *tok_data = STRING_DATA(tok);
     _TCHAR *str_data = STRING_DATA(str);

     size_t str_loc = STRING_DIM(str) - 1;

     if (!NULLP(maybe_from))
          str_loc = get_c_long(maybe_from);

     while (str_loc < STRING_DIM(str))
     {
          size_t tok_loc = STRING_DIM(tok) - 1;

          while (tok_data[tok_loc] == str_data[str_loc - (STRING_DIM(tok) - tok_loc - 1)])
          {
               if (tok_loc == 0)
                    return fixcons(str_loc - (STRING_DIM(tok) - 1));

               if ((str_loc - (STRING_DIM(tok) - tok_loc - 1)) == 0)
                    break;

               tok_loc--;
          }

          str_loc--;
     }

     return boolcons(false);
}


/* string-trim ************************************************
 * string-trim-left
 * string-trim-right */

lref_t lstring_trim(lref_t str, lref_t tc)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     const _TCHAR *trim_chars = _T(" \t\r\n");

     if (!NULLP(tc))
     {
          if (!STRINGP(tc))
               vmerror_wrong_type_n(2, str);

          trim_chars = get_c_string(tc);
     }


     size_t start = 0;
     size_t end = STRING_DIM(str);

     while ((start < STRING_DIM(str)) && strchr(trim_chars, STRING_DATA(str)[start]))
          start++;

     while ((end > start) && strchr(trim_chars, STRING_DATA(str)[end - 1]))
          end--;

     return strconsbufn(end - start, &(STRING_DATA(str)[start]));
}

lref_t lstring_trim_left(lref_t str, lref_t tc)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     const _TCHAR *trim_chars = _T(" \t\r\n");

     if (!NULLP(tc))
     {
          if (!STRINGP(tc))
               vmerror_wrong_type_n(2, str);

          trim_chars = get_c_string(tc);
     }

     size_t start = 0;

     while ((start < STRING_DIM(str)) && strchr(trim_chars, STRING_DATA(str)[start]))
          start++;

     return strconsbufn(STRING_DIM(str) - start, &(STRING_DATA(str)[start]));
}

lref_t lstring_trim_right(lref_t str, lref_t tc)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     const _TCHAR *trim_chars = _T(" \t\r\n");

     if (!NULLP(tc))
     {
          if (!STRINGP(tc))
               vmerror_wrong_type_n(2, str);

          trim_chars = get_c_string(tc);
     }

     size_t end = STRING_DIM(str);

     while ((end > 0) && strchr(trim_chars, STRING_DATA(str)[end - 1]))
          end--;

     return strconsbufn(end, STRING_DATA(str));
}


/* string-upcase **********************************************
 * string-downcase */

lref_t lstring_upcased(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     for (size_t loc = 0; loc < STRING_DIM(str); loc++)
          if (_istlower(STRING_DATA(str)[loc]))
               STRING_DATA(str)[loc] = (_TCHAR) _totupper(STRING_DATA(str)[loc]);

     return str;
}

lref_t lstring_upcase(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     return lstring_upcased(strconsdup(str));
}

lref_t lstring_downcased(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     for (size_t loc = 0; loc < STRING_DIM(str); loc++)
          if (_istupper(STRING_DATA(str)[loc]))
               STRING_DATA(str)[loc] = (_TCHAR) _totlower(STRING_DATA(str)[loc]);

     return str;
}

lref_t lstring_downcase(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     return lstring_downcased(strconsdup(str));
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
     _TCHAR *string, *endobj;
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

     string = get_c_string(s);

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

lref_t lisp_strcmp(lref_t s1, lref_t s2)
{
     size_t loc;

     if (!STRINGP(s1))
          vmerror_wrong_type_n(1, s1);
     if (!STRINGP(s2))
          vmerror_wrong_type_n(2, s2);

     for (loc = 0; (loc < STRING_DIM(s1)) && (loc < STRING_DIM(s2)); loc++)
     {
          if (STRING_DATA(s1)[loc] > STRING_DATA(s2)[loc])
               return fixcons(1);
          else if (STRING_DATA(s1)[loc] < STRING_DATA(s2)[loc])
               return fixcons(-1);
     }

     if (loc < STRING_DIM(s1))
          return fixcons(1);
     else if (loc < STRING_DIM(s2))
          return fixcons(-1);

     return fixcons(0);
}

lref_t lstringp(lref_t x)
{
     if (STRINGP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lstring_length(lref_t string)
{
     return fixcons(STRING_DIM(string));
}


lref_t lstring_first_char(lref_t string, lref_t char_set, lref_t maybe_initial_ofs)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     if (!VECTORP(char_set))
          vmerror_wrong_type_n(2, char_set);

     if (VECTOR_DIM(char_set) != _TCHAR_MAX)
          vmerror_index_out_of_bounds(fixcons(_TCHAR_MAX - 1), char_set);

     size_t loc = get_string_offset(maybe_initial_ofs);
     _TCHAR *str = STRING_DATA(string);

     for (; loc < STRING_DIM(string); loc++)
     {
          if (TRUEP(VECTOR_ELEM(char_set, str[loc])))
               return fixcons(loc);
     }

     return boolcons(false);
}

lref_t lstring_first_substring(lref_t string, lref_t char_set, lref_t maybe_initial_ofs)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     if (!VECTORP(char_set))
          vmerror_wrong_type_n(2, char_set);

     if (VECTOR_DIM(char_set) != _TCHAR_MAX)
          vmerror_index_out_of_bounds(fixcons(_TCHAR_MAX - 1), char_set);

     size_t substring_length = 0;
     size_t loc = get_string_offset(maybe_initial_ofs);
     _TCHAR *str = STRING_DATA(string);

     for (; loc < STRING_DIM(string); loc++)
     {
          if (!TRUEP(VECTOR_ELEM(char_set, str[loc])))
               break;

          substring_length++;
     }

     if (substring_length == 0)
          return boolcons(false);
     else
          return fixcons(loc);
}

lref_t lstring_copy(lref_t string)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     return strconsdup(string);
}

lref_t lcharacter2string(lref_t obj)
{
     if (!CHARP(obj))
          vmerror_wrong_type_n(1, obj);

     _TCHAR buf = CHARV(obj);

     return strconsch(buf);
}


void string_appendd(lref_t str, const _TCHAR *buf, size_t len)
{
     assert(STRINGP(str));

     size_t size = STRING_DIM(str);

     strrecons(str, size + len);

     memcpy(&(STRING_DATA(str)[size]), buf, len);
     STRING_DATA(str)[size + len + 1] = _T('\0');
}

/**************************************************************
 * C string access
 */

_TCHAR *try_get_c_string(lref_t x)
{
     if (SYMBOLP(x))
     {
          x = SYMBOL_PNAME(x);

          assert(STRINGP(x));
     }

     if (STRINGP(x))
          return STRING_DATA(x);
     else
          return NULL;
}

_TCHAR *get_c_string(lref_t x)
{
     _TCHAR *str = try_get_c_string(x);

     if (str)
          return str;

     vmerror_wrong_type(x);

     return NULL;
}

_TCHAR *get_c_string_dim(lref_t x, size_t * len)
{
     _TCHAR *rc = get_c_string(x);

     *len = _tcslen(rc);

     return rc;
}

typedef enum
{
     NO_SEPERATOR,
     US_SEPERATOR,
     EURO_SEPERATOR
} float_seperator;

size_t float_format(_TCHAR * buf, size_t buf_len,
                    double nd, int sigfigs, bool round, bool scientific, float_seperator sep)
{
     /* First things first, get the easy cases out of the way. */
     if (isnan(nd))
          return _sntprintf(buf, buf_len, _T("<not-a-number>"));
     else if (!finite(nd))
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
     if (scientific)
     {
          _stprintf(result_loc, _T("e%i"), ecvt_decimal - 1);
     }
     else
     {
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

     float_format(buf, STACK_STRBUF_LEN, get_c_double(n), (int) get_c_fixnum(sf), true, TRUEP(sci),
                  sep);

     return strconsbuf(buf);
}

