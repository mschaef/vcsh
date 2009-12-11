/* text-data.cpp
 *
 * Code related to textual data objects: strings and characters.
 */

#include "scan.h"

#include <limits.h>
#include <float.h>

namespace scan {

  /**************************************************************
   * Character
   */

  LRef charcons(_TCHAR ch)
  {
    return LREF2_CONS(LREF2_CHARACTER, ch);
  }

  LRef lchar2integer(LRef s)
  {
    if (!CHARP(s))
      vmerror_wrong_type(1, s);

    return fixcons(CHARV(s));
  }

  LRef lcharp(LRef x)
  {
    if (CHARP(x))
      return x;
    else
      return boolcons(false);
  }

  LRef linteger2char(LRef s)
  {
    if (!NUMBERP(s))
      vmerror_wrong_type(1, s);

    fixnum_t c = get_c_fixnum(s);

    if ((c < 0) || (c > 255))
      vmerror("integer out of range [0..255] to integer->char", s);

    return charcons((_TCHAR)c);
  }

  /**************************************************************
   * String data type implementation
   */

  bool string_equal(LRef a, LRef b)
  {
    size_t len;

    assert(TYPE (a) == TC_STRING);

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

    while(i >= 16)
      {
        i >>= 1;

        storage_size <<= 1;
      }

    assert(bytes_to_be_stored <= storage_size);

    return storage_size;
  }

  LRef strrecons(LRef obj, size_t new_length)
  {
    size_t space_needed;
    size_t space_already_allocated = 0;

    assert(STRING_OFS(obj) == 0);

    space_needed = string_storage_size(new_length + 1);

    if (STRING_DATA(obj) != NULL)
      space_already_allocated = string_storage_size(STRING_DIM(obj));

    if (space_already_allocated != space_needed)
      {
	_TCHAR *new_buffer = (_TCHAR *)safe_malloc(space_needed);

	memset(new_buffer, 0, space_needed);

	if (STRING_DATA(obj))
	  {
	    memcpy(new_buffer, STRING_DATA(obj), STRING_DIM(obj) + 1);

	    safe_free(STRING_DATA(obj));
	  }

	SET_STRING_DATA(obj, new_buffer);
      }

    SET_STRING_DIM(obj, new_length);

    return obj;
  }


  LRef strcons()
  {
    return strcons(0, (const _TCHAR *)NULL);
  }

  LRef strcons(_TCHAR ch) {
    return strcons(1, &ch);
  }

  LRef strcons(const _TCHAR *buffer)
  {
    assert(buffer);

    return strcons(_tcslen(buffer), buffer);
  }


  LRef strcons(const _TCHAR *buffer, _TCHAR trailing)
  {
    assert(buffer);

    size_t len = _tcslen(buffer);

    LRef retval = strcons(len + 1, buffer);

    STRING_DATA(retval)[len] = trailing;

    return retval;
  }

  LRef strcons(LRef str)
  {
    assert(STRINGP(str));

    return strcons(STRING_DIM(str), STRING_DATA(str));
  }

  LRef strcons(size_t length, const _TCHAR *buffer)
  {
    LRef new_string = new_cell(TC_STRING);

    SET_STRING_DATA(new_string, NULL);
    SET_STRING_OFS(new_string,  0);
    SET_STRING_DIM(new_string, 0);

    strrecons(new_string, length);

    if (buffer)
      memcpy (STRING_DATA(new_string), buffer, length);

    return new_string;
  }

  LRef strcons_transfer_buffer(size_t length, _TCHAR *buffer)
  {
    /* This variant of strcons is used to transfer ownership
     * of <buffer> to the interpreter, so that the interpreter
     * will free the buffer when the cell is reclaimed. <buffer>
     * must be dynamically allocated with safe_malloc for this
     * to work. */

    LRef new_string = new_cell(TC_STRING);

    assert(buffer[length] == _T('\0')); // String buffers must be null terminated.

    SET_STRING_DATA(new_string, buffer);
    SET_STRING_OFS(new_string,  0);
    SET_STRING_DIM(new_string, length);

    return new_string;
  }

  /* string-ref *************************************************
   * string-set! */

  LRef lstring_ref (LRef a, LRef i)
  {
    fixnum_t k;

    if (!FIXNUMP (i))
      vmerror("bad index to aref", i);

    k = get_c_fixnum(i);

    if (k < 0)
      vmerror("negative index to string-ref", i);

    if(!STRINGP(a))
      return vmerror("not a string", a);

    if (k >= STRING_DIM(a))
      vmerror("index too large", i);

    return charcons(STRING_DATA(a)[k]);
  }

  LRef lstring_set(LRef a, LRef i, LRef v)
  {
    fixnum_t k;

    if (!FIXNUMP (i))
      vmerror("bad index to string-set!", i);

    k = get_c_fixnum(i);
    if (k < 0)
      vmerror("negative index to string-set!", i);

    if(!STRINGP(a))
      return vmerror("not a string", a);

    if (k >= STRING_DIM(a))
      return vmerror("index to string-set too large", i);

    if (FIXNUMP(v))
      {
	STRING_DATA(a)[k] = (_TCHAR) FIXNM(v);
      }
    else if (CHARP(v))
      {
	STRING_DATA(a)[k] = CHARV(v);
      }
    else
      {
        vmerror("bad value to store in string", v);
      }

    return (a);
  }

  /* string-append **********************************************/

  LRef lstring_append(size_t argc, LRef argv[])
  {
    fixnum_t size = 0;
    LRef current_string;

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
	      vmerror("out of range, invalid character code", current_string);
            else
	      size += 1;
	  }
        else
	  vmerror_wrong_type(ii, argv[ii]);
      }

    LRef s = strcons((size_t)size, NULL);

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
            data[pos] = (_TCHAR)(get_c_fixnum(current_string));
            pos += 1;
	  }
        else
          panic("Unexpected string type in string-append concatenation phase.");
      }

    return (s);
  }

  /* substring **************************************************/

  LRef lsubstring(LRef str, LRef start, LRef end)
  {
    size_t s, e;

    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    s = get_c_long(start);
    e = NULLP(end) ? STRING_DIM(str) : get_c_long(end);

    if ((s < 0) || (s > STRING_DIM(str)))
      vmerror("bad start index", start);

    // REVISIT: it would be more permissive for substring to coerce e <= STRING_DIM. Is this desirable?
    if ((e < 0) || (e > STRING_DIM(str)))
       vmerror("bad end index", end);

    if (s > e)
      vmerror("start index after end index", start);

    return strcons(e - s, &(STRING_DATA(str)[s]));
  }

  /* string-search **********************************************/

  size_t get_string_offset(LRef maybe_ofs)
  {
    if (NULLP(maybe_ofs))
      return 0;

    if (!NUMBERP(maybe_ofs))
      vmerror_wrong_type(maybe_ofs);

    long ofs = get_c_long(maybe_ofs);

    if (ofs < 0)
      vmerror("String offsets cannot be <0: ~s", maybe_ofs);

    return (size_t)ofs;
  }

  LRef lstring_search(LRef tok, LRef str, LRef maybe_initial_ofs) // REVISIT: to Knuth-Morris-Pratt
  {
    if (!STRINGP(tok) && !CHARP(tok))
      vmerror_wrong_type(1, tok);
    if (!STRINGP(str))
      vmerror_wrong_type(2, str);

    if (CHARP(tok))
      tok = strcons(CHARV(tok));

    _TCHAR *tok_data = STRING_DATA(tok);
    _TCHAR *str_data = STRING_DATA(str);

    size_t str_loc = get_string_offset(maybe_initial_ofs);

    while(str_loc < STRING_DIM(str))
      {
        size_t tok_loc = 0;

        while (   (tok_data[tok_loc] == str_data[str_loc + tok_loc])
                  && (tok_loc < STRING_DIM(tok))
                  && (str_loc + tok_loc < STRING_DIM(str)))
          tok_loc++;

        if (tok_loc == STRING_DIM(tok))
          return fixcons(str_loc);

        str_loc++;
      }

    return boolcons(false);
  }

  LRef lstring_search_from_right(LRef tok, LRef str, LRef maybe_from)
  {

    if (!STRINGP(tok) && !CHARP(tok))
      vmerror_wrong_type(1, tok);
    if (!STRINGP(str))
      vmerror_wrong_type(2, str);

    if (CHARP(tok))
      tok = strcons(CHARV(tok));

    _TCHAR *tok_data = STRING_DATA(tok);
    _TCHAR *str_data = STRING_DATA(str);

    size_t str_loc = STRING_DIM(str) - 1;

    if (!NULLP(maybe_from)) str_loc = get_c_long(maybe_from);

    while((str_loc >= 0) && (str_loc < STRING_DIM(str)))
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

  LRef lstring_fold(LRef kons, LRef knil, LRef str)
  {
    if (!PROCEDUREP(kons))
      vmerror_wrong_type(1, kons);
    if (!STRINGP (str ))
      vmerror_wrong_type(3, str );

    LRef result = knil;

    for(size_t str_loc = 0; str_loc < STRING_DIM(str); str_loc++)
      result = lfuncall2(kons, charcons(STRING_DATA(str)[str_loc]), result);

    return result;
  }

  /* string-trim ************************************************
   * string-trim-left
   * string-trim-right */

  LRef lstring_trim(LRef str, LRef tc)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    const _TCHAR *trim_chars = _T(" \t\r\n");

    if (!NULLP(tc))
      {
	if (!STRINGP(tc))
          vmerror_wrong_type(2, str);

	trim_chars = get_c_string(tc);
      }


    size_t start = 0;
    size_t end = STRING_DIM(str);

    while (  (start < STRING_DIM(str))
	     && strchr(trim_chars, STRING_DATA(str)[start]))
      start++;

    while (  (end > start)
	     && strchr(trim_chars, STRING_DATA(str)[end - 1]))
      end--;

    return strcons(end - start, &(STRING_DATA(str)[start]));
  }

  LRef lstring_trim_left (LRef str, LRef tc)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    const _TCHAR *trim_chars = _T(" \t\r\n");

    if (!NULLP(tc))
      {
	if (!STRINGP(tc))
          vmerror_wrong_type(2, str);

	trim_chars = get_c_string(tc);
      }

    size_t start = 0;

    while (  (start < STRING_DIM(str))
	     && strchr(trim_chars, STRING_DATA(str)[start]))
      start++;

    return strcons(STRING_DIM(str) - start, &(STRING_DATA(str)[start]));
  }

  LRef lstring_trim_right (LRef str, LRef tc)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    const _TCHAR *trim_chars = _T(" \t\r\n");

    if (!NULLP(tc))
      {
	if (!STRINGP(tc))
          vmerror_wrong_type(2, str);

	trim_chars = get_c_string(tc);
      }

    size_t end = STRING_DIM(str);

    while (  (end > 0)
	     && strchr(trim_chars, STRING_DATA(str)[end - 1]))
      end--;

    return strcons(end, STRING_DATA(str));
  }


  /* string-upcase **********************************************
   * string-downcase */

  LRef lstring_upcased(LRef str)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    for (size_t loc = 0; loc < STRING_DIM(str); loc++)
      if (_istlower(STRING_DATA(str)[loc]))
	STRING_DATA(str)[loc] = (_TCHAR)_totupper(STRING_DATA(str)[loc]);

    return str;
  }

  LRef lstring_upcase(LRef str)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    return lstring_upcased(strcons(str));
  }

  LRef lstring_downcased(LRef str)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    for (size_t loc = 0; loc < STRING_DIM(str); loc++)
      if (_istupper(STRING_DATA(str)[loc]))
	STRING_DATA(str)[loc] = (_TCHAR)_totlower(STRING_DATA(str)[loc]);

    return str;
  }

  LRef lstring_downcase(LRef str)
  {
    if (!STRINGP(str))
      vmerror_wrong_type(1, str);

    return lstring_downcased(strcons(str));
  }



  LRef lnumber2string(LRef x, LRef r, LRef s, LRef p)
  {
    _TCHAR buffer[STACK_STRBUF_LEN];
    fixnum_t radix = 10;
    bool signedp = false;

    if (!NULLP(r))
      {
        if (FIXNUMP(r))
          radix = get_c_fixnum(r);
        else
          vmerror_wrong_type(2, r);
      }

    if (!NULLP(s))
      {
        if (BOOLP(s))
          signedp = BOOLV(s);
        else
          vmerror_wrong_type(3, s);
      }

    int digits = DEBUG_FLONUM_PRINT_PRECISION;

    if (!NULLP(p))
      {
        if (!FIXNUMP(p))
          vmerror(_T("exact number expected for precision"), p);

        digits = (int)get_c_fixnum(p);

        if ((digits < 0) || (digits > 16))
          vmerror(_T("print precision out of range [0, 16]"), p);
      }

    if (FLONUMP(x))
      {
        if (radix != 10)
          vmerror("inexact numbers require a radix of 10 in number->string", r);

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
          _sntprintf (buffer, STACK_STRBUF_LEN,  _T("%.*e"), digits, FLONM (x));
        else
          {
            /* Prevent numbers on the left of the decimal point from
             * adding to the number of digits we print. */
            if ((scale > 0) && (scale <= digits))
              digits -= (int)scale;

            _sntprintf(buffer, STACK_STRBUF_LEN, _T("%.*f"), digits, FLONM (x));
          }

	return strcons( buffer);
      }
    else if (FIXNUMP(x))
      {
	switch(radix) {
	case 10:
	  _sntprintf(buffer, STACK_STRBUF_LEN, signedp ? _T(FIXNUM_PRINTF_PREFIX "i") : _T(FIXNUM_PRINTF_PREFIX "u"), FIXNM(x));

	  return strcons( buffer);
	  break;

	case 16:
	  if (signedp) {
	    if (FIXNM(x) < 0)
	      _sntprintf(buffer, STACK_STRBUF_LEN, _T("-" FIXNUM_PRINTF_PREFIX "x"), -FIXNM(x));
	    else
	      _sntprintf(buffer, STACK_STRBUF_LEN, _T(FIXNUM_PRINTF_PREFIX "x"), FIXNM(x));
	  } else
	    _sntprintf(buffer, STACK_STRBUF_LEN, _T(FIXNUM_PRINTF_PREFIX "x"), FIXNM(x));
	  return strcons( buffer);
	  break;

	case 8:
	  if (signedp) {
	    if (FIXNM(x) < 0)
	      _sntprintf(buffer, STACK_STRBUF_LEN, _T("-" FIXNUM_PRINTF_PREFIX "o"), -FIXNM(x));
	    else
	      _sntprintf(buffer, STACK_STRBUF_LEN, _T(FIXNUM_PRINTF_PREFIX "o"), FIXNM(x));
	  } else
	    _sntprintf(buffer, STACK_STRBUF_LEN, _T(FIXNUM_PRINTF_PREFIX "o"), FIXNM(x));
	  return strcons( buffer);
	  break;

	default:
	  /* REVISIT: Implement alternate radixes in number->string */
	  /* REVISIT: Implement precision/width in number->string */
	  vmerror(_T("unimplemented radix in number->string"), r);
	  break;
	}
      }
    else
      vmerror_wrong_type(1, x);

    return NIL;
  }

  bool parse_string_as_fixnum(_TCHAR *string, int radix, fixnum_t &result)
  {
    bool overflow = false;

    _TCHAR *endobj = NULL;

    assert((radix >= 2) && (radix <= 36));


#ifdef FIXNUM_64BIT
    result = strtoll(string, &endobj, radix);

    if (((result == I64_MIN) || (result == I64_MAX)) && (errno == ERANGE)) // REVISIT: errno causes problems with the _link_
      overflow = true;
#else
    result = strtol(string, &endobj, radix);

    if (((result == LONG_MIN) || (result == LONG_MAX)) && (errno == ERANGE)) // REVISIT: errno causes problems with the _link_
      overflow = true;
#endif

    if (   overflow
	   || (endobj == NULL)
           || (endobj == string)
	   || (*endobj != _T('\0')))
      return false;
    else
      return true;
  }

  LRef lstring2number (LRef s, LRef r)
  {
    _TCHAR *string, *endobj;
    long radix = 10;
    bool radix_specified = false;
    fixnum_t fix_result = 0;
    flonum_t flo_result = 0;

    if (!STRINGP(s))
      vmerror_wrong_type(1, s);

    if (!NULLP(r))
      {
	if (!FIXNUMP(r))
          vmerror_wrong_type(2, r);

	radix_specified = true;

	radix = (long)get_c_fixnum(r);
      }

    if ((radix > 36) || (radix < 2))
      vmerror("Invalid radix for string->number, expected [2, 36]", r);

    string = get_c_string (s);

    /* string->number doesn't do any sort of 'nice' processing
     * of the string it accepts. In other words, the string must
     * only contain a valid number to be parsed. No spaces or
     * anything else is tolerated in the parse. */
    if (parse_string_as_fixnum(string, radix, fix_result))
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

  LRef lisp_strcmp (LRef s1, LRef s2)
  {
    size_t loc;

    if (!STRINGP(s1))
      vmerror_wrong_type(1, s1);
    if (!STRINGP(s2))
      vmerror_wrong_type(2, s2);

    for(loc = 0; (loc < STRING_DIM(s1)) && (loc < STRING_DIM(s2)) ; loc++)
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

  LRef lstringp (LRef x)
  {
    if (STRINGP(x))
      return x;
    else
      return boolcons(false);
  }

  LRef lstring_length(LRef string)
  {
    return fixcons(STRING_DIM(string));
  }


  LRef lstring_first_char(LRef string, LRef char_set, LRef maybe_initial_ofs)
  {
    // REVISIT: string-first-char should accept character/lists as char_set
    // REVISIT: string-first-char should take args in same order as string-search

    if (!STRINGP(string))
      vmerror_wrong_type(1, string);
    if (!STRINGP(char_set))
      vmerror_wrong_type(2, char_set);

    size_t loc = get_string_offset(maybe_initial_ofs);

    _TCHAR *str   = STRING_DATA(string  );
    _TCHAR *chars = STRING_DATA(char_set);

    for(; loc < STRING_DIM(string); loc++)
      {
        for(size_t current_char = 0; current_char < STRING_DIM(char_set); current_char++)
          {
            if (chars[current_char] == str[loc])
              return fixcons(loc);
          }
      }

    return boolcons(false);
  }

  LRef lstring_first_substring(LRef string, LRef char_set, LRef maybe_initial_ofs)
  {
    // REVISIT: string-first-substring should accept character/lists as char_set
    // REVISIT: string-first-string should take args in same order as string-search

    if (!STRINGP(string))
      vmerror_wrong_type(1, string);
    if (!STRINGP(char_set))
      vmerror_wrong_type(2, char_set);

    size_t loc = get_string_offset(maybe_initial_ofs);

    _TCHAR *str   = STRING_DATA(string  );
    _TCHAR *chars = STRING_DATA(char_set);

    size_t substring_length  = 0;

    for(; loc < STRING_DIM(string); loc++)
      {
        bool char_found = false;

        for(size_t current_char = 0; current_char < STRING_DIM(char_set); current_char++)
          {
            if (chars[current_char] == str[loc])
              {
                substring_length++;
                char_found = true;
                break;
              }
          }

        if (!char_found)
          break;
      }

    if (substring_length == 0)
      return boolcons(false);
    else
      return fixcons(loc);
  }

  LRef lstring_copy(LRef string)
  {
    if (!STRINGP(string))
      vmerror_wrong_type(1, string);

    return strcons(string);
  }

  LRef lcharacter2string(LRef obj)
  {
    if (!CHARP(obj))
      vmerror_wrong_type(1, obj);

    _TCHAR buf = CHARV(obj);

    return strcons(1, &buf);
  }


  int str_next_character(LRef obj)
  {
    size_t ofs;
    int ch;

    assert(STRINGP(obj));

    ofs = STRING_OFS(obj);
    ch = STRING_DATA(obj)[ofs];

    if (ofs >= STRING_DIM(obj))
      return EOF;

    SET_STRING_OFS(obj, STRING_OFS(obj) + 1);

    return ch;
  }

  void str_append_str(LRef obj, _TCHAR *str, size_t len)
  {
    assert(STRINGP(obj));

    size_t size = STRING_DIM(obj);

    strrecons(obj, size + len);

    memcpy(&(STRING_DATA(obj)[size]), str, len);
    STRING_DATA(obj)[size + len + 1] = _T('\0');
  }

  /**************************************************************
   * C string access
   */

  _TCHAR *try_get_c_string (LRef x)
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

  _TCHAR *get_c_string (LRef x)
  {
    _TCHAR *str = try_get_c_string(x);

    if (str)
      return (str);
    else
      vmerror("not a symbol or string", x);

    return (NULL);
  }

  _TCHAR *get_c_string_dim (LRef x, size_t &len)
  {
    _TCHAR *rc = get_c_string(x);

    len = _tcslen(rc);

    return rc;
  }

  typedef enum {
    NO_SEPERATOR,
    US_SEPERATOR,
    EURO_SEPERATOR
  } float_seperator;

  size_t float_format(_TCHAR *buf, size_t buf_len,
		      double nd,
		      int sigfigs,
		      bool round,
		      bool scientific,
		      float_seperator sep)
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
	double int_part; // Unused
	double frac_part;

        double adjust_amount = 0.5;

	frac_part = modf(nd, &int_part);

        for(places = 0; places < sigfigs; places++)
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
    int   ecvt_decimal;         /* position of decimal point */
    int   sign;                 /* sign of number (1 = - ) */
    _TCHAR *ecvt_result = NULL; /* result text */
    int   precision = 16;       /* Input precision */

    /* working variables */
    int  decimal;               /* count of numerals to the decimal */
    _TCHAR *result_loc = buf;   /* location in result buffer */
    bool first = true;          /* True on the first numeric character */

    // TODO: result_loc should be range checked against buf_len

    /* Most of the interesting numerical work is done by _ecvt. _ecvt
     * gives us most of the information we need to print the number. */

    ecvt_result = ecvt( nd, precision, &ecvt_decimal, &sign );

    /* _ecvt formally leaves the return value of ecvt_decimal
     * undefined (either 0 or 1) if nd==0.0. Since we assume
     * it to be 1 in that case, we fix it up here. */
    if (nd == 0.0)
      ecvt_decimal = 1;

    /* We 'fail' over into scientific notation if we're printing a number
     * either too large for our precision or too small for the requested
     * number of significant digits. We also allow for the case the caller
     * has explicitly requested scientific notation. */
    if (   (ecvt_decimal >= precision)
	   || (ecvt_decimal <= -sigfigs)
	   || scientific)
      {
	decimal = 1;
	scientific = true;
      }
    else
      decimal  = ecvt_decimal;


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
	    switch(sep)
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


    switch(sep)
      {
      case NO_SEPERATOR:
      case US_SEPERATOR:
	*result_loc++ = '.';
	break;

      case EURO_SEPERATOR:
	*result_loc++ = ',';
	break;
      }

    while (   (decimal < 0)
	      && (sigfigs > 0))
      {
	*result_loc++ = '0';
	decimal ++;
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

  LRef linexact2display_string(LRef n, LRef sf, LRef sci, LRef s)
  {
    _TCHAR buf[STACK_STRBUF_LEN];
    float_seperator sep = NO_SEPERATOR;

    if (!NUMBERP(n))
      vmerror_wrong_type(1, n );
    if (!FIXNUMP(sf))
      vmerror_wrong_type(2, sf);
    if (!BOOLP(sci))
      vmerror_wrong_type(3, sci);
    if (!SYMBOLP(s))
      vmerror_wrong_type(4, s );

    if (FIXNM(sf) < 0)
      vmerror("Invalid significant figure count to inexact->display-string", sf);

    if (keyword_intern(_T("none")) == s)
      sep = NO_SEPERATOR;
    else if (keyword_intern(_T("us")) == s)
      sep = US_SEPERATOR;
    else if (keyword_intern(_T("euro")) == s)
      sep = EURO_SEPERATOR;
    else
      vmerror("Invalid seperator description to inexact->display-string", s);

    float_format(buf, STACK_STRBUF_LEN, get_c_double(n), (int)get_c_fixnum(sf), true, BOOLV(sci), sep);

    return strcons(buf);
  }

} // end namespace scan
