/*
 * string.c --
 *
 * Strings.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#include "scan-private.h"

bool string_equal(lref_t a, lref_t b)
{
     size_t len;

     assert(TYPE(a) == TC_STRING);

     len = a->as.string.dim;

     if (len != b->as.string.dim)
          return false;

     if (memcmp(a->as.string.data, b->as.string.data, len) == 0)
          return true;
     else
          return false;
}

static size_t string_storage_size(size_t str_size)
{
     size_t buf_size = 16;

     while (buf_size <= str_size)
     {
          buf_size <<= 1;
     }

     return buf_size;
}


lref_t strrecons(lref_t obj, size_t new_length)
{
     size_t space_needed;
     size_t space_already_allocated = 0;

     space_needed = string_storage_size(new_length + 1);

     if (obj->as.string.data != NULL)
          space_already_allocated = string_storage_size(obj->as.string.dim);

     if (space_already_allocated != space_needed) {
          _TCHAR *new_buffer = (_TCHAR *)gc_malloc(space_needed);

          memset(new_buffer, 0, space_needed);

          if (obj->as.string.data) {
               memcpy(new_buffer, obj->as.string.data, obj->as.string.dim + 1);

               gc_free(obj->as.string.data);
          }

          obj->as.string.data = new_buffer;
     }

     obj->as.string.dim = new_length;

     return obj;
}

static void allocate_string_buffer(lref_t obj, size_t length)
{
     obj->as.string.data = NULL;
     obj->as.string.dim = 0;

     strrecons(obj, length);
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

     retval->as.string.data[len] = trailing;

     return retval;
}

lref_t strconsdup(lref_t str)
{
     assert(STRINGP(str));

     return strconsbufn(str->as.string.dim, str->as.string.data);
}

lref_t strconsbufn(size_t length, const _TCHAR * buffer)
{
     lref_t new_string = new_cell(TC_STRING);

     allocate_string_buffer(new_string, length);

     if (buffer) {
          memcpy(new_string->as.string.data, buffer, length);
     }

     return new_string;
}

lref_t lstring_ref(lref_t a, lref_t i)
{
     if (!STRINGP(a))
          vmerror_wrong_type_n(1, a);

     if (!FIXNUMP(i))
          vmerror_wrong_type_n(2, i);

     fixnum_t k = get_c_fixnum(i);

     if ((k < 0) || ((size_t) k >= a->as.string.dim))
          vmerror_index_out_of_bounds(i, a);

     return charcons(a->as.string.data[k]);
}

lref_t lstring_set(lref_t a, lref_t i, lref_t v)
{
     if (!STRINGP(a))
          vmerror_wrong_type_n(1, a);

     if (!FIXNUMP(i))
          vmerror_wrong_type_n(2, i);

     fixnum_t k = get_c_fixnum(i);

     if ((k < 0) || ((size_t) k >= a->as.string.dim))
          vmerror_index_out_of_bounds(i, a);

     if (FIXNUMP(v)) {
          a->as.string.data[k] = (_TCHAR)FIXNM(v);
     } else if (CHARP(v)) {
          a->as.string.data[k] = CHARV(v);
     } else {
          vmerror_wrong_type_n(3, v);
     }

     return (a);
}

lref_t lstring_append(size_t argc, lref_t argv[])
{
     fixnum_t size = 0;
     lref_t current_string;

     for (size_t ii = 0; ii < argc; ii++) {
          current_string = argv[ii];

          if (SYMBOLP(current_string))
               current_string = SYMBOL_PNAME(current_string);

          if (STRINGP(current_string)) {
               size += current_string->as.string.dim;
          } else if (CHARP(current_string)) {
               size += 1;
          } else if (FIXNUMP(current_string)) {
               if ((FIXNM(current_string) < 0x00) || (FIXNM(current_string) > 0xFF))
                    vmerror_arg_out_of_range(current_string, _T("[0,255]"));
               else
                    size += 1;
          } else
               vmerror_wrong_type_n(ii, argv[ii]);
     }

     lref_t s = strconsbufn((size_t) size, NULL);

     _TCHAR *data = s->as.string.data;
     size_t pos = 0;

     for (size_t ii = 0; ii < argc; ii++)
     {
          current_string = argv[ii];

          if (SYMBOLP(current_string))
               current_string = SYMBOL_PNAME(current_string);

          if (STRINGP(current_string)) {
               memcpy(data + pos, current_string->as.string.data, current_string->as.string.dim);
               pos += current_string->as.string.dim;
          } else if (CHARP(current_string)) {
               data[pos] = CHARV(current_string);
               pos += 1;
          } else if (FIXNUMP(current_string)) {
               data[pos] = (_TCHAR)get_c_fixnum(current_string);
               pos += 1;
          } else
               panic("Unexpected string type in string-append concatenation phase.");
     }

     return (s);
}

lref_t lsubstring(lref_t str, lref_t start, lref_t end)
{
     size_t s, e;

     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     s = get_c_long(start);
     e = NULLP(end) ? str->as.string.dim : get_c_long(end);

     if (s > str->as.string.dim)
          vmerror_index_out_of_bounds(start, str);

     if (e > str->as.string.dim)
          vmerror_index_out_of_bounds(end, str);

     if (s > e)
          vmerror_arg_out_of_range(start, _T("start<=end"));

     return strconsbufn(e - s, &(str->as.string.data[s]));
}

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

     _TCHAR *tok_data = tok->as.string.data;
     _TCHAR *str_data = str->as.string.data;

     size_t str_loc = get_string_offset(maybe_initial_ofs);

     while (str_loc < str->as.string.dim) {
          size_t tok_loc = 0;

          while ((tok_data[tok_loc] == str_data[str_loc + tok_loc])
                 && (tok_loc < tok->as.string.dim)
                 && (str_loc + tok_loc < str->as.string.dim))
               tok_loc++;

          if (tok_loc == tok->as.string.dim)
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

     _TCHAR *tok_data = tok->as.string.data;
     _TCHAR *str_data = str->as.string.data;

     size_t str_loc = str->as.string.dim - 1;

     if (!NULLP(maybe_from))
          str_loc = get_c_long(maybe_from);

     while (str_loc < str->as.string.dim) {
          size_t tok_loc = tok->as.string.dim - 1;

          while (tok_data[tok_loc] == str_data[str_loc - (tok->as.string.dim - tok_loc - 1)])
          {
               if (tok_loc == 0)
                    return fixcons(str_loc - (tok->as.string.dim - 1));

               if ((str_loc - (tok->as.string.dim - tok_loc - 1)) == 0)
                    break;

               tok_loc--;
          }

          str_loc--;
     }

     return boolcons(false);
}

lref_t lstring_trim(lref_t str, lref_t tc)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     const _TCHAR *trim_chars = _T(" \t\r\n");
     _TCHAR buffer[STACK_STRBUF_LEN];

     if (!NULLP(tc)) {
          if(get_c_string(tc, STACK_STRBUF_LEN, buffer) < 0)
               vmerror_arg_out_of_range(tc, _T("trim character specification too long"));

          trim_chars = buffer;
     }


     size_t start = 0;
     size_t end = str->as.string.dim;

     while ((start < str->as.string.dim) && strchr(trim_chars, str->as.string.data[start]))
          start++;

     while ((end > start) && strchr(trim_chars, str->as.string.data[end - 1]))
          end--;

     return strconsbufn(end - start, &(str->as.string.data[start]));
}

lref_t lstring_trim_left(lref_t str, lref_t tc)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     const _TCHAR *trim_chars = _T(" \t\r\n");
     _TCHAR buffer[STACK_STRBUF_LEN];

     if (!NULLP(tc)) {
          if(get_c_string(tc, STACK_STRBUF_LEN, buffer) < 0)
               vmerror_arg_out_of_range(tc, _T("trim character specification too long"));

          trim_chars = buffer;
     }


     size_t start = 0;

     while ((start < str->as.string.dim) && strchr(trim_chars, str->as.string.data[start]))
          start++;

     return strconsbufn(str->as.string.dim - start, &(str->as.string.data[start]));
}

lref_t lstring_trim_right(lref_t str, lref_t tc)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     const _TCHAR *trim_chars = _T(" \t\r\n");
     _TCHAR buffer[STACK_STRBUF_LEN];

     if (!NULLP(tc)) {
          if(get_c_string(tc, STACK_STRBUF_LEN, buffer) < 0)
               vmerror_arg_out_of_range(tc, _T("trim character specification too long"));

          trim_chars = buffer;
     }

     size_t end = str->as.string.dim;

     while ((end > 0) && strchr(trim_chars, str->as.string.data[end - 1]))
          end--;

     return strconsbufn(end, str->as.string.data);
}

lref_t lstring_upcased(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     for (size_t loc = 0; loc < str->as.string.dim; loc++) {
          if (_istlower(str->as.string.data[loc])) {
               str->as.string.data[loc] = (_TCHAR)_totupper(str->as.string.data[loc]);
          }
     }

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

     for (size_t loc = 0; loc < str->as.string.dim; loc++)
          if (_istupper(str->as.string.data[loc]))
               str->as.string.data[loc] = (_TCHAR) _totlower(str->as.string.data[loc]);

     return str;
}

lref_t lstring_downcase(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     return lstring_downcased(strconsdup(str));
}

lref_t lisp_strcmp(lref_t string_1, lref_t string_2)
{
     size_t loc;

     if (!STRINGP(string_1))
          vmerror_wrong_type_n(1, string_1);
     if (!STRINGP(string_2))
          vmerror_wrong_type_n(2, string_2);

     for (loc = 0; (loc < string_1->as.string.dim) && (loc < string_2->as.string.dim); loc++)
     {
          _TCHAR char_1 = string_1->as.string.data[loc];
          _TCHAR char_2 = string_2->as.string.data[loc];

          if (char_1 > char_2)
               return fixcons(1);
          else if (char_1 < char_2)
               return fixcons(-1);
     }

     if (loc < string_1->as.string.dim)
          return fixcons(1);
     else if (loc < string_2->as.string.dim)
          return fixcons(-1);

     return fixcons(0);
}

lref_t lisp_stricmp(lref_t string_1, lref_t string_2)
{
     size_t loc;

     if (!STRINGP(string_1))
          vmerror_wrong_type_n(1, string_1);
     if (!STRINGP(string_2))
          vmerror_wrong_type_n(2, string_2);

     for (loc = 0; (loc < string_1->as.string.dim) && (loc < string_2->as.string.dim); loc++)
     {
          _TCHAR char_1 = string_1->as.string.data[loc];
          _TCHAR char_2 = string_2->as.string.data[loc];

          if (_istupper(char_1))
               char_1 = _totlower(char_1);
          if (_istupper(char_2))
               char_2 = _totlower(char_2);

          if (char_1 > char_2)
               return fixcons(1);
          else if (char_1 < char_2)
               return fixcons(-1);
     }

     if (loc < string_1->as.string.dim)
          return fixcons(1);
     else if (loc < string_2->as.string.dim)
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
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     return fixcons(string->as.string.dim);
}


lref_t lstring_first_char(lref_t string, lref_t char_set, lref_t maybe_initial_ofs)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     if (!VECTORP(char_set))
          vmerror_wrong_type_n(2, char_set);

     if (char_set->as.vector.dim != _TCHAR_MAX)
          vmerror_index_out_of_bounds(fixcons(_TCHAR_MAX - 1), char_set);

     size_t loc = get_string_offset(maybe_initial_ofs);
     _TCHAR *str = string->as.string.data;

     for (; loc < string->as.string.dim; loc++)
     {
          if (TRUEP(char_set->as.vector.data[(size_t)str[loc]]))
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

     if (char_set->as.vector.dim != _TCHAR_MAX)
          vmerror_index_out_of_bounds(fixcons(_TCHAR_MAX - 1), char_set);

     size_t substring_length = 0;
     size_t loc = get_string_offset(maybe_initial_ofs);
     _TCHAR *str = string->as.string.data;

     for (; loc < string->as.string.dim; loc++)
     {
          if (!TRUEP(char_set->as.vector.data[(size_t)str[loc]]))
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

     size_t size = str->as.string.dim;

     strrecons(str, size + len);

     memcpy(&(str->as.string.data[size]), buf, len);
     str->as.string.data[size + len + 1] = _T('\0');
}

int get_c_string(lref_t obj, size_t buflen, _TCHAR *buf)
{
     assert(buflen > 0);
     assert(buf);

     if (SYMBOLP(obj))
     {
          obj = SYMBOL_PNAME(obj);

          assert(STRINGP(obj));
     }

     if (!STRINGP(obj)) {
          vmerror_wrong_type(obj);
     }

     int n = MIN2(buflen - 1, obj->as.string.dim);

     memcpy(buf, obj->as.string.data, n);
     buf[n] = _T('\0');

     if (n < obj->as.string.dim)
          return -1;
     else
          return n;
}

