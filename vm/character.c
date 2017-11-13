/*
 * character.c --
 *
 * Character data.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

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
