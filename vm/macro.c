/*
 * macro.c --
 *
 * The primitive macro type. All of the macro logic is in the compiler,
 * with the internal type serving as a marker only.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

/*  REVISIT: Macros should be moved entirely to Lisp... */

lref_t macrocons(lref_t t)          
{
     lref_t z = new_cell(TC_MACRO);

     SET_MACRO_TRANSFORMER(z, t);

     return (z);
}

lref_t limacrocons(lref_t t)
{
     if (!CLOSUREP(t))
          vmerror_wrong_type_n(1, t);

     return macrocons(t);
}

lref_t lmacro_transformer(lref_t mac)
{
     if (!MACROP(mac))
          vmerror_wrong_type_n(1, mac);

     return MACRO_TRANSFORMER(mac);
}

lref_t lmacrop(lref_t obj)
{
     return MACROP(obj) ? obj : boolcons(false);
}

