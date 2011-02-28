
/*
 * macro.cpp --
 *
 * The primitive macro type. All of the macro logic is in the compiler,
 * with the internal type serving as a marker only.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)
LRef macrocons(LRef t)          /*  REVISIT: Macros should be moved entirely to Lisp... */
{
     LRef z = new_cell(TC_MACRO);

     SET_MACRO_TRANSFORMER(z, t);

     return (z);
}

LRef limacrocons(LRef t)
{
     if (!CLOSUREP(t))
          vmerror_wrong_type(1, t);

     return macrocons(t);
}

LRef lmacro_transformer(LRef mac)
{
     if (!MACROP(mac))
          vmerror_wrong_type(1, mac);

     return MACRO_TRANSFORMER(mac);
}

LRef lmacrop(LRef obj)
{
     return MACROP(obj) ? obj : boolcons(false);
}

END_NAMESPACE
