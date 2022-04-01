/*
 * macro.c --
 *
 * The primitive macro type. All of the macro logic is in the compiler,
 * with the internal type serving as a marker only.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

/*  REVISIT: Macros should be moved entirely to Lisp... */

lref_t macrocons(lref_t transformer)
{
     lref_t macro = new_cell(TC_MACRO);

     macro->as.macro.transformer = transformer;

     return macro;
}

lref_t limacrocons(lref_t transformer)
{
     if (!CLOSUREP(transformer))
          vmerror_wrong_type_n(1, transformer);

     return macrocons(transformer);
}

lref_t lmacro_transformer(lref_t macro)
{
     if (!MACROP(macro))
          vmerror_wrong_type_n(1, macro);

     return macro->as.macro.transformer;
}

lref_t lmacrop(lref_t obj)
{
     return MACROP(obj) ? obj : boolcons(false);
}

