
/*
 * global-env.cpp --
 *
 * The global environment.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

LRef lsymbol_vcell(LRef sym)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);

     return SYMBOL_VCELL(sym);
}

LRef lset_symbol_vcell(LRef sym, LRef val)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);

     SET_SYMBOL_VCELL(sym, val);

     return sym;
}

LRef lunbound_marker()
{
     return UNBOUND_MARKER;
}

LRef lisymbol_globally_boundp(LRef sym)
{
     assert(SYMBOLP(sym));

     return boolcons(SYMBOL_VCELL(sym) != UNBOUND_MARKER);
}

LRef lidefine_global(LRef var, LRef val)
{
     assert(SYMBOLP(var));

     dscwritef(DF_SHOW_GLOBAL_DEFINES, (_T("; DEBUG: globally defining ~a\n"), var));

     SET_SYMBOL_VCELL(var, val);

     vmtrap(TRAP_DEFINE, VMT_OPTIONAL_TRAP, 2, var, val);

     return val;
}


END_NAMESPACE
