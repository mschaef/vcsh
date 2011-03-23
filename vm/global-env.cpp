
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

#include "scan-private.h"

BEGIN_NAMESPACE(scan)

lref_t lsymbol_vcell(lref_t sym)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);

     return SYMBOL_VCELL(sym);
}

lref_t lset_symbol_vcell(lref_t sym, lref_t val)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);

     SET_SYMBOL_VCELL(sym, val);

     return sym;
}

lref_t lunbound_marker()
{
     return UNBOUND_MARKER;
}

lref_t lisymbol_globally_boundp(lref_t sym)
{
     assert(SYMBOLP(sym));

     return boolcons(SYMBOL_VCELL(sym) != UNBOUND_MARKER);
}

lref_t lidefine_global(lref_t var, lref_t val)
{
     assert(SYMBOLP(var));

     dscwritef(DF_SHOW_GLOBAL_DEFINES, (_T("; DEBUG: globally defining ~a\n"), var));

     SET_SYMBOL_VCELL(var, val);

     vmtrap(TRAP_DEFINE, VMT_OPTIONAL_TRAP, 2, var, val);

     return val;
}


END_NAMESPACE
