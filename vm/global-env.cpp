/*
 * The global environment.
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
