/* slib_mac.c
 *
 * scan Common Lisp macro code
 */

#include "scan.h"

namespace scan {

  /**************************************************************
   * Macro type implementation
   */

  LRef macrocons (LRef t) // TODO: Macros should be moved entirely to Lisp...
  {
    LRef z = new_cell(TC_MACRO);

    SET_MACRO_TRANSFORMER(z, t);

    return (z);
  }

  LRef apply_macro(LRef macro, LRef form)
  {
    assert(CONSP(form));
    assert(MACROP(macro));

    return napply(MACRO_TRANSFORMER(macro), 3, form, NIL, NIL);
  }

  LRef lapply_macro(LRef macro, LRef form)
  {
    if (!CONSP(form))
      vmerror_wrong_type(2, form);
    if (!MACROP(macro))
      vmerror_wrong_type(1, macro);

    return apply_macro(macro, form);
  }

  LRef macroexpand(LRef form, LRef env, bool rewrite_form, bool more_than_once)
  {
    LRef macro_sym;
    LRef macro;

    bool rewritten;

    do {
      rewritten = false;

      if (!CONSP(form)) break;

      macro_sym = lcar(form);

      if (!SYMBOLP(macro_sym)) break;

      LRef local_binding = NULLP(env) ? NIL : lenvlookup(macro_sym, env);

      if (NULLP(local_binding))
        macro = SYMBOL_VCELL(macro_sym);
      else
        {
          assert(CONSP(local_binding));

          if (UNBOUND_MARKER_P(CAR(local_binding)))
            vmerror_unbound(macro_sym);

          macro = CAR(local_binding);
        }

      if (MACROP(macro))
        {
          LRef expanded = apply_macro(macro, form);

          if (DEBUG_FLAG(DF_SHOW_VM_MACROEXPANDS))
            scwritef(_T("; DEBUG: vm macroexpansion ~s\n; DEBUG   => ~s\n"),
                     CURRENT_DEBUG_PORT, form, expanded);

          rewritten = true;

          if (CONSP(expanded) && CONSP(form) && rewrite_form)
            {
              SET_CAR(form, CAR(expanded));
              SET_CDR(form, CDR(expanded));
            }
          else
            form = expanded;
        }

    } while (rewritten && more_than_once);

    return form;
  }


  LRef lmacroexpand(LRef form, LRef env)    { return macroexpand(form, env, false, true ); }
  LRef lmacroexpand_1(LRef form, LRef env)  { return macroexpand(form, env, false, false); }
  LRef lmacroexpandn(LRef form, LRef env)   { return macroexpand(form, env, true,  true ); }
  LRef lmacroexpand_1n(LRef form, LRef env) { return macroexpand(form, env, true,  false); }

  LRef limacro(LRef t)
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

} // end namespace scan
