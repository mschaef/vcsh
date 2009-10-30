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

} // end namespace scan
