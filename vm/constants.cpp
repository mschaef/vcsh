
/* constants.cpp
 *
 * The implementation of functions related to VM constants.
 */

#include "sys.h"

BEGIN_NAMESPACE(scan)

#define CONST_C_IMPL
#include "constants.i"
#undef CONST_C_IMPL

END_NAMESPACE
