
/* external-data.cpp
 *
 * Facilities for managing references to data external to the Lisp
 * managed heap.
 */

#include <float.h>

#include "scan.h"

BEGIN_NAMESPACE(scan)
LRef externalcons(void *data, LRef desc, external_meta_t * meta /* = NULL */ )
{
     LRef z = new_cell(TC_EXTERNAL);

     assert((meta == NULL) || (meta->_name != NULL));

     SET_EXTERNAL_DATA(z, data);
     SET_EXTERNAL_DESC(z, desc);
     SET_EXTERNAL_META(z, meta);

     return z;
}

LRef lexternal_data(LRef x)
{
     if (!EXTERNALP(x))
          vmerror_wrong_type(1, x);

     return fixcons((uptr_t) EXTERNAL_DATA(x));
}

LRef lexternal_desc(LRef x)
{
     if (!EXTERNALP(x))
          vmerror_wrong_type(1, x);

     return EXTERNAL_DESC(x);
}

LRef lexternalp(LRef x)
{
     if (EXTERNALP(x))
          return x;
     else
          return boolcons(false);
}

LRef lexternal_type_name(LRef obj)
{
     if (!EXTERNALP(obj))
          return boolcons(false);

     if (EXTERNAL_META(obj))
          return strcons(_T(EXTERNAL_META(obj)->_name));

     return boolcons(true);
}

LRef lprint_external_details(LRef obj, LRef port)
{
     if (!EXTERNALP(obj))
          vmerror_wrong_type(1, obj);

     if (EXTERNAL_META(obj) && (EXTERNAL_META(obj)->_print_details))
          return EXTERNAL_META(obj)->_print_details(obj, port);

     return boolcons(false);
}

END_NAMESPACE
