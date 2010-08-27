/* fasl-utils.cpp
 * June 26th, 2007
 *
 * A few FASL-file related utility functions
 */

#include "scan.h"
#include "fasl-file.h"

namespace scan {

const _TCHAR *fasl_opcode_name(FaslOpcode opcode)
{
     /*  Returns NULL for invalid opcodes */

    switch(opcode)
    {
    case FASL_OP_NIL:               return _T("FASL_OP_NIL");               break;
    case FASL_OP_TRUE:              return _T("FASL_OP_TRUE");              break;
    case FASL_OP_FALSE:             return _T("FASL_OP_FALSE");             break;
    case FASL_OP_CHARACTER:         return _T("FASL_OP_CHARACTER");         break;
    case FASL_OP_LIST:              return _T("FASL_OP_LIST");              break;
    case FASL_OP_LISTD:             return _T("FASL_OP_LISTD");             break;
    case FASL_OP_NOP_1:             return _T("FASL_OP_NOP_1");             break;
    case FASL_OP_NOP_2:             return _T("FASL_OP_NOP_2");             break;
    case FASL_OP_FIX8:              return _T("FASL_OP_FIX8");              break;
    case FASL_OP_FIX16:             return _T("FASL_OP_FIX16");             break;
    case FASL_OP_FIX32:             return _T("FASL_OP_FIX32");             break;
    case FASL_OP_FIX64:             return _T("FASL_OP_FIX64");             break;
    case FASL_OP_FLOAT:             return _T("FASL_OP_FLOAT");             break;
    case FASL_OP_COMPLEX:           return _T("FASL_OP_COMPLEX");           break;
    case FASL_OP_STRING:            return _T("FASL_OP_STRING");            break;
    case FASL_OP_NOP_3:             return _T("FASL_OP_NOP_3");             break;
    case FASL_OP_PACKAGE:           return _T("FASL_OP_PACKAGE");           break;
    case FASL_OP_VECTOR:            return _T("FASL_OP_VECTOR");            break;
    case FASL_OP_INSTANCE:          return _T("FASL_OP_INSTANCE");          break;
    case FASL_OP_HASH:              return _T("FASL_OP_HASH");              break;
    case FASL_OP_CLOSURE:           return _T("FASL_OP_CLOSURE");           break;
    case FASL_OP_MACRO:             return _T("FASL_OP_MACRO");             break;
    case FASL_OP_SYMBOL:            return _T("FASL_OP_SYMBOL");            break;
    case FASL_OP_SUBR:              return _T("FASL_OP_SUBR");              break;
    case FASL_OP_STRUCTURE:         return _T("FASL_OP_STRUCTURE");         break;
    case FASL_OP_STRUCTURE_LAYOUT:  return _T("FASL_OP_STRUCTURE_LAYOUT");  break;
    case FASL_OP_FAST_OP_0:         return _T("FASL_OP_FAST_OP_0");         break;
    case FASL_OP_FAST_OP_1:         return _T("FASL_OP_FAST_OP_1");         break;
    case FASL_OP_FAST_OP_2:         return _T("FASL_OP_FAST_OP_2");         break;
    case FASL_OP_FAST_OP_3:         return _T("FASL_OP_FAST_OP_3");         break;
    case FASL_OP_INSTANCE_MAP:      return _T("FASL_OP_INSTANCE_MAP");      break;
    case FASL_OP_RESET_READER_DEFS: return _T("FASL_OP_RESET_READER_DEFS"); break;
    case FASL_OP_READER_DEFINITION: return _T("FASL_OP_READER_DEFINITION"); break;
    case FASL_OP_READER_REFERENCE:  return _T("FASL_OP_READER_REFERENCE");  break;
    case FASL_OP_LOADER_DEFINEQ:    return _T("FASL_OP_LOADER_DEFINEQ");    break;
    case FASL_OP_LOADER_DEFINEA0:   return _T("FASL_OP_LOADER_DEFINEA0");   break;
    case FASL_OP_LOADER_APPLY0:     return _T("FASL_OP_LOADER_APPLY0");     break;
    case FASL_OP_BEGIN_LOAD_UNIT:   return _T("FASL_OP_BEGIN_LOAD_UNIT");   break;
    case FASL_OP_END_LOAD_UNIT:     return _T("FASL_OP_END_LOAD_UNIT");     break;
    case FASL_OP_EOF:               return _T("FASL_OP_EOF");               break;
    default:                        return NULL;                            break;
    }
}

} /*  namespace scan */
