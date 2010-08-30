
/* fasl-file.h
 * June 26th, 2007
 *
 * This is the FASL file format.
 */

#ifndef __FASL_FILE_H
#define __FASL_FILE_H

#include "scan.h"

BEGIN_NAMESPACE(scan)
enum FaslOpcode
{                               /* see fasl-write.scm for details about opcode assignment */
     FASL_OP_NIL = 1,
     FASL_OP_TRUE = 2,
     FASL_OP_FALSE = 3,
     FASL_OP_CHARACTER = 4,
     FASL_OP_LIST = 8,
     FASL_OP_LISTD = 9,
     FASL_OP_NOP_1 = 10,
     FASL_OP_NOP_2 = 13,
     FASL_OP_FIX8 = 16,
     FASL_OP_FIX16 = 17,
     FASL_OP_FIX32 = 18,
     FASL_OP_FIX64 = 19,
     FASL_OP_FLOAT = 21,
     FASL_OP_COMPLEX = 22,
     FASL_OP_STRING = 24,
     FASL_OP_NOP_3 = 26,
     FASL_OP_PACKAGE = 28,
     FASL_OP_VECTOR = 30,
     FASL_OP_INSTANCE = 33,
     FASL_OP_HASH = 34,
     FASL_OP_COMMENT_1 = 35,
     FASL_OP_CLOSURE = 36,
     FASL_OP_MACRO = 37,
     FASL_OP_SYMBOL = 48,
     FASL_OP_SUBR = 50,
     FASL_OP_COMMENT_2 = 59,
     FASL_OP_STRUCTURE = 60,
     FASL_OP_STRUCTURE_LAYOUT = 61,

     FASL_OP_FAST_OP_0 = 64,
     FASL_OP_FAST_OP_1 = 65,
     FASL_OP_FAST_OP_2 = 66,
     FASL_OP_FAST_OP_3 = 67,

     FASL_OP_INSTANCE_MAP = 96,

     FASL_OP_RESET_READER_DEFS = 192,
     FASL_OP_READER_DEFINITION = 193,
     FASL_OP_READER_REFERENCE = 194,
     FASL_OP_LOADER_DEFINEQ = 208,
     /*  209 is the former FASL_OP_LOADER_DEFINE (which invoked the evaluator to determine the definition value.) */
     FASL_OP_LOADER_DEFINEA0 = 210,
     FASL_OP_LOADER_APPLY0 = 216,

     FASL_OP_BEGIN_LOAD_UNIT = 224,
     FASL_OP_END_LOAD_UNIT = 225,

     FASL_OP_EOF = 253
         /*  254, 255 reserved for Unicode Byte Order Marker */
};


const _TCHAR *fasl_opcode_name(FaslOpcode opcode);

END_NAMESPACE
#endif                          /*  __FASL_FILE_H */
