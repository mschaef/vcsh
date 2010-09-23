
/*
 * constants.i
 * 
 * Constant tables. This file is pre-processed twice: once to get the
 * constants defined in the VM, and a second time to generate scheme
 * code that's incorporated into the scheme image.
 */

/* *INDENT-OFF* */

#ifdef CONST_SCHEME
#  define BEGIN_VM_CONSTANT_TABLE(table_name, name_fn_name)
#  define VM_CONSTANT(name, value) (define system::name value)
#  define END_VM_CONSTANT_TABLE
#endif

#ifdef CONST_C_HEADER
#  define BEGIN_VM_CONSTANT_TABLE(table_name, name_fn_name) enum table_name {
#  define VM_CONSTANT(name, value) name = value,
#  define END_VM_CONSTANT_TABLE };
#endif

#ifdef CONST_C_IMPL
#  define BEGIN_VM_CONSTANT_TABLE(table_name, name_fn_name) const _TCHAR *name_fn_name(table_name val) { switch(val) {
#  define VM_CONSTANT(name, value)  case name: return _T("name");
#  define END_VM_CONSTANT_TABLE default: return NULL; } }
#endif

BEGIN_VM_CONSTANT_TABLE(FaslOpcode, fasl_opcode_name)
    VM_CONSTANT(FASL_OP_NIL,                  1  )
    VM_CONSTANT(FASL_OP_TRUE,                 2  )
    VM_CONSTANT(FASL_OP_FALSE,                3  )
    VM_CONSTANT(FASL_OP_CHARACTER,            4  )
    VM_CONSTANT(FASL_OP_LIST,                 8  )
    VM_CONSTANT(FASL_OP_LISTD,                9  )
    VM_CONSTANT(FASL_OP_NOP_1,                10 ) /* #\newline */
    VM_CONSTANT(FASL_OP_NOP_2,                13 ) /* #\cr */
    VM_CONSTANT(FASL_OP_FIX8,                 16 )
    VM_CONSTANT(FASL_OP_FIX16,                17 )
    VM_CONSTANT(FASL_OP_FIX32,                18 )
    VM_CONSTANT(FASL_OP_FIX64,                19 )
    VM_CONSTANT(FASL_OP_FLOAT,                21 )
    VM_CONSTANT(FASL_OP_COMPLEX,              22 )
    VM_CONSTANT(FASL_OP_STRING,               24 )
    VM_CONSTANT(FASL_OP_NOP_3,                26 ) /* Control+Z */
    VM_CONSTANT(FASL_OP_PACKAGE,              28 )
    VM_CONSTANT(FASL_OP_RSYMBOL,              29 )
    VM_CONSTANT(FASL_OP_VECTOR,               30 )
    VM_CONSTANT(FASL_OP_BASE_INSTANCE,        32 )
    VM_CONSTANT(FASL_OP_INSTANCE,             33 )
    VM_CONSTANT(FASL_OP_HASH,                 34 )
    VM_CONSTANT(FASL_OP_COMMENT_1,            35 ) /* #\# */
    VM_CONSTANT(FASL_OP_CLOSURE,              36 )
    VM_CONSTANT(FASL_OP_MACRO,                37 )
    VM_CONSTANT(FASL_OP_SYMBOL,               48 )
    VM_CONSTANT(FASL_OP_SUBR,                 50 )
    VM_CONSTANT(FASL_OP_COMMENT_2,            59 ) /* #\; */
    VM_CONSTANT(FASL_OP_STRUCTURE,            60 )
    VM_CONSTANT(FASL_OP_STRUCTURE_LAYOUT,     61 )
    VM_CONSTANT(FASL_OP_FAST_OP_0,            64 )
    VM_CONSTANT(FASL_OP_FAST_OP_1,            65 )
    VM_CONSTANT(FASL_OP_FAST_OP_2,            66 )
    VM_CONSTANT(FASL_OP_FAST_OP_3,            67 )
    VM_CONSTANT(FASL_OP_INSTANCE_MAP,         96 )
    VM_CONSTANT(FASL_OP_RESET_READER_DEFS,    192)
    VM_CONSTANT(FASL_OP_READER_DEFINITION,    193)
    VM_CONSTANT(FASL_OP_READER_REFERENCE,     194)
    VM_CONSTANT(FASL_OP_LOADER_DEFINEQ,       208)
     /*  209 is the former FASL_OP_LOADER_DEFINE (which invoked the evaluator to determine the definition value.) */
    VM_CONSTANT(FASL_OP_LOADER_DEFINEA0,      210)
    VM_CONSTANT(FASL_OP_LOADER_APPLY0,        216)
    VM_CONSTANT(FASL_OP_BEGIN_LOAD_UNIT,      224)
    VM_CONSTANT(FASL_OP_END_LOAD_UNIT,        225)
    VM_CONSTANT(FASL_OP_EOF,                  253)
    /*  254, 255 reserved for Unicode Byte Order Marker */
END_VM_CONSTANT_TABLE

BEGIN_VM_CONSTANT_TABLE(fast_op_opcode_t, fast_op_name)
    VM_CONSTANT(FOP_LITERAL,                  8  )
    VM_CONSTANT(FOP_GLOBAL_REF,               16 )
    VM_CONSTANT(FOP_GLOBAL_SET,               17 )
    VM_CONSTANT(FOP_LOCAL_REF,                18 )
    VM_CONSTANT(FOP_LOCAL_SET,                19 )
    VM_CONSTANT(FOP_APPLY,                    24 )
    VM_CONSTANT(FOP_IF_TRUE,                  32 )
    VM_CONSTANT(FOP_AND2,                     64 )
    VM_CONSTANT(FOP_OR2,                      65 )
    VM_CONSTANT(FOP_SEQUENCE,                 96 )
    VM_CONSTANT(FOP_CLOSE_ENV,                128)
    VM_CONSTANT(FOP_GET_ENV,                  224)
    VM_CONSTANT(FOP_GLOBAL_DEF,               240)
    VM_CONSTANT(FOP_MARK_STACK,               248)
END_VM_CONSTANT_TABLE

/* *INDENT-ON */
