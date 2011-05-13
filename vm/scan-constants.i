
/*
 * scan-constants.i --
 *
 * Constant tables. This file is processed multiple times, with different
 * CONST_* flags set each time. constants.cpp and constants.h both
 * include this file for most of their source text.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */


/* *INDENT-OFF* */

/* Defined when included as a header file to emit enumerations and
 * prototypes. */
#ifdef CONST_C_HEADER
#  define BEGIN_VM_CONSTANT_TABLE(table_name, name_fn_name) enum table_name {
#  define VM_CONSTANT(name, value) name = value,
#  define VM_ANON_CONSTANT(name, value) VM_CONSTANT(name, value)
#  define END_VM_CONSTANT_TABLE(table_name, name_fn_name) }; const _TCHAR *name_fn_name(const table_name val); 
#endif

/* Defined when included as a source file to emit const->string mapping
 * functions. */
#ifdef CONST_C_IMPL
#  define BEGIN_VM_CONSTANT_TABLE(table_name, name_fn_name) const _TCHAR *name_fn_name(const table_name val) { switch(val) {
#  define VM_CONSTANT(name, value)  case name: return _T(#name);
#  define VM_ANON_CONSTANT(name, value)
#  define END_VM_CONSTANT_TABLE(table_name, name_fn_name) default: return NULL; } }
#endif

/* Defined to scheme source included by the scheme-core compile. */
#ifdef CONST_SCHEME
#  define BEGIN_VM_CONSTANT_TABLE(table_name, name_fn_name)
#  define VM_CONSTANT(name, value) (scheme::%define system::name value)
#  define VM_ANON_CONSTANT(name, value) VM_CONSTANT(name, value)
#  define END_VM_CONSTANT_TABLE(table_name, name_fn_name)
#endif

/*
 * There are some subtleties in this assignment of opcode numbers:
 *
 * 1. To allow textual comments to be incorporated in FASL files, the #\;
 *    character is interpreted as a 'skip until #\newline or #\cr' opcode.
 *
 * 2. Since we don't know what our end of line convention is, we treat both
 *    #\newline and #\cr as no-ops. That way, no matter how a textual comment
 *    string ends, we can still read past the line terminator.
 *
 * 3. #\# is treated the same way as #\;. to allow FASL files on Unix
 *    machines to point to an interpreter, like a shell script.
 *
 * 4. Control+Z is a no-op, since it's useful to cause DOS machines to stop
 *   typing a file to the screen.
 *
 * 5. #\nul is also a no-op, since it seems too important to use for
 *    arbitrary reasons.
 */

BEGIN_VM_CONSTANT_TABLE(fasl_opcode_t, fasl_opcode_name)
    /* 0 reserved to allow for Unicode double byte characters, somehow */
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
    /* Former RSYMBOL at 29. */
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
     /*  209 is the former FASL_OP_LOADER_DEFINE (which invoked
      * the evaluator to determine the definition value.) */
    VM_CONSTANT(FASL_OP_LOADER_DEFINEA0,      210)
    VM_CONSTANT(FASL_OP_LOADER_APPLY0,        216)
    VM_CONSTANT(FASL_OP_LOADER_APPLYN,        217)
    VM_CONSTANT(FASL_OP_BEGIN_LOAD_UNIT,      224)
    VM_CONSTANT(FASL_OP_END_LOAD_UNIT,        225)
    VM_CONSTANT(FASL_OP_LOADER_PUSH,          228)
    VM_CONSTANT(FASL_OP_LOADER_DROP,          229)
    VM_CONSTANT(FASL_OP_EOF,                  253)
    /*  254, 255 reserved for Unicode Byte Order Marker */
END_VM_CONSTANT_TABLE(fasl_opcode_t, fasl_opcode_name)

BEGIN_VM_CONSTANT_TABLE(fast_op_opcode_t, fast_op_opcode_name)
    VM_CONSTANT(FOP_LITERAL,                  8  )
    VM_CONSTANT(FOP_GLOBAL_REF,               16 )
    VM_CONSTANT(FOP_GLOBAL_SET,               17 )
    VM_CONSTANT(FOP_LOCAL_REF,                18 )
    VM_CONSTANT(FOP_LOCAL_SET,                19 )

    VM_CONSTANT(FOP_APPLY_GLOBAL,             24 )
    VM_CONSTANT(FOP_APPLY,                    25 )

    VM_CONSTANT(FOP_IF_TRUE,                  32 )
    VM_CONSTANT(FOP_AND2,                     64 )
    VM_CONSTANT(FOP_OR2,                      65 )
    VM_CONSTANT(FOP_SEQUENCE,                 96 )
    VM_CONSTANT(FOP_CLOSURE,                  129)

    VM_CONSTANT(FOP_THROW,                    193)

    VM_CONSTANT(FOP_CATCH,                    195)
    VM_CONSTANT(FOP_WITH_UNWIND_FN,           196)

    VM_CONSTANT(FOP_GET_ENV,                  224)
    VM_CONSTANT(FOP_GLOBAL_DEF,               240)

    VM_CONSTANT(FOP_GET_FSP,                  249)
    VM_CONSTANT(FOP_GET_FRAME,                250)
    VM_CONSTANT(FOP_GET_HFRAMES,              251)
    VM_CONSTANT(FOP_SET_HFRAMES,              252)
    VM_CONSTANT(FOP_GLOBAL_PRESERVE_FRAME,    253)
END_VM_CONSTANT_TABLE(fast_op_opcode_t, fast_op_opcode_name)

BEGIN_VM_CONSTANT_TABLE(trap_type_t, trap_type_name)
    VM_CONSTANT(TRAP_BAD_APPLY                  , 0 )
    VM_CONSTANT(TRAP_DEFINE                     , 1 )
    VM_CONSTANT(TRAP_RESOLVE_FASL_STRUCT_LAYOUT , 2 )
    VM_CONSTANT(TRAP_SIGNAL                     , 3 )
    VM_CONSTANT(TRAP_TIMER_EVENT                , 4 )
    VM_CONSTANT(TRAP_USER_BREAK                 , 5 )
    VM_CONSTANT(TRAP_MSG_NOT_UNDERSTOOD         , 6 )
    VM_CONSTANT(TRAP_PRIMITIVE_INSTANCE         , 7 )
    VM_CONSTANT(TRAP_UNCAUGHT_THROW             , 8 )
    VM_CONSTANT(TRAP_WRONG_TYPE                 , 9 )
    VM_CONSTANT(TRAP_INDEX_OUT_OF_BOUNDS        , 10)
    VM_CONSTANT(TRAP_ARG_OUT_OF_RANGE           , 11)
    VM_CONSTANT(TRAP_UNSUPPORTED                , 12)
    VM_CONSTANT(TRAP_UNIMPLEMENTED              , 13)
    VM_CONSTANT(TRAP_DIVIDE_BY_ZERO             , 14)
    VM_CONSTANT(TRAP_IO_ERROR                   , 15)
    VM_CONSTANT(TRAP_UNBOUND_GLOBAL             , 16)
    VM_CONSTANT(TRAP_FAST_READ_ERROR            , 17)
    VM_CONSTANT(TRAP_RUN0                       , 18)
    VM_CONSTANT(TRAP_AFTER_GC                   , 19)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_ADD        , 20)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_MULTIPLY   , 21)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_NEGATE     , 22)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_SUBTRACT   , 23)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_DIVIDE     , 24)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_MODULO     , 25)
    VM_CONSTANT(TRAP_OVERFLOW_FIXNUM_SHL        , 26)

    VM_ANON_CONSTANT(TRAP_LAST                  , 26)
END_VM_CONSTANT_TABLE(trap_type_t, trap_type_name)

BEGIN_VM_CONSTANT_TABLE(typecode_t, typecode_name)
    VM_CONSTANT(TC_FREE_CELL,                 0  )
    VM_CONSTANT(TC_NIL,                       1  )
    VM_CONSTANT(TC_BOOLEAN,                   2  )
    VM_CONSTANT(TC_CONS,                      3  )
    VM_CONSTANT(TC_FIXNUM,                    4  )
    VM_CONSTANT(TC_FLONUM,                    5  )
    VM_CONSTANT(TC_CHARACTER,                 6  )
    VM_CONSTANT(TC_SYMBOL,                    7  )
    VM_CONSTANT(TC_PACKAGE,                   8  )
    VM_CONSTANT(TC_SUBR,                      9  )
    VM_CONSTANT(TC_CLOSURE,                   10 )
    VM_CONSTANT(TC_MACRO,                     11 )
    VM_CONSTANT(TC_STRING,                    12 )
    VM_CONSTANT(TC_VECTOR,                    13 )
    VM_CONSTANT(TC_STRUCTURE,                 14 )
    VM_CONSTANT(TC_HASH,                      15 )
    VM_CONSTANT(TC_PORT,                      16 )
    VM_CONSTANT(TC_END_OF_FILE,               17 )
    VM_CONSTANT(TC_VALUES_TUPLE,              18 )
    VM_CONSTANT(TC_INSTANCE,                  19 )
    VM_CONSTANT(TC_UNBOUND_MARKER,            20 )
    VM_CONSTANT(TC_GC_TRIP_WIRE,              21 )
    VM_CONSTANT(TC_FAST_OP,                   22 )

    VM_ANON_CONSTANT(LAST_INTERNAL_TYPEC,     22 )
END_VM_CONSTANT_TABLE(typecode_t, typecode_name)

BEGIN_VM_CONSTANT_TABLE(subr_arity_t, subr_arity_name)
    VM_CONSTANT(SUBR_0,                       0  )
    VM_CONSTANT(SUBR_1,                       1  )
    VM_CONSTANT(SUBR_2,                       2  )
    VM_CONSTANT(SUBR_2N,                      3  )  /*  2 or more homogenous argument, */
    VM_CONSTANT(SUBR_3,                       4  )
    VM_CONSTANT(SUBR_4,                       5  )
    VM_CONSTANT(SUBR_ARGC,                    8  )  /*  Arbitrary number of paramaters, passed as array. */
    VM_CONSTANT(SUBR_N,                       9  )  /*  Arbitrary number of paramaters, passed as list. */
END_VM_CONSTANT_TABLE(subr_arity_t, subr_arity_name)

BEGIN_VM_CONSTANT_TABLE(debug_flag_t, debug_flag_name)
    VM_CONSTANT(DF_SHOW_GLOBAL_DEFINES  , 0x00000010)
    VM_CONSTANT(DF_SHOW_LOCAL_DEFINES   , 0x00000020)

    VM_CONSTANT(DF_SHOW_THROWS          , 0x00000100)
    VM_CONSTANT(DF_SHOW_TRAPS           , 0x00000400)
    VM_CONSTANT(DF_SHOW_VMERRORS        , 0x00000800)

    VM_CONSTANT(DF_SHOW_GC              , 0x00002000)
    VM_CONSTANT(DF_SHOW_GC_DETAILS      , 0x00004000)

    VM_CONSTANT(DF_PRINT_SYMBOL_PACKAGES, 0x00010000)
    VM_CONSTANT(DF_PRINT_FOR_DIFF       , 0x00020000)
    VM_CONSTANT(DF_PRINT_CLOSURE_CODE   , 0x00040000)
    VM_CONSTANT(DF_PRINT_ADDRESSES      , 0x00080000)

    VM_CONSTANT(DF_SHOW_LOAD_FORMS      , 0x01000000)
    VM_CONSTANT(DF_FASL_SHOW_OPCODES    , 0x02000000)
    VM_CONSTANT(DF_SHOW_FAST_LOAD_FORMS , 0x04000000)
    VM_CONSTANT(DF_SHOW_FAST_LOAD_UNITS , 0x08000000)

    VM_CONSTANT(DF_DEBUGGER_TO_ODS      , 0x10000000)
    VM_CONSTANT(DF_NO_STARTUP           , 0x20000000)
    VM_CONSTANT(DF_TEMP                 , 0x40000000)

#ifdef WITH_FOPLOG_SUPPORT
    VM_CONSTANT(DF_STARTUP_FOPLOG       , 0x80000000)
#endif

    VM_CONSTANT(DF_NONE                 , 0x00000000)
    VM_CONSTANT(DF_ALL                  , 0xFFFFFFFF)
END_VM_CONSTANT_TABLE(debug_flag_t, debug_flag_name)

#define DF_ALWAYS ::scan::DF_NONE



BEGIN_VM_CONSTANT_TABLE(frame_type_t, frame_type_name)
    VM_CONSTANT(FRAME_SUBR   , 0)
    VM_CONSTANT(FRAME_EVAL   , 1)
    VM_CONSTANT(FRAME_ESCAPE , 3)
    VM_CONSTANT(FRAME_UNWIND , 4)
END_VM_CONSTANT_TABLE(frame_type_t, frame_type_name)


BEGIN_VM_CONSTANT_TABLE(frame_ofs_t, frame_ofs_name)
    VM_ANON_CONSTANT(FOFS_LINK              ,  0)
    VM_ANON_CONSTANT(FOFS_FTYPE             , -1)

    VM_ANON_CONSTANT(FOFS_SUBR_SUBR         , -2)

    VM_ANON_CONSTANT(FOFS_EVAL_FORM_PTR     , -2)
    VM_ANON_CONSTANT(FOFS_EVAL_IFORM        , -3)
    VM_ANON_CONSTANT(FOFS_EVAL_ENV          , -4)

    VM_ANON_CONSTANT(FOFS_UNWIND_AFTER      , -2)

    VM_ANON_CONSTANT(FOFS_ESCAPE_TAG        , -2)
    VM_ANON_CONSTANT(FOFS_ESCAPE_FRAME      , -3)
    VM_ANON_CONSTANT(FOFS_ESCAPE_JMPBUF_PTR , -4)
END_VM_CONSTANT_TABLE(frame_ofs_t, frame_ofs_name)

BEGIN_VM_CONSTANT_TABLE(vmctrl_field_t, vmctrl_field_name)
  VM_CONSTANT(VMCTRL_CURRENT_INPUT_PORT , 0)
  VM_CONSTANT(VMCTRL_CURRENT_OUTPUT_PORT, 1)
  VM_CONSTANT(VMCTRL_CURRENT_ERROR_PORT , 2)
  VM_CONSTANT(VMCTRL_CURRENT_DEBUG_PORT , 3)
  VM_CONSTANT(VMCTRL_PACKAGE_SYSTEM     , 4)
  VM_CONSTANT(VMCTRL_PACKAGE_SCHEME     , 5)
  VM_CONSTANT(VMCTRL_PACKAGE_KEYWORD    , 6)

  VM_ANON_CONSTANT(VMCTRL_LAST          , 6)
END_VM_CONSTANT_TABLE(vmctrl_field_t, vmctrl_field_name)

BEGIN_VM_CONSTANT_TABLE(vminterrupt_t, vminterrupt_name)
  VM_CONSTANT(VMINTR_NONE  , 0x00000000)
  VM_CONSTANT(VMINTR_TIMER , 0x00000001)
  VM_CONSTANT(VMINTR_BREAK , 0x00000002)
END_VM_CONSTANT_TABLE(vminterrupt_t, vminterrupt_name)

BEGIN_VM_CONSTANT_TABLE(sys_retcode_t, sys_retcode_name)
  VM_CONSTANT(SYS_OK              , 0 )      /* No error */
  VM_CONSTANT(SYS_E_NO_FILE       , 1 )      /* No such file, directory, or devic */
  VM_CONSTANT(SYS_E_IO_ERROR      , 2 )      /* I/O error */
  VM_CONSTANT(SYS_E_BAD_ARGUMENT  , 3 )      /* Invalid argument */
  VM_CONSTANT(SYS_E_OUT_OF_MEMORY , 4 )      /* Out of memory */
  VM_CONSTANT(SYS_E_FILE_EXISTS   , 5 )      /* File exists */
  VM_CONSTANT(SYS_E_NAME_TOO_LONG , 6 )      /* File name too long */
  VM_CONSTANT(SYS_E_NOT_PERMITTED , 7 )      /* Not permitted */
  VM_CONSTANT(SYS_E_IS_DIRECTORY  , 8 )      /* Is a directory */
  VM_CONSTANT(SYS_E_NOT_DIRECTORY , 9 )      /* Not a directory */
  VM_CONSTANT(SYS_E_NO_SPACE      , 10)      /* Out of space */
  VM_CONSTANT(SYS_E_BAD_ADDRESS   , 11)      /* Bad address */

  VM_CONSTANT(SYS_E_FAIL          , 0xFFFF)  /* Generic return code. */
END_VM_CONSTANT_TABLE(sys_retcode_t, sys_retcode_name)

#undef BEGIN_VM_CONSTANT_TABLE
#undef VM_CONSTANT
#undef VM_ANON_CONSTANT
#undef END_VM_CONSTANT_TABLE

/* *INDENT-ON */
