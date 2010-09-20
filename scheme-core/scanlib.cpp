
/* scanlib.cpp
 * January 16th, 2006
 * 
 * The interpreter library. This is the VM, plus some compiled scheme
 * source files.
 */

#include "../vm/scan.h"

extern data_block_t scf_compiler_run;
extern data_block_t scf_scheme;

BEGIN_NAMESPACE(scan)

void scanlib_register_internal_files();

void init1()
{
     /* Load and evaluate the .scm initialization code */

     register_internal_file(_T("compiler-run"), true, &scf_compiler_run);
     register_internal_file(_T("s-core"), true, &scf_scheme);

     ENTER_TRY(NIL)
     {
          lifasl_load(open_c_data_input(true, &scf_scheme));
     } ON_ERROR()
     {
          assert(SYMBOLP(interp.sym_errobj));

          scwritef("\nError loading sinit, errobj = ~a\n",
                   DEFAULT_PORT, SYMBOL_VCELL(interp.sym_errobj));
     } LEAVE_TRY();
}

void init(int argc, _TCHAR * argv[], debug_flag_t initial_debug_flags)
{
     init0(argc, argv, initial_debug_flags);
     init1();
}

END_NAMESPACE
