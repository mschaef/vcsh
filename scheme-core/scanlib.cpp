/* scanlib.cpp
 * January 16th, 2006
 * 
 * The interpreter library. This is the VM, plus some compiled scheme
 * source files.
 */

#include "../vm/scan.h"

extern unsigned char scmFaslCompilerRun[]; // REVISIT: need to change this to _TCHAR
extern unsigned int scmFaslCompilerRun_bytes;

namespace scan {

  void scanlib_register_internal_files();

  void init1()
  {
    lidefine_global(interp.sym_args, SYMBOL_VCELL(interp.sym_args0), NIL);
    lidefine_global(interp.sym_args0, NIL, NIL);

    /* Load and evaluate the .scm initialization code */

    register_internal_file(_T("fasl-compiler-run"), true, 
                           scmFaslCompilerRun, scmFaslCompilerRun_bytes);

    register_internal_file(_T("s-core"), true, scmSCore, scmSCore_bytes);

    ENTER_TRY(NIL) {
      lifasl_load(open_c_data_input(true, scmSCore, scmSCore_bytes));
    } ON_ERROR() {
      assert(SYMBOLP(interp.sym_errobj));

      scwritef("\nError loading sinit, errobj = ~a\n", 
	       DEFAULT_PORT, 
	       SYMBOL_VCELL(interp.sym_errobj));
    } LEAVE_TRY();
  }

  void init(int argc, _TCHAR *argv[], debug_flag_t initial_debug_flags)
  {
    init0(argc, argv, initial_debug_flags);
    init1();
  }

}
