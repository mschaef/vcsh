
/*
 * scanlib.cpp --
 *
 * The interpreter library. This is the VM, plus some compiled scheme
 * source files.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "../vm/scan.h"

extern scan::data_block_t scf_compiler_run;
extern scan::data_block_t scf_scheme;

BEGIN_NAMESPACE(scan)

void scanlib_register_internal_files();

void init1()
{
     /* Load and evaluate the .scm initialization code */

     register_internal_file(_T("compiler-run"), true, &scf_compiler_run);
     register_internal_file(_T("s-core"), true, &scf_scheme);

     liifasl_load(open_c_data_input(true, &scf_scheme));
}

void init(int argc, _TCHAR * argv[], debug_flag_t initial_debug_flags)
{
     init0(argc, argv, initial_debug_flags);
     init1();
}

END_NAMESPACE
