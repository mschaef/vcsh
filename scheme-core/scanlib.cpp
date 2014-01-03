
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

extern internal_file_t ifile_compiler_run_scf;
extern internal_file_t ifile_scheme_scf;

void scanlib_register_internal_files();

void init1()
{
     /* Load and evaluate the .scm initialization code */

     register_internal_file(&ifile_compiler_run_scf);
     register_internal_file(&ifile_scheme_scf);

     lref_t port = open_c_data_input(&ifile_scheme_scf);

     liifasl_load(lmake_fasl_reader(port));
}

void init(int argc, _TCHAR * argv[], debug_flag_t initial_debug_flags)
{
     init0(argc, argv, initial_debug_flags);
     init1();
}
