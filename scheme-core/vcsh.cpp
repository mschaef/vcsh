
/*
 * vcsh.cpp --
 *
 * The entry point to the command line scheme interpreter.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <signal.h>

#include "../vm/scan.h"

void sigint_handler(int i)
{
     UNREFERENCED(i);

     scan::signal_interrupt(scan::VMINTR_BREAK);

     signal(SIGINT, sigint_handler);
};

#if !defined(NO_VCSH_STANDARD_LIBRARY)
#  include "vcsh-standard-lib-registration.i"
#endif

int _tmain(int argc, _TCHAR * argv[])
{
     scan::sys_init();
     scan::init(argc, argv, scan::DF_NONE);

#if !defined(NO_VCSH_STANDARD_LIBRARY)
     auto_register_internal_files();
#endif

     signal(SIGINT, sigint_handler);

     scan::lref_t retval = scan::run();

     long return_status = 0;

     if (scan::NUMBERP(retval))
          return_status = scan::get_c_long(retval);

     if (DEBUGGING_BUILD)
          scwritef(_T(";;;; end run, rc=~cd\n"), scan::CURRENT_DEBUG_PORT(), return_status);

     scan::shutdown();

     exit((int) return_status);
}
