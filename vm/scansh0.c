
/*
 * scansh0.cpp --
 *
 * This is the scheme interpreter used during bootstrap. It
 * is as simple as possible: it has no internal scheme code
 * and all it does is load the files named at the command
 * line one after another.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

#include "scan-private.h"

void sigint_handler(int i)
{
  UNREFERENCED(i);

  signal_interrupt(VMINTR_BREAK);

  signal(SIGINT, sigint_handler);
};


int _tmain(int argc, _TCHAR* argv[])
{
  sys_init();
  init0(argc, argv, DF_NONE);

  signal(SIGINT, sigint_handler);

  fprintf(stderr, ";;; scansh0 - %s\n", scan_vm_build_id_string());

  int rc = 0;

  if (DEBUG_FLAG(DF_TEST_VM)) {

       if (execute_vm_tests() > 0)
            rc = 1;

  } else {

       interp.control_fields[VMCTRL_CURRENT_ERROR_PORT] = interp.control_fields[VMCTRL_CURRENT_OUTPUT_PORT];
       interp.control_fields[VMCTRL_CURRENT_DEBUG_PORT] = interp.control_fields[VMCTRL_CURRENT_ERROR_PORT];

       lref_t retval = run();

       if (NUMBERP(retval))
            rc = get_c_long(retval);
  }

  shutdown();

  fprintf(stderr, "\n;;; scansh0 return code = %d\n", rc);

  exit(rc);
}

