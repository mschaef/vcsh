
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

#include "scan.h"

void sigint_handler(int i)
{
  UNREFERENCED(i);

  scan::signal_interrupt(scan::VMINTR_BREAK);

  signal(SIGINT, sigint_handler);
};


int _tmain(int argc, _TCHAR* argv[])
{
  scan::sys_init();
  scan::init0(argc, argv, scan::DF_NONE);

  signal(SIGINT, sigint_handler);

  fprintf(stderr, ";;; scansh0 - %s\n", scan::build_id_string());

  scan::interp.control_fields[scan::VMCTRL_CURRENT_ERROR_PORT] = scan::interp.control_fields[scan::VMCTRL_CURRENT_OUTPUT_PORT];
  scan::interp.control_fields[scan::VMCTRL_CURRENT_DEBUG_PORT] = scan::interp.control_fields[scan::VMCTRL_CURRENT_ERROR_PORT];

  scan::LRef retval = scan::run();

  long return_status = 0;

  if (scan::NUMBERP(retval))
    return_status = scan::get_c_long(retval);

  int rc = (int)(FIXNUMP(retval) ? get_c_fixnum(retval) : 0);

  scan::shutdown();

  fprintf(stderr, "\n;;; scansh0 return code = %d\n", rc);

  exit(rc);
}

